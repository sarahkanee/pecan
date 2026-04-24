#' Build parcel-year to HLS tile map
#'
#' Identifies which agricultural LandIQ parcels intersect each HLS tile for a
#' requested year range. The script reads LandIQ crop records, filters to
#' agricultural class-subclass combinations using the crop-code lookup table,
#' loads matching parcel geometries in chunks, intersects parcel polygons with
#' pre-built HLS tile extents, and writes parcel-to-tile and tile-to-parcel-count
#' outputs.
#'
#' This script should be run after `build_hls_tile_extent.R`, which creates
#' `hls_tile_extent.rds`.
#'
#' @section Usage:
#' ```sh
#' Rscript build_hls_parcel_tile_map.R [year_min] [year_max] [overwrite]
#' ```
#'
#' @section Arguments:
#' \describe{
#'   \item{year_min}{Optional integer. First year to include. Defaults to `2016`.}
#'   \item{year_max}{Optional integer. Last year to include. Defaults to `2024`.}
#'   \item{overwrite}{Optional logical-like value. If `"overwrite"`, `"true"`,
#'   `"t"`, `"1"`, `"yes"`, or `"y"`, existing outputs are replaced.}
#' }
#'
#' @section Environment variables:
#' \describe{
#'   \item{CCMMF_LANDIQ_V4}{Base directory for LandIQ v4.1 harmonized inputs.
#'   Defaults to `/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1`.}
#'   \item{CCMMF_MANAGEMENT}{Management directory containing lookup tables,
#'   tile extents, and script outputs. Defaults to
#'   `/projectnb/dietzelab/ccmmf/management`.}
#' }
#'
#' @section Inputs:
#' \describe{
#'   \item{parcels-consolidated.gpkg}{Parcel geometry layer from LandIQ v4.1.}
#'   \item{crops_all_years.parq}{Parcel-year crop records from LandIQ v4.1.}
#'   \item{LandIQ_cropCode_lookup_table.csv}{Crop-code lookup table used to keep
#'   agricultural `CLASS` and `SUBCLASS` combinations.}
#'   \item{hls_tile_extent.rds}{Pre-built HLS tile extent object created by
#'   `build_hls_tile_extent.R`.}
#' }
#'
#' @section Outputs:
#' \describe{
#'   \item{hls_parcel_tile_map_v4.1_years=<min>-<max>.rds}{RDS file containing
#'   parcel-year rows with intersecting HLS tile IDs and tile counts.}
#'   \item{hls_tile_parcel_counts_v4.1_years=<min>-<max>.csv}{CSV file giving
#'   the number of parcel-year records per HLS tile and year.}
#'   \item{hls_parcel_tile_map_removed_v4.1_years=<min>-<max>.csv}{Optional CSV
#'   written only when parcel-years are dropped because of invalid or corrupt
#'   geometries.}
#' }
#'
#' @section Output columns:
#' The parcel-tile RDS contains:
#' \describe{
#'   \item{parcel_id}{LandIQ parcel identifier.}
#'   \item{year}{Crop year.}
#'   \item{tileIDs}{Comma-separated HLS tile IDs intersecting the parcel.}
#'   \item{n_tiles}{Number of intersecting HLS tiles.}
#' }
#'
#' The tile-count CSV contains:
#' \describe{
#'   \item{tile_id}{HLS tile identifier.}
#'   \item{year}{Crop year.}
#'   \item{n_parcels}{Number of parcel-year records intersecting the tile.}
#' }
#'
#' @details
#' Agricultural filtering is done by joining `CLASS` and `SUBCLASS` against the
#' LandIQ crop-code lookup table. This preserves subclass-level differences in
#' crop grouping and PFT assignment.
#'
#' Parcel geometries are read in chunks to avoid very large SQL `IN` queries.
#' Empty, invalid, or corrupt geometries are dropped and optionally logged. If
#' bulk geometry checks, reprojection, or spatial intersection fail, the script
#' falls back to row-by-row checks to keep usable parcels.
#'
#' A parcel is assigned to every HLS tile polygon it intersects; any overlap
#' counts.
#'
#' @examples
#' \dontrun{
#' # Default years, no overwrite
#' Rscript build_hls_parcel_tile_map.R
#'
#' # Specific year range
#' Rscript build_hls_parcel_tile_map.R 2018 2023
#'
#' # Force overwrite
#' Rscript build_hls_parcel_tile_map.R 2018 2023 overwrite
#' }
#'
#' @seealso build_hls_tile_extent.R
#'
#' @keywords internal
NULL

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(arrow)
  library(dplyr)
})
sf::sf_use_s2(FALSE)

# --- Configuration ---
path_landiq_v4     <- Sys.getenv("CCMMF_LANDIQ_V4", "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1")
path_management    <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
path_parcels      <- file.path(path_landiq_v4, "parcels-consolidated.gpkg")
path_crops_parq   <- file.path(path_landiq_v4, "crops_all_years.parq")
path_cropcode_lookup <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
path_tiles        <- file.path(path_management, "hls_tile_extent.rds")
path_out          <- path_management

# --- Parse args ---
args    <- commandArgs(trailingOnly = TRUE)
year_min <- if (length(args) >= 1) as.integer(args[1]) else 2016L
year_max <- if (length(args) >= 2) as.integer(args[2]) else 2024L
overwrite <- length(args) >= 3 && tolower(args[3]) %in% c("overwrite", "true", "t", "1", "yes", "y")

out_file <- file.path(path_out, sprintf("hls_parcel_tile_map_v4.1_years=%d-%d.rds", year_min, year_max))
if (file.exists(out_file) && !overwrite) quit(save = "no", status = 0)
if (!file.exists(path_tiles)) {
  stop("Tile extent not found. Run: Rscript scripts/hls/build_hls_tile_extent.R")
}

tile_prep   <- readRDS(path_tiles)
tile_extent <- tile_prep$tile_extent_sf
used_crs   <- tile_prep$used_crs

# --- Parcel-year rows: agricultural only via (CLASS, SUBCLASS) join ---
# Join on CLASS+SUBCLASS so subclass-level PFT differences (e.g. T19 vs T28 woody) are correct.
lookup         <- fread(path_cropcode_lookup)
ag_pairs       <- unique(lookup[is_agricultural == TRUE,
  .(CLASS = trimws(CLASS), SUBCLASS = as.character(SUBCLASS))])
ag_classes_filter <- unique(ag_pairs$CLASS)

parcel_year_raw <- arrow::open_dataset(path_crops_parq) |>
  dplyr::filter(year >= year_min, year <= year_max, CLASS %in% ag_classes_filter) |>
  dplyr::select(parcel_id, year, CLASS, SUBCLASS) |>
  dplyr::collect() |>
  as.data.table()
parcel_year_raw[, CLASS    := trimws(as.character(CLASS))]
parcel_year_raw[, SUBCLASS := as.character(SUBCLASS)]
parcel_year <- merge(parcel_year_raw, ag_pairs, by = c("CLASS", "SUBCLASS"))[
  , .(parcel_id = as.character(parcel_id), year = as.integer(year))
] |> unique()
parcel_year[, parcel_id := as.character(parcel_id)]
parcel_year[, year := as.integer(year)]
message("Parcel-year rows (agricultural, ", year_min, "-", year_max, "): ", nrow(parcel_year))

# --- Load parcel geometry in chunks (avoid huge SQL IN) ---
ids   <- unique(parcel_year$parcel_id)
layer <- st_layers(path_parcels)$name[1]
chunks <- split(ids, ceiling(seq_along(ids) / 5000L))
geom_chunks <- lapply(chunks, function(x) {
  esc <- gsub("'", "''", x, fixed = TRUE)
  q   <- sprintf('SELECT * FROM "%s" WHERE parcel_id IN (%s)', layer, paste0("'", esc, "'", collapse = ","))
  st_read(path_parcels, query = q, quiet = TRUE)
})
parcels <- do.call(rbind, geom_chunks)
parcels$parcel_id <- as.character(parcels$parcel_id)

# --- QC: drop invalid/empty geometries (corrupt WKB can cause OGR errors) ---
valid <- tryCatch(
  !sf::st_is_empty(sf::st_geometry(parcels)),
  error = function(e) {
    message("Bulk geometry check failed; checking row-by-row for corrupt geometries.")
    vapply(seq_len(nrow(parcels)), function(i) {
      tryCatch(!sf::st_is_empty(sf::st_geometry(parcels)[i]), error = function(e) FALSE)
    }, logical(1))
  }
)
removed_log <- if (any(!valid)) {
  parcel_year[parcel_id %in% parcels$parcel_id[!valid], .(parcel_id, year)]
} else {
  data.table(parcel_id = character(), year = integer())
}
parcels <- parcels[valid, ]

# --- Reproject to tile CRS; fallback to row-by-row if bulk transform fails ---
parcels_tr <- tryCatch(sf::st_transform(parcels, used_crs), error = function(e) NULL)
if (is.null(parcels_tr)) {
  message("Bulk st_transform failed; checking row-by-row.")
  chunk_size <- 5000L
  n <- nrow(parcels)
  good <- logical(n)
  for (start in seq(1L, n, by = chunk_size)) {
    end <- min(start + chunk_size - 1L, n)
    chk <- tryCatch(sf::st_transform(parcels[start:end, ], used_crs), error = function(e) NULL)
    if (!is.null(chk)) {
      good[start:end] <- TRUE
    } else {
      for (i in start:end) {
        good[i] <- tryCatch({
          sf::st_transform(parcels[i, ], used_crs)
          TRUE
        }, error = function(e) FALSE)
      }
    }
  }
  drop_ids <- parcels$parcel_id[!good]
  if (length(drop_ids) > 0) {
    removed_log <- rbind(removed_log, parcel_year[parcel_id %in% drop_ids, .(parcel_id, year)])
  }
  parcels <- parcels[good, ]
  parcels <- sf::st_transform(parcels, used_crs)
} else {
  parcels <- parcels_tr
}

# --- Spatial join: parcel polygon intersects tile polygon (any overlap counts) ---
hits <- tryCatch(sf::st_intersects(parcels, tile_extent), error = function(e) NULL)
if (is.null(hits)) {
  message("Bulk st_intersects failed; checking row-by-row.")
  n <- nrow(parcels)
  good <- logical(n)
  for (i in seq_len(n)) {
    good[i] <- tryCatch({
      hi <- sf::st_intersects(parcels[i, ], tile_extent)
      length(hi[[1]]) >= 0
      TRUE
    }, error = function(e) FALSE)
  }
  drop_ids <- parcels$parcel_id[!good]
  if (length(drop_ids) > 0) {
    removed_log <- rbind(removed_log, parcel_year[parcel_id %in% drop_ids, .(parcel_id, year)])
  }
  parcels <- parcels[good, ]
  hits <- sf::st_intersects(parcels, tile_extent)
}
keep <- lengths(hits) > 0
parcels <- parcels[keep, ]
hits <- hits[keep]

if (nrow(removed_log) > 0) {
  removed_log <- unique(removed_log)
  removed_log_file <- file.path(path_out, sprintf("hls_parcel_tile_map_removed_v4.1_years=%d-%d.csv", year_min, year_max))
  dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
  fwrite(removed_log, removed_log_file)
  message("Dropped ", nrow(removed_log), " parcel-years with invalid geometry; log: ", removed_log_file)
}

# --- Build parcel -> tiles table and join to parcel_year ---
tile_by_parcel <- data.table(
  parcel_id = parcels$parcel_id,
  tileIDs   = vapply(hits, function(i) paste(tile_extent$tile_id[i], collapse = ","), character(1)),
  n_tiles   = lengths(hits)
)
setkey(tile_by_parcel, parcel_id)
setkey(parcel_year, parcel_id)
out <- tile_by_parcel[parcel_year, nomatch = 0][, .(parcel_id, year, tileIDs, n_tiles)]

# --- Tile -> parcel counts (for scheduling) ---
tile_long    <- out[, .(tile_id = unlist(strsplit(tileIDs, ",", fixed = TRUE))), by = .(parcel_id, year)]
tile_counts  <- tile_long[, .(n_parcels = .N), by = .(tile_id, year)]
setorder(tile_counts, tile_id, year)

# --- Write ---
dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
saveRDS(out, out_file)
tile_counts_file <- file.path(path_out, sprintf("hls_tile_parcel_counts_v4.1_years=%d-%d.csv", year_min, year_max))
fwrite(tile_counts, tile_counts_file)
message("Wrote tile->parcel counts: ", tile_counts_file)
