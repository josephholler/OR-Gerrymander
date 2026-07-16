library(sf)
library(dplyr)

#' Area-weighted reaggregation
#'
#' Reaggregate numeric fields from a source `sf` to a target `sf` using
#' area-weighting. The function computes source feature areas internally,
#' intersects source and target, weights each source field by the proportion
#' of the source area that falls inside each target feature, and returns the
#' target `sf` with new `aw_<field>` columns containing the aggregated estimates.
#'
#' @param source sf data frame (e.g. block groups or precincts)
#' @param target sf data frame (e.g. districts)
#' @param fields character vector of field names in `source` to reaggregate
#' @return `sf` target with added `aw_<field>` columns
area_weighted_reaggregate <- function(source, target, fields){
  if(!inherits(source, "sf") || !inherits(target, "sf")){
    stop("Both 'source' and 'target' must be sf objects")
  }

  # align CRS: transform source to target CRS
  if (is.na(st_crs(target))) stop("target has no CRS")
  source <- st_transform(source, st_crs(target))

  # ensure valid geometries
  source <- st_make_valid(source)
  target <- st_make_valid(target)

  # compute source areas (internal field to avoid collisions)
  source$.sarea__awr <- st_area(source)

  # add temporary id to target for grouping and later join
  target$.tid__awr <- seq_len(nrow(target))

  # intersection
  inter <- tryCatch(
    st_intersection(source, target),
    error = function(e) stop("st_intersection failed: ", e$message)
  )

  if(nrow(inter) == 0){
    warning("No intersections found. Returning target with NA aw_ fields")
    for(f in fields){
      target[[paste0("aw_", f)]] <- NA_real_
    }
    target$.tid__awr <- NULL
    source$.sarea__awr <- NULL
    return(target)
  }

  # compute intersected area and ratio relative to source area
  inter$.iarea__awr <- st_area(inter)
  inter$ratio__awr <- as.numeric(inter$.iarea__awr / inter$.sarea__awr)

  # create area-weighted fields
  for(f in fields){
    if(!f %in% names(inter)) stop(paste0("Field '", f, "' not found in source (after intersection)."))
    inter[[paste0("aw_", f)]] <- as.numeric(inter[[f]]) * inter$ratio__awr
  }

  # summarize by target id
  summed <- inter |>
    st_drop_geometry() |>
    group_by(.tid__awr) |>
    summarize(across(starts_with("aw_"), ~sum(.x, na.rm = TRUE)), .groups = "drop")

  # join back to target
  target <- left_join(target, summed, by = ".tid__awr")

  # clean temporary columns
  target$.tid__awr <- NULL
  source$.sarea__awr <- NULL

  return(target)
}
