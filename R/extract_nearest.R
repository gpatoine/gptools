



#' Extract from nearest available pixel
#'
#' takes Raster* and assigns proper method
#'
#'
#' @param x Raster* object
#' @param y points represented by a sf object
#' @param max_range numeric maximum distance in meters. Default is 5 x res
#'
#' @return A vector for RasterLayer objects, and a matrix for RasterStack or RasterBrick objects.
#' @export
extract_nearest <- function(x, y, max_range = NULL) {

  if (is.null(max_range)) {
    max_range <- mean(res(x)) * 5

  }

  if (class(x) == "RasterLayer") {
    extract_nearest_layer(x, y, max_range)

  } else if (class(x) %in% c("RasterBrick", "RasterStack")) {

    xus <- unstack(x)
    vals <- purrr::map_dfc(xus, ~ extract_nearest_layer(.xus, y, max_range))

    return(vals)

  } else {
    stop("Not a Raster* object")
  }

}




#' Extract nearest for RasterLayer
#'
#' Takes 1 layer, group same locations together
#'
#' @param x RasterLayer
#' @param y points represented by a sf object
#' @param max_range numeric maximum distance in meters
#'
#' @return vector of values
#' @export
#'
#' @examples
extract_nearest_layer <- function(x, y, max_range = NULL) {

  # group same locations


  # try to extract
  extract1 <- extract(r, points, cellnumbers = TRUE) %>% as_tibble

  points <- points %>% mutate(cells = extract1$cells,
                              extr_val = extract1[[2]],
                              dist = 0)

  # subset NAs
  missing_values <- points %>% filter(is.na(extr_val))

  extract_miss <- map_dfr(seq_len(nrow(missing_values)),
                          ~ nearest_value(r, missing_values[.x,], max_range))

  missing_values <- missing_values %>% bind_cols(extract_miss)


  # extract_nearest_value



  # # merge with original dataset
  # stgeo <- st_geometry(points)
  # points <- points %>% st_drop_geometry()
  #
  # points[is.na(points$extr_val), c("extr_val", "dist")] <-
  #   missing_values[c("sampl_val", "dist_cell")] %>% st_drop_geometry()
  #
  # points %>% select(-c(cells)) %>% st_set_geometry(stgeo)
  #


}





#' Extract nearest value for single point
#'
#' For each NA, get cells in buffer, subset raster, calc distance, pick smallest
#'
#' @param x RasterLayer
#' @param point single sf object created by subsetting a single row from sf df.
#' @param max_range
#'
#' @return data.frame with extract value and distance from cell
#' @export
#'
#' @examples
extract_nearest_value <- function(x, point, max_range = NULL) {
  # point <- missing_values[1,]

  # buffer_values
  # alternative could be to fix points around (probably faster), but
  # extract does fancy stuff with buffer distance if lonlat
  # could also just count cells on each side, would then use a cell_dist argument

  buff_vals0 <- raster::extract(x, point, buffer = max_range,
                                cellnumbers = TRUE) %>% .[[1]]

  if (class(buff_vals0) == "numeric") {
    buff_vals <- data.frame(as.list(buff_vals0))
  } else {
    buff_vals <- as.data.frame(buff_vals0)
  }

  # subset raster
  r_sub <- rasterFromCells(r, buff_vals$cell)
  #plot(r_sub)

  r_sub[] <- r[values(r_sub)]
  #plot(r_sub)

  # distance matrix

  point <- st_transform(point, crs(r_sub))

  dist <- replace(distanceFromPoints(r_sub, point), is.na(r_sub), NA)
  dist[dist > max_range] <- NA
  #plot(dist)
  #plot(point, add = T)

  xyCell <- xyFromCell(dist, which.min(dist))

  data.frame(sampl_val = raster::extract(r_sub, xyCell),
             dist_cell = minValue(dist))

}



# examples ----------------------------------------------------------------

# tcomp <- readRDS(here("data_mod/2-4_tcomp_extracted.rds"))
# points <- tcomp
#
# rast_path <- file.path(datapath, "SoilGrids/phh2o_0-5cm_mean.tif")
# r <- raster(rast_path)
#
# # example
# df <- extract_nearest(r, points, max_range = 2000)


#extract_nearest_value(r, point = miss1[11,], 2000)

# from globcmic
# library(raster)
# library(tidyverse)
# library(here)
# library(sf)
#
# cmic0 <- readRDS(here("data_mod/02-xu_cmic_v4.rds"))
# cmic2 <- cmic0 %>% filter(!is.na(latitude) & !is.na(longitude)) %>%
#   filter(between(latitude, 41.53, 59.64),
#          between(longitude, -1.34, 23.40)) %>%
#   st_as_sf(coords = c("longitude", "latitude"), remove = FALSE,
#            crs = CRS("+init=epsg:4326")) %>%
#   sample_n(150)
#
#   # rpath <- file.path(datapath, "sand05-crop_EU_test.tif") #file.exists(rpath)
# rpath <- "~/Desktop/sand05-crop_EU_test.tif"
# file.exists(rpath)
#
# ras <- raster(rpath)
# cras <- crs(ras)
#
# extr <- raster::extract(ras, cmic2)
