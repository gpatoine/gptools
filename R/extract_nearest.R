#created: 2020-10-15
#updated: 2020-11-05
# author: Guillaume Patoine <guillaume.patoine@idiv.de>
#purpose: extract value from nearest raster cell

#' Extract from nearest available pixel
#'
#' takes Raster* and assigns proper method
#'
#'
#' @param x Raster* object
#' @param y points represented by a sf object
#' @param max_range numeric maximum distance in meters. Default is 5 x res
#' @param .as_na vector of values to be ignored (considered as NA) in the Raster*
#'
#' @return A vector for RasterLayer objects, and a matrix for RasterStack or RasterBrick objects.
#' @export
extract_nearest <- function(x, y, max_range = NULL, .as_na = NULL) {

  y0 <- y
  y <- y %>% mutate(uni_enl = row_number()) %>% dplyr::select(uni_enl)

  # group same locations
  uni_loc <- y %>% dplyr::distinct(geometry, .keep_all = TRUE)

  y <- y %>%
    mutate(match_loc = purrr::map_dbl(seq_len(nrow(y)),
                                      ~ uni_loc$uni_enl[st_equals(uni_loc, y[.x,], sparse = F)])) # st_equals much faster then ==

  if (is.null(max_range)) {
    max_range <- mean(res(x)) * 5 * (if (isLonLat(x)) 111000 else 1) # in meters
  }

  # dispatch depending n layers
  if (inherits(x, "RasterLayer")) {
    vals <- extract_nearest_layer(x, y = uni_loc, max_range, .as_na)
    vals <- data.frame(vals)

  } else if (inherits(x, c("RasterBrick", "RasterStack"))) {
    xus <- unstack(x)
    vals <- purrr::map_dfc(xus, ~ {cat("\n"); print(.x);
      extract_nearest_layer(.x, y = uni_loc, max_range, .as_na)})

  } else {
    stop("Not a Raster* object")
  }

  # TODO check is warning New name ..1 is from here, FIXME
  uni_vals <- vals %>% set_names(names(x)) %>% bind_cols(st_drop_geometry(uni_loc), .) %>%
    rename(match_loc = uni_enl)

  y <- left_join(st_drop_geometry(y), uni_vals, by = "match_loc") %>% dplyr::select(-c(uni_enl, match_loc))

  # return vector or df
  if (ncol(y) == 1) y[[1]] else y

}




#' Extract nearest for RasterLayer
#'
#' Takes 1 layer, group same locations together
#'
#' @param x RasterLayer
#' @param y points represented by a sf object
#' @param max_range numeric maximum distance in meters
#' @param .as_na vector of values to be ignored (considered as NA) in the Raster*
#'
#' @return vector of values
#' @export
extract_nearest_layer <- function(x, y, max_range = NULL, .as_na = NULL) {
  # x = xus[[1]]
  # y = uni_loc

  # todebug: plot locations
  # world <- rnaturalearth::ne_countries(returnclass = "sf")
  # ggplot()+
  #   geom_sf(data = world) +
  #   geom_sf(data = y, col = "red")

  y <- st_transform(y, crs(x))

  # try to extract
  ext1 <- raster::extract(x, y, cellnumbers = TRUE) %>% as_tibble

  y <- y %>% mutate(cells = ext1$cells,
                    value = ext1[[2]])

  # remove as_na values
  if (!is.null(.as_na)) {
    y$value[y$value %in% .as_na] <- NA
  }

  # subset NAs
  miss_y <- y %>% filter(is.na(value))

  # todebug: plot locations
  # world <- rnaturalearth::ne_countries(returnclass = "sf")
  # world_proj <- st_transform(world, crs(x))
  # ggplot()+
  #   geom_sf(data = world_proj) +
  #   geom_sf(data = miss_y, col = "red")

  if (nrow(miss_y) == 0) {
    ym <- ext1[[2]]

  } else {
    # extract_nearest_value point by point
    message("Looking for ", nrow(miss_y), " missing values\n")

    ext_miss <- map_dfr(seq_len(nrow(miss_y)),
                        ~ {cat(.x, "- "); extract_nearest_value(x, miss_y[.x,], max_range, .as_na)})

    miss_y <- miss_y %>% bind_cols(ext_miss) %>% st_drop_geometry %>%
      dplyr::select(uni_enl, value = near_val)

    # merge with original dataset
    ym <- y %>% filter(!is.na(value)) %>% st_drop_geometry %>%
      dplyr::select(uni_enl, value) %>% bind_rows(miss_y) %>% arrange(uni_enl) %>% pull(value)

  }

  # Or should it return a df with names(x)?
  ym

}



#' Extract nearest value for single point
#'
#' For each NA, get cells in buffer, subset raster, calc distance, pick smallest
#'
#' @param x RasterLayer
#' @param point single sf object created by subsetting a single row from sf df.
#' @param max_range numeric maximum distance in meters
#' @param .as_na vector of values to be ignored (considered as NA) in the Raster*
#'
#' @return data.frame with extract value and distance from cell
#' @export
extract_nearest_value <- function(x, point, max_range = NULL, .as_na = NULL) {
  # point <- miss_y[2,]

  # buffer_values
  # alternative could be to fix points around (probably faster), but
  # extract does fancy stuff with buffer distance if lonlat
  # could also just count cells on each side, would then use a cell_dist argument

  buff_vals0 <- raster::extract(x, point, buffer = max_range,
                                cellnumbers = TRUE, small = TRUE) %>% .[[1]]

  buff_vals <-
    if(length(buff_vals0) == 1 && is.na(buff_vals0)) {
      data.frame(near_val = NA,
                 dist_cell = NA)
    } else {
      if (inherits(buff_vals0, "numeric")) {
        data.frame(as.list(buff_vals0))
      } else {
        as.data.frame(buff_vals0)
      }
    }

  # subset raster
  x_sub <- rasterFromCells(x, buff_vals$cell)
  x_sub[] <- x[values(x_sub)]

  # remove .as_na values
  if (!is.null(.as_na)) {
    na_df <- data.frame(id = .as_na, value = NA)
    x_sub <- subs(x_sub, na_df, subsWithNA=FALSE)
  }
  # plot(x_sub)

  # distance matrix
  dist <- replace(distanceFromPoints(x_sub, point), is.na(x_sub), NA)
  dist[dist > max_range] <- NA
  #plot(dist)
  #plot(point, add = T)

  # if many mins, pick one
  min_cells <- raster::which.min(dist)
  min_cell <- min_cells[sample.int(length(min_cells))]

  # just for a nice plot
  # x_subp <- x_sub
  # x_subp[is.na(dist)] <- NA
  # plot(x_subp)
  # plot(point, add = T)
  # # points(xyFromCell(x_subp, min_cell), col = "red")
  # posp <- xyFromCell(x_subp, min_cell)
  # polygon(x = c(rep(posp[1]-res(x_subp)[1]/2, 2), rep(posp[1]+res(x_subp)[1]/2, 2)),
  #         y = c(posp[2]-res(x_subp)[2]/2, rep(posp[2]+res(x_subp)[2]/2, 2), posp[2]-res(x_subp)[2]/2), border = "red", )


  no_min <- is.na(min_cell)

  near_val <- if (no_min) NA else {
    raster::extract(x_sub, xyFromCell(dist, min_cell))
  }

  data.frame(near_val = near_val,
             dist_cell = minValue(dist))
}



# examples ----------------------------------------------------------------
#
# library(raster)
# library(tidyverse)
# library(here)
# library(sf)
# library(gptools)
#
#
# # Example 1
# tcomp <- readRDS(here("data_mod/2-4_tcomp_extracted.rds"))
# rast_path <- file.path(datapath, "SoilGrids/phh2o_0-5cm_mean.tif")
# r <- raster(rast_path)
# df <- extract_nearest(r, points, max_range = 2000)
#
#
# # Example 2
# set.seed(101)
# # y
# cmic0 <- readRDS(here("../globcmic/data_mod/02-xu_cmic_v4.rds"))
# cmic2 <- cmic0 %>% filter(!is.na(latitude) & !is.na(longitude)) %>%
#   filter(between(latitude, 41.53, 59.64),
#          between(longitude, -1.34, 23.40)) %>%
#   st_as_sf(coords = c("longitude", "latitude"), remove = FALSE,
#            crs = CRS("+init=epsg:4326")) %>%
#   sample_n(150)
#
# # x, one layer
# rpath <- file.path("~/data/eie-group-share/GlobCmic_GP/sand05-crop_EU_test.tif")
# # rpath <- "~/Desktop/sand05-crop_EU_test.tif"
# file.exists(rpath)
#
# ras <- raster(rpath)
#
# extr <- raster::extract(ras, cmic2)
# extr2 <- extract_nearest(x = ras, y = cmic2)
#
# cmic3 <- cmic2 %>% mutate(new_val = extract_nearest(ras, .))
#
# # missing values
# mna <- which(is.na(cmic3$new_val))
# extr3 <- extract_nearest(x = ras, y = cmic2[mna,])
# point <- miss_y[2,]
#
#
# # using stack
# st <- stack("~/data/eie-group-share/GlobCmic_GP/sandstack-crop_EU_test.tif")
# ext_st <- extract_nearest(x = st, y = cmic2 %>% sample_frac(0.25))
#
# ext_st0 <- raster::extract(x = st, y = cmic2, df = TRUE)



#' Extract buffer, return 1 value
#'
#' @param points sf point object
#' @param rast raster* object
#' @param dist distance
#' @param coverage defaults to 0.4
#' @param fct median
#'
#' @return extracted and merged values
#' @export
gp_ext_buff <- function(points, rast, dist, coverage = 0.4, fct = median) {

  df_buff <- points %>% sf::st_buffer(dist)

  ext <- exactextractr::exact_extract(rast, df_buff)

  ext <- purrr::map(ext, ~ dplyr::filter(.x, coverage_fraction > 0.4))
  ext <- ext %>% purrr::map(~ .x %>% dplyr::select(-coverage_fraction))

  print(purrr::map_dbl(ext, nrow) %>% spl + ggplot2::ylim(0,NA))

  ext_medians <- purrr::map_dfr(ext, ~ purrr::map_dbl(.x, ~ fct(.x, na.rm = TRUE)))
  ext_medians %>% purrr::set_names(names(rast))

}



# Implement for terra -----------------------------------------------------

#' Extract from nearest available pixel
#'
#' takes Raster* and assigns proper method
#'
#'
#' @param x Raster* object
#' @param y points represented by a sf object
#' @param max_range numeric maximum distance in meters. Default is 5 x res
#' @param .as_na vector of values to be ignored (considered as NA) in the Raster*
#'
#' @return A vector for RasterLayer objects, and a matrix for RasterStack or RasterBrick objects.
#' @export
extract_nearest_terra <- function(x, y, max_range = NULL, .as_na = NULL) {

  stopifnot(inherits(x, "SpatRaster"),
            inherits(y, "sf"))

  y0 <- y
  y <- y %>% dplyr::mutate(uni_enl = dplyr::row_number()) %>% dplyr::select(uni_enl)

  # group same locations
  uni_loc <- y %>% dplyr::distinct(geometry, .keep_all = TRUE)

  y <- y %>%
    mutate(match_loc = purrr::map_dbl(seq_len(nrow(y)),
                                      ~ uni_loc$uni_enl[sf::st_equals(uni_loc, y[.x,], sparse = F)])) # st_equals much faster then ==

  if (is.null(max_range)) {
    max_range <- mean(res(x)) * 5 * (if (terra::is.lonlat(x)) 111000 else 1) # in meters
  }

  # dispatch depending n layers
  if (terra::nlyr(x) == 1) {
    vals <- extract_nearest_layer_terra(x, y = uni_loc, max_range, .as_na)
    vals <- data.frame(vals)

  } else if (terra::nlyr(x) > 1) {
    xus <- as.list(x)
    vals <- purrr::map_dfc(xus, ~ {cat("\n"); print(.x);
      extract_nearest_layer_terra(.x, y = uni_loc, max_range, .as_na)})

  } else {
    stop("Wrong format")
  }

  # TODO check is warning New name ..1 is from here, FIXME
  uni_vals <- vals %>% dplyr::bind_cols(sf::st_drop_geometry(uni_loc), .) %>%
    dplyr::rename(match_loc = uni_enl)

  y <- dplyr::left_join(sf::st_drop_geometry(y), uni_vals, by = "match_loc") %>% dplyr::select(-c(uni_enl, match_loc))

  # return vector or df
  if (ncol(y) == 1) y[[1]] else y

}




#' Extract nearest for RasterLayer
#'
#' Takes 1 layer, group same locations together
#'
#' @param x RasterLayer
#' @param y points represented by a sf object
#' @param max_range numeric maximum distance in meters
#' @param .as_na vector of values to be ignored (considered as NA) in the Raster*
#'
#' @return vector of values
#' @export
extract_nearest_layer_terra <- function(x, y, max_range = NULL, .as_na = NULL) {
  # x = xus[[10]]
  # y = uni_loc

  # todebug: plot locations
  # world <- rnaturalearth::ne_countries(returnclass = "sf")
  # ggplot()+
  #   geom_sf(data = world) +
  #   geom_sf(data = y, col = "red")

  y <- sf::st_transform(y, crs(x))

  # try to extract
  ext1 <- terra::extract(x, y, cells = TRUE) %>% as_tibble

  y <- y %>% mutate(cells = ext1$cell,
                    value = ext1[[2]])

  # remove as_na values
  if (!is.null(.as_na)) {
    y$value[y$value %in% .as_na] <- NA
  }

  # for testing
  # y$value[c(1,5,20)] <- NA

  # subset NAs
  miss_y <- y %>% dplyr::filter(is.na(value))

  # todebug: plot locations
  # world <- rnaturalearth::ne_countries(returnclass = "sf")
  # world_proj <- st_transform(world, crs(x))
  # ggplot()+
  #   geom_sf(data = world_proj) +
  #   geom_sf(data = miss_y, col = "red")

  if (nrow(miss_y) == 0) {
    ym <- ext1 %>% dplyr::select(2)

  } else {

    # extract_nearest_value point by point
    message("Looking for ", nrow(miss_y), " missing values\n")

    ext_miss <- purrr::map_dfr(seq_len(nrow(miss_y)),
                               ~ {cat(.x, "- "); extract_nearest_value_terra(x, miss_y[.x,], max_range, .as_na)})

    miss_y <- miss_y %>% dplyr::bind_cols(ext_miss) %>% sf::st_drop_geometry() %>%
      dplyr::select(uni_enl, value = near_val)

    # merge with original dataset
    ym <- y %>% dplyr::filter(!is.na(value)) %>% sf::st_drop_geometry() %>%
      dplyr::select(uni_enl, value) %>% dplyr::bind_rows(miss_y) %>% dplyr::arrange(uni_enl) %>% dplyr::select(value) %>%
      dplyr::rename_with(.fn = function(n) {names(x)}, .cols = value)

  }

  # Or should it return a df with names(x)?
  ym

}



#' Extract nearest value for single point
#'
#' For each NA, get cells in buffer, subset raster, calc distance, pick smallest
#'
#' @param x RasterLayer
#' @param point single sf object created by subsetting a single row from sf df.
#' @param max_range numeric maximum distance in meters
#' @param .as_na vector of values to be ignored (considered as NA) in the Raster*
#'
#' @return data.frame with extract value and distance from cell
#' @export
extract_nearest_value_terra <- function(x, point, max_range = NULL, .as_na = NULL) {
  # point <- miss_y[1,]

  # buffer_values
  # alternative could be to fix points around (probably faster), but
  # extract does fancy stuff with buffer distance if lonlat
  # could also just count cells on each side, would then use a cell_dist argument

  p_buff <- sf::st_buffer(point, max_range)

  buff_vals0 <- terra::extract(x, p_buff, xy = TRUE,
                               cells = TRUE, touches = TRUE)

  # FIX make sure it handles NA values
  buff_vals <- buff_vals0
  # if(length(buff_vals0) == 1 && is.na(buff_vals0)) {
  #   tibble(near_val = NA,
  #              dist_cell = NA)
  # }

  # subset raster
  # there is no equivalent to rasterFromCells in terra
  # maybe get x y, then crop?
  x_sub <- terra::crop(x, terra::ext(x, cells = buff_vals$cell))

  # remove .as_na values
  if (!is.null(.as_na)) {
    stop("Not implemented yet")
    # na_df <- data.frame(id = .as_na, value = NA)
    # x_sub <- subs(x_sub, na_df, subsWithNA=FALSE)

  }
  # plot(x_sub)

  # distance
  # terra fct behaves differently
  # dist <- terra::distance(rast(x_sub), vect(point))
  dist <- rast(raster::distanceFromPoints(raster::raster(x_sub), point))

  dist[is.na(x_sub)] <- NA
  dist[dist > max_range] <- NA
  #plot(dist)
  #plot(vect(point), add = T)

  # if many mins, pick one
  min_cells <- terra::where.min(dist) %>% as_tibble
  min_cell <- min_cells %>% slice_sample(n = 1)

  no_min <- is.na(min_cell)

  near_val <- if (nrow(min_cell) == 0) NA else {
    values(x_sub)[min_cell$cell]
  }

  data.frame(near_val = near_val,
             dist_cell = min_cell %>% pull(value))
}

