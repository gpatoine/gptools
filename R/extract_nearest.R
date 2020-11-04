# # TODO make as a function
#
#
# # select <- dplyr::select
#
# # input spatial points location (e.g. tcomp), raster layer
#
# # output dataframe with new value and cell distance
#
#
# # example data
#
# tcomp <- readRDS(here("data_mod/2-4_tcomp_extracted.rds"))
# points <- tcomp
#
# rast_path <- file.path(datapath, "SoilGrids/phh2o_0-5cm_mean.tif")
# r <- raster(rast_path)
#
# # example
# df <- extract_nearest(r, points, max_range = 2000)
#
#
#
# # main function -----------------------------------------------------------
# # TODO run it for each unique coordinates (faster if repeated values)
#
# extract_nearest <- function(r, points, max_range = 5000) {
#
#   # try to extract
#   extract1 <- extract(r, points, cellnumbers = TRUE) %>% as_tibble
#
#   points <- points %>% mutate(cells = extract1$cells,
#                               extr_val = extract1[[2]],
#                               dist = 0)
#
#   # subset NAs
#   missing_values <- points %>% filter(is.na(extr_val))
#
#   extract_miss <- map_dfr(seq_len(nrow(missing_values)),
#                           ~ nearest_value(r, missing_values[.x,], max_range))
#
#   missing_values <- missing_values %>% bind_cols(extract_miss)
#
#   # merge with original dataset
#   stgeo <- st_geometry(points)
#   points <- points %>% st_drop_geometry()
#
#   points[is.na(points$extr_val), c("extr_val", "dist")] <-
#     missing_values[c("sampl_val", "dist_cell")] %>% st_drop_geometry()
#
#   points %>% select(-c(cells)) %>% st_set_geometry(stgeo)
#
# }
#
#
# # subset NA, run nearest neighbor
#
# # for each NA, get cells in buffer, subset raster, calc distance, pick smallest
# # return value and distance
#
# nearest_value <- function(r, point, max_range = Inf) {
#   # point <- miss1[1,]
#   # max_range <- 50000
#
#   # buffer values
#   # alternative could be to fix points around (probably faster)
#   buff_vals <- extract(r, point, buffer = max_range,
#                        cellnumbers = TRUE) %>% as.data.frame
#
#   # subset raster
#   r_sub <- rasterFromCells(r, buff_vals$cell)
#   #plot(r_sub)
#
#   r_sub[] <- r[getValues(r_sub)]
#   #plot(r_sub)
#
#   # distance matrix
#
#   point <- st_transform(point, crs(r_sub))
#
#   dist <- replace(distanceFromPoints(r_sub, point), is.na(r_sub), NA)
#   #plot(dist)
#
#   xyCell <- xyFromCell(dist, which.min(dist))
#
#   tibble(sampl_val = extract(r_sub, xyCell),
#          dist_cell = minValue(dist))
#
# }
#
# #nearest_value(r, point = miss1[11,], 2000)
#
#
#
