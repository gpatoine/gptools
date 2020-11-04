#
#
#
# # https://rdrr.io/cran/raster/src/R/extractPoints.R
# # https://github.com/rspatial/raster/blob/master/R/extractPoints.R
# # https://rspatial.org/raster/pkg/8-cell_level_functions.html
# # https://github.com/rspatial/raster/blob/master/R/distance.R
#
#
# # usefull raster fcts
#
# couldBeLonLat(r)
# isLonLat(r)
#
# # easy: rerun extract with buffer --------------------------------------------
#
# # iSBio data
#
# tcomp <- readRDS(here("data_mod/2-4_tcomp_extracted.rds"))
#
# rast_path <- file.path(datapath, "static/SoilGrids/phh2o_0-5cm_mean.tif")
# file.exists(rast_path)
#
# ph_rast <- raster(rast_path)
#
# tcomp$ph1 <- extract(ph_rast, tcomp)
# tcomp$ph2[is.na(tcomp$ph1)] <- extract(ph_rast, tcomp[is.na(tcomp$ph1),], buffer = 1000)
#
# tcomp[is.na(tcomp$ph1),] %>% View
#
#
#
# # potential helpers for assign buffer region ------------------------------
#
# dims <- rowColFromCell(ph_rast, 1978878419)
# cellFromRowCol(ph_rast,dims[1]+1,dims[2]+1)
# ph_rast[cellFromRowCol(ph_rast,dims[1]+1,dims[2]+1)]
# xyFromCell(ph_rast, 1978878419+1)
# cellFromXY(ph_rast, xy = c(1955875+250, 5254375))
#
# extract(ph_rast, y = data.frame(x= 1955875, y = 5254375))
# extract(ph_rast, tcomp[1,])
# ph_rast[cellFromXY(ph_rast, data.frame(x = 1955875, y = 5254375))]
#
#
# plot(ph_rast[12427:12437, 87623:87633])
#
# ph_subset <- rasterFromCells(ph_rast, cellFromRowCol(ph_rast, 12427:12437, 87623:87633))
#
# ph_subset <- rasterFromCells(ph_rast, cellFromRowCol(ph_rast, 12427:12437, 87623:87633)[-c(4,7,11)])
#
# # only paired
# new_cells <- cellFromRowCol(ph_rast, 12427:12437, 87623:87633)#[-c(4,7,11)]
# # all possibilities
# new_cells <- cellFromRowColCombine(ph_rast, 12427:12437, 87623:87633)#[-c(4,7,11)]
#
# ph_subset <- rasterFromCells(ph_rast, range(new_cells))
# ph_subset[1:3,1:3, drop=FALSE] %>% plot
#
# plot(ph_subset, axes=FALSE, box=FALSE)
# plot(rasterToPolygons(ph_subset), add=TRUE, border='black', lwd=1)
#
# distanceFromPoints(ph_subset, tcomp[1,]) %>% plot
#
# # remove NAs, pick minimum value
#
# cellFromRowCol(ph_rast)
#
# tcomp$ph1 <- extract(ph_rast, tcomp)
# tcomp$ph2 <- NA
# tcomp$ph2[is.na(tcomp$ph1)] <- extract(ph_rast, tcomp[is.na(tcomp$ph1),], buffer = 1000,
#                                        fun = mean, cellnumbers = TRUE)
#
# # check tidy from broom package to add two columns
#
# tcomp %>% select(ph1, ph2) %>% View
#
# tcomp[is.na(tcomp$ph1),] %>% View
#
# file.exists(rast_path)
#
# identical(tcomp$ph1, tcomp$ph1)
#
# sum(is.na(tcomp$ph1))
#
# sampled = apply(X = tcomp, MARGIN = 1,
#                 FUN = function(tcomp) getValues(ph_rast)[which.min(
#                   replace(distanceFromPoints(ph_rast, tcomp[1,]), is.na(ph_rast), NA))])
# plot(distanceFromPoints(ph_rast, tcomp[1,]))
#
# tcomp[1,]
# plot(ph_rast, axes=F, box=F)
# points(tcomp, pch=3)
# plot(sf::st_geometry(tcomp[200,]), pch =16, cex = 0.8, add = TRUE)
#
# extent(phcrop, sf::st_bbox(tcomp[1,]))
#
# phcrop <- crop(ph_rast, extent(c(tcomp$Longitude[1] - 10, tcomp$Longitude[1] + 10,
#                                  tcomp$Latitude[1] - 10, tcomp$Latitude[1] + 10)))
#
# plot(phcrop)
#
#
#
# # more --------------------------------------------------------------------
#
# library(raster)
#
#
# set.seed(2)
#
# # create a 10x10 raster
# r <- raster(ncol=10,nrow=10, xmn=0, xmx=10, ymn=0,ymx=10)
# r[] <- 1:10
# r[sample(1:ncell(r), size = 25)] <- NA
#
# # plot the raster
# plot(r, axes=F, box=F)
# segments(x0 = 0, y0 = 0:10, x1 = 10, y1 = 0:10, lty=2)
# segments(y0 = 0, x0 = 0:10, y1 = 10, x1 = 0:10, lty=2)
#
# # create sample points and add them to the plot
# xy = data.frame(x=runif(10,1,10), y=runif(10,1,10))
# points(xy, pch=3)
# text(x = xy$x, y = xy$y, labels = as.character(1:nrow(xy)), pos=4, cex=0.7, xpd=NA)
#
# # use normal extract function to show that NAs are extracted for some points
# extracted = extract(x = r, y = xy)
#
# # then take the raster value with lowest distance to point AND non-NA value in the raster
# sampled = apply(X = xy, MARGIN = 1,
#                 FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy[1,]),
#                                                                    is.na(r), NA))])
#
#
# distanceFromPoints()
# plot(replace(distanceFromPoints(r, xy[1,]),
#              is.na(r), NA))
#
# # https://stackoverflow.com/questions/39375629/how-to-subset-a-raster-by-cell-number-in-r
# r <- raster(ncols=100, nrows=100)
# r[] <- rnorm(ncell(r))
#
# cells <- c(3:5, 210)
#
# r2 <- rasterFromCells(r, cells, values=TRUE)
# plot(r)
# plot(r2)
# ini_cells <- getValues(r2)
# # Simply feed the values according to the index:
#
# r2[] <- r[ini_cells]
# # This results in a raster of 24 cells instead of 10'000!
#
# c(ncell(r), ncell(r2))
#
#
#
#
# # testing with stack ------------------------------------------------------
#
#
# library(raster)
#
# abc <- stack(10, 10)
#
# abc <- raster(nrows = 10, ncols = 10)
#
#
# r <- raster(ncols=10, nrows=10)
# r1 <- setValues(r, runif(ncell(r)))
# r2 <- setValues(r, runif(ncell(r)))
# r3 <- setValues(r, runif(ncell(r)))
# r4 <- setValues(r, runif(ncell(r)))
# r5 <- setValues(r, NA)
# r6 <- setValues(r, runif(ncell(r)))
# r1[24:50] <- NA
# r2[20:60] <- NA
# r3[32:100] <- NA
# s <- stack(r1,r2,r3,r4,r6)
# plot(s)
#
# s[1:5] <- NA
# x1 <- approxNA(s)
# x2 <- approxNA(s, rule=2)
# x3 <- approxNA(s, rule=2, z=c(1,2,3,5,14,15))
#
# plot(x1)
# plot(x2)
# plot(x3)
#
# ana <- all(is.na(s[[1]]))
#
# plot(ana)
#
# raster::un
#
#
#
#
#
# # test param --------------------------------------------------------------
# r = r1
# points <- data.frame(
#   latitude = runif(n = 20, min = -5, max = 5),
#   longitude = runif(n = 20, min = -175, max = 175)
# ) %>% st_as_sf(coords = c("longitude", "latitude"))
#
# plot(r1)
# plot(points, add =T)
#
#
# map_dfr(seq_len(nrow(missing_values)),
#         ~ nearest_value(r, missing_values[.x,], max_range))
#
