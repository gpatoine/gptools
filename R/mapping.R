
#' Create sf object (WGS84) from dataframe
#'
#' Guesses which columns contain location information
#'
#' @param data data
#' @param coords cloumn names like c("x", "y")
#'
#' @return sf object
#' @export
make_sf_wgs84 <- function(data, coords = NULL) {

  if (inherits(data, "sf")) {

    message("The object is already of class `sf`")
    return(data)

  }

  checkmate::assert_data_frame(data)

  # deal with coords
  if (!is.null(coords)) {

    checkmate::assert_character(coords, len = 2)
    checkmate::assert(all(coords %in% names(data)))

    lng_tag <- coords[1]
    lat_tag <- coords[2]

  } else {

    lng_vect <- c("longitude", "Longitude", "lng", "Lng", "long", "Long", "x", "X")
    lat_vect <- c("latitude", "Latitude", "lat", "Lat", "y", "Y")

    lng_tag <- lng_vect[which(lng_vect %in% names(data))[1]]
    lat_tag <- lat_vect[which(lat_vect %in% names(data))[1]]

    checkmate::assert(!(is.na(lat_tag)) & !(is.na(lng_tag)))

  }

  data %>% dplyr::rename(longitude = all_of(lng_tag),
                       latitude = all_of(lat_tag)
                       ) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326,
                 remove = FALSE)

}



#' Quick world map
#'
#' @param data
#'
#' @return ggplot
#' @export
qwmap <- function(data) {

  if (!inherits(data, "sf")) {

    data <- make_sf_wgs84(data)

  }

  ggplot2::ggplot(data) +
    ggplot2::borders()+
    ggplot2::geom_sf(color = "blue", alpha = 0.2, size = 2)+
    ggplot2::coord_sf(expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Longitude", y = "Latitude")

}


# NOTE Pointmap works best with mapview. The leaflet and ggplot version needs additional work
# It seemed like a good idea to have one function to do all, but the different methods require
# different data formats, so sf might not always be the way to go

#' Point map
#'
#' Can be used with df or sf, and implemented with ggplot, leaflet, and mapview
#'
#' @param data dataset
#' @param ids character colum name to identify ids to plot
#' @param coords numeric vector of length 2
#' @param type gg or leaf
#'
#' @return ggplot or leaflet map
#' @export
gp_pointmap <- function(data, id_col = NULL, coords = NULL, type = c("mapview", "leaflet", "ggplot")) {

  checkmate::assert_data_frame(data)

  if (!inherits(data, "sf")) {

    data <- make_sf_wgs84(data, coords)

  }

  type <- match.arg(type)

  if (type == "mapview") {

    if (!is.null(id_col)) {

      mapview::mapview(data, zcol = id_col)

    } else {

      mapview::mapview(data)

    }

  } else if (type == "leaflet") {

    # TODO add color per group, fix id_col

    lmap <- leaflet::leaflet(data = data) %>%
      leaflet::addTiles() %>%
      # leaflet::addProviderTiles("Stamen.Watercolor",
      #                           options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
      leaflet::addMarkers(
        # lat = ~latitude,
        # lng = ~longitude,
        popup = ~as.character(paste0("<b>", "id: ",
                                     if (!is.null(id_col)) id_col else "NA",
                                     "</b>", "<br/>", "Coords: ", geometry)), # How to round geometry
        clusterOptions = leaflet::markerClusterOptions()) %>%
      leaflet::addScaleBar(position = "topright")

    if(nrow(data) == 1) {

      lmap <- lmap %>%
        leaflet::setView(lng = .$x$calls[[2]]$args[[2]],
                         lat = .$x$calls[[2]]$args[[1]],
                         zoom = 12)

    } else {
      lmap

    }

  } else if (type == "ggplot") {

    p <- qwmap(data)

    if (!is.null(id_col)) {

    p +
      ggplot2::geom_sf_label(aes(label = {{id_col}}))
    } else {
      p

    }

  }

}



#' Plot point neighborhood from raster
#'
#' @param point only single sf point. will only consider first row
#' @param ras rasteLayer
#' @param dist range
#'
#' @return ggplot
#' @export
#'
#' @examples gp_point_neibor(point, lcras_ori, 80000)
gp_point_ras <- function(point, ras, dist = 8000, type = c("mapview", "ggplot")) {

  if (!inherits(point, "sf")) {
    point <- make_sf_wgs84(point)
  }

  type <- match.arg(type)

  # extract cell with buffer
  point <- point %>% dplyr::slice(1)

  point <- sf::st_transform(point, crs(ras))

  buff_vals0 <- raster::extract(ras, point, buffer = dist*1.05,
                                cellnumbers = TRUE, small = TRUE) %>% .[[1]]

  # NEW
  if(length(buff_vals0) == 1 && is.na(buff_vals0)) {
    return(list(value = numeric(0), dist = numeric(0)))
  }

  # deal with different extract outputs
  buff_vals <-
    if (inherits(buff_vals0, "numeric")) {
      data.frame(as.list(buff_vals0))
    } else {
      as.data.frame(buff_vals0)
    }

  # subset raster
  x_sub <- raster::rasterFromCells(ras, buff_vals$cell)
  x_sub[] <- ras[raster::values(x_sub)]

  # remove too far cells
  # need to transform, raster doesn't by itself
  dist_r <- replace(raster::distanceFromPoints(x_sub, point),
                    is.na(x_sub), NA)
  dist_r[dist_r > dist] <- NA

  # subset
  x_sub[is.na(dist_r)] <- NA

  if (type == "mapview") {

    # method argument needed to avoid reprojection and wrong NA values
    # see: https://github.com/r-spatial/mapview/issues/123
    mapview::mapview(x_sub, method = "ngb") +
      mapview::mapview(point)

  } else {

    gp_gplot(x_sub)+
      ggplot2::geom_sf(data = point, inherit.aes = F)

  }

}


#' Open location in googlemaps
#'
#' @param point sf point object
#'
#' @return open a new browser tab
#' @export
#'
#' @examples open_gmaps(point)
gp_open_gmaps <- function(point){

  point <- point %>% dplyr::slice(1)

  if (!inherits(point, "sf")) {

    # slow, but wtv
    point <- make_sf_wgs84(point)

  }

  # point %>% st_coordinates %>% rev %>% toString %>% writeClipboard
  # browseURL("https://www.google.com/maps/")

  loc <- point %>% sf::st_coordinates() %>% rev
  browseURL(paste0("https://www.google.com/maps/search/", loc[1], ",", loc[2], "/@", loc[1], ",", loc[2], ",14z"))

}



#' gplot from rasterVis hacked
#'
#' see https://github.com/oscarperpinan/rastervis/blob/master/R/gplot.R
#' as opposed to the rasterVis implementation, this version does all the work
#' already of setting geom_raster, scale_fill and coord_fixed
#'
#' Not implemented for RasterStack
#'
#' @param x a RasterLayer or SpatRaster
#' @param maxpixels Maximum number of pixels to use
#' @param filt_val filter value for categorical raster
#'
#' @return ggplot
#' @export
gp_gplot <- function(x, maxpixels = 5e+4, title = names(x)[1], filt_val = NULL) { #, ...

  # NOTE could use basename(filename(r)) to avoid replacing - with ., but then won't work for rasterStack

  x0 <- x

  if (inherits(x, "RasterLayer")) {

    x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)

    coords <- raster::xyFromCell(x, seq_len(ncell(x)))
    dat <- utils::stack(as.data.frame(raster::values(x)))
    names(dat) <- c('value', 'variable')
    dat <- cbind(coords, dat)

    subt <- paste0("gain: ", raster::gain(x0), ", offs: ", raster::offs(x0))


  } else if (inherits(x, "SpatRaster")) {

    # plot only the first layer
    if (terra::nlyr(x) > 1) x <- x[[1]]

    x <- terra::spatSample(x, maxpixels, "regular", as.raster = TRUE)

    coords <- terra::xyFromCell(x, seq_len(terra::ncell(x)))
    dat <- utils::stack(as.data.frame(terra::values(x)))
    names(dat) <- c('value', 'variable')
    dat <- cbind(coords, dat)

    subt <- scoff(t) %>% paste(colnames(.), ., sep = ": ", collapse = ", ")


  } else stop("Wrong class")

  # dat$value %>% unique

  if (!is.null(filt_val)) {
    dat <- dat %>% filter(value == filt_val)
  }

  ggplot2::ggplot(data=dat, ggplot2::aes(x = x, y = y))+ #, ...
    ggplot2::geom_raster(ggplot2::aes(fill = value))+
    ggplot2::scale_fill_viridis_c(na.value = NA)+
    ggplot2::coord_fixed()+
    ggplot2::ggtitle(title, subt)

}
