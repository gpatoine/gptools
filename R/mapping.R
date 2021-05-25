
# TODO still issues with the leaflet one

#' Quick world map
#'
#' @param data dataset
#' @param ids character colum name to identify ids to plot
#' @param coords numeric vector of length 2
#' @param type gg or leaf
#'
#' @return ggplot or leaflet map
#' @export
gp_pointmap <- function(data, id_col = NULL, coords = NULL, type = "gg") {

  checkmate::assert_data_frame(data)
  # checkmate::assert_character(coords, len = 2) #or NULL

  # if (!is.null(ids)) {
  #   x <- x %>% dplyr::filter(id %in% ids)
  # }

  if (inherits(data, "sf")) {
    qwmap <- ggplot2::ggplot(data) +
      ggplot2::borders()+
      ggplot2::geom_sf(color = "red", shape = 1)+
      ggplot2::theme_bw()

    return(qwmap)
  }

  if (!is.null(coords)) {
    lat_tag <- coords[1]
    lng_tag <- coords[2]

  } else {
    lat_vect <- c("latitude", "lat", "y", "Y")
    lng_vect <- c("longitude", "lng", "long", "x", "X")

    lat_tag <- lat_vect[which(lat_vect %in% names(data))[1]]
    lng_tag <- lng_vect[which(lng_vect %in% names(data))[1]]
  }

  data <- data %>% dplyr::rename(latitude = all_of(lat_tag),
                                 longitude = all_of(lng_tag),
                                 id_col = {{id_col}})

  if (type == "leaf") {
    leaflet::leaflet(data = data) %>%
      leaflet::addProviderTiles("Stamen.Watercolor",
                                options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
      leaflet::addMarkers(lat = ~latitude,
                          lng = ~longitude,
                          popup = ~as.character(paste(sep = "", "<b>", "id: ",
                                                      if (!is.null(id_col)) id_col else "NA",
                                                      "</b>", "<br/>", "Lat: ",
                                                      latitude, "<br/>", "Lon: ", longitude)),
                          clusterOptions = leaflet::markerClusterOptions()) %>%
      leaflet::addScaleBar(position = "topright")

  } else {

    qwmap <- ggplot2::ggplot(data, ggplot2::aes(longitude, latitude)) +
      ggplot2::borders()+
      ggplot2::geom_point(color = "darkblue", shape = 1)+
      ggplot2::coord_fixed()+
      ggplot2::theme_bw()

    if (!is.null(id_col)) {
      qwmap +
        ggrepel::geom_text_repel(ggplot2::aes(label = id_col), color = "blue")

    } else {
      qwmap

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
gp_point_neighbor <- function(point, ras, dist = 8000) {

  # extract cell with buffer

  point <- point %>% dplyr::slice(1)

  buff_vals0 <- raster::extract(ras, point, buffer = dist*1.05,
                                cellnumbers = TRUE, small = TRUE) %>% .[[1]]

  # NEW
  if(length(buff_vals0) == 1 && is.na(buff_vals0)) {
    return(list(value = numeric(0), dist = numeric(0)))
  }

  # deal with different extract outputs
  buff_vals <-
    if (class(buff_vals0) == "numeric") {
      data.frame(as.list(buff_vals0))
    } else {
      as.data.frame(buff_vals0)
    }

  # subset raster
  x_sub <- raster::rasterFromCells(ras, buff_vals$cell)
  x_sub[] <- ras[values(x_sub)]

  # remove too far cells
  dist_r <- replace(raster::distanceFromPoints(x_sub, point), is.na(x_sub), NA)
  dist_r[dist_r > dist] <- NA

  # subset
  x_sub[is.na(dist_r)] <- NA

  rasterVis::gplot(x_sub)+
    ggplot2::geom_raster(aes(fill = factor(value)))+
    ggplot2::geom_sf(data = point, inherit.aes = F)+
    ggplot2::scale_fill_discrete(type = "Dark2", na.value = NA)

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

  point <- point %>% slice(1)

  # point %>% st_coordinates %>% rev %>% toString %>% writeClipboard
  # browseURL("https://www.google.com/maps/")

  loc <- point %>% st_coordinates %>% rev
  browseURL(paste0("https://www.google.com/maps/@", loc[1], ",", loc[2], ",14z"))

}



#' gplot from rasterVis hacked
#'
#' see https://github.com/oscarperpinan/rastervis/blob/master/R/gplot.R
#' as opposed to the rasterVis implementation, this version does all the work
#' already of setting geom_raster, scale_fill and coord_fixed
#'
#' @param x a RasterLayer
#' @param maxpixels Maximum number of pixels to use
#' @param filt_val filter value for categorical raster
#'
#' @return ggplot
#' @export
gp_gplot <- function(x, maxpixels = 5e+4, filt_val = NULL) { #, ...

  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)

  coords <- xyFromCell(x, seq_len(ncell(x)))
  dat <- utils::stack(as.data.frame(values(x)))
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  # dat$value %>% unique

  if (!is.null(filt_val)) {
    dat <- dat %>% filter(value == filt_val)
  }

  ggplot2::ggplot(data=dat, ggplot2::aes(x = x, y = y))+ #, ...
    geom_raster(aes(fill = value))+
    scale_fill_viridis_c(na.value = NA)+
    coord_fixed()

}
