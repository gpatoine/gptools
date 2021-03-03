
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
#'
#' @examples
gp_pointmap <- function(data, id_col = NULL, coords = NULL, type = "gg") {

  checkmate::assert_data_frame(data)
  # checkmate::assert_character(coords, len = 2) #or NULL

  # if (!is.null(ids)) {
  #   x <- x %>% dplyr::filter(id %in% ids)
  # }

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
      ggplot2::geom_point(color = "orange")+
      ggplot2::coord_fixed()

    if (!is.null(id_col)) {
      qwmap +
        ggrepel::geom_text_repel(ggplot2::aes(label = id_col), color = "blue")

    } else {
      qwmap

    }
  }
}
