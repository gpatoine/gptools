

#check nonnum values
#' which.nonnum
#'
#' @param x
#'
#' @return
#' @export
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}


#' nonumdf
#'
#' @param df
#' @param cols
#'
#' @return
#' @export
nonumdf <- function(df, cols = seq_along(df)){
  #df = fv1
  #

  nonumlist <- lapply(df %>% select(New_ID, all_of(cols)), which.nonnum)

  nonumdf <- NULL

  for (ielem in seq_along(nonumlist)) {
    #ielem <- 3

    if (length(nonumlist[[ielem]]) == 0) {
      next
    }

    nonumval <- tibble(row = nonumlist[[ielem]],
                       column = names(nonumlist)[ielem],
                       New_ID = df %>% select(New_ID) %>% slice(row) %>% unlist,
                       value = df %>% select(names(nonumlist)[ielem]) %>% slice(row) %>% unlist
    )

    nonumdf <- bind_rows(nonumdf, nonumval)

  }

  nonumdf

}



#' repl_dict
#'
#' dictionary of terms to replace
#'
#' @param x
#' @param dict
#'
#' @return
#' @export
repl_dict <- function (x, dict) {

  for(idict in seq_along(dict)) {
    x[x == names(dict[idict])] <- dict[idict]
  }

  x
}


check_modif <- function (col1, col2) {
  tibble(col1, col2) %>%
    filter(!map2_lgl(col1, col2, identical))
}


#' dputran
#'
#' dput with integer ranges
#'
#' @param x
#'
#' @return
#' @export
dputran <- function(x) {
  sx <- sort(x) %>% unique
  diff <- sx[-1] - sx[-length(sx)]

  same <- diff == 1

  string <- sx[1] %>% as.character

  pos <- 2
  len <- length(sx)
  follow <- 0

  while (pos <= len){

    if (same[pos-1]) {

      follow <- follow + 1

    } else {

      if (follow > 0) {
        string <- paste0(string, ":", sx[pos-1], sep = "")
      }

      follow <- 0
      string <- paste0(string, ",", sx[pos])

    }

    pos <- pos + 1

  }

  if (follow > 0) {
    string <- paste0(string, ":", sx[pos-1], sep = "")
  }

  if (str_detect(string, ",")) {
    string <- paste0("c(", string, ")")
  }

  cat(string)
  invisible(string)

}



#' File opened
#'
#' Check if file is available for writing
#'
#' @param path
#'
#' @return logical
#' @export
file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path,
               open = "w"),
          silent = TRUE
      )
    )
  )
}

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
