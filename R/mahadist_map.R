


#' Make grid
#'
#' Specific use in spatial model predictions and mahalanobis analysis.
#'
#' @param reso in degree
#'
#' @return tibble
#' @export
make_grid <- function(reso){
  # could also use tidyr::crossing
  tibble(X_from = seq(-180, 180 - reso, by = reso) %>% rep(180 / reso),
         X_to =seq(-180 + reso, 180, by = reso) %>% rep(180 / reso),
         Y_from = seq(-90, 90 - reso, by = reso) %>% rep(each = 360 / reso),
         Y_to = seq(-90 + reso, 90, by = reso) %>% rep(each = 360 / reso),
         X_mid = X_from + reso / 2,
         Y_mid = Y_from + reso / 2) %>%
    sf::st_as_sf(coords = c("X_mid", "Y_mid"), remove = FALSE,
                 crs = CRS("+init=epsg:4326"))
}



#' Calculate mahalanobis distance
#'
#' @param dat data
#' @param world data.frame
#' @param vars variables used
#' @param xy coordinate column names as character vector
#'
#' @return input data.frame with added columns
#' @export
mahadist <- function (dat, world, vars, xy = c("X", "Y")){
  #check names in both df, stop if not

  stopifnot(all(c(vars %in% names(dat), vars %in% names(world))))

  dat_sub <- dat %>% dplyr::select(all_of(vars)) %>%
    tidyr::drop_na()


  dat_diff_nrow <- nrow(dat) - nrow(dat_sub)
  if (dat_diff_nrow > 0) {
    message(dat_diff_nrow, " dat entries with NA values removed")
  }

  world_sub <- world %>% dplyr::select(all_of(xy), all_of(vars)) %>%
    tidyr::drop_na()

  wrld_diff_nrow <- nrow(world) - nrow(world_sub)
  if (wrld_diff_nrow > 0) {
    message(wrld_diff_nrow, " dat entries with NA values removed")
  }

  world_calc <- world_sub %>% dplyr::select(-c(all_of(xy)))

  mu <- colMeans(dat_sub) #vector of means
  sigma <- cov(dat_sub) #covariance matrix
  limit97 <- qchisq(.975, df = length(dat_sub))
  limit50 <- qchisq(.5, df = length(dat_sub))

  mahaDist <- mahalanobis(world_calc, mu, sigma)

  world_calc <- world_calc %>%
    mutate(mahaDistance = mahaDist,
           mahatype = case_when(
             is.na(mahaDist) ~ NA_character_,
             mahaDistance < limit50 ~ "ok",
             mahaDistance < limit97 ~ "chisq > 0.5",
             TRUE ~ "chisq > 0.975"
           ))

  world_calc <- world_calc %>%
    mutate(mahatype = factor(world_calc$mahatype,
                             levels = c("ok", "chisq > 0.5", "chisq > 0.975")))

  outliers <- length(which(world_calc$mahaDistance > limit97))
  # world_calc %>% filter(mahatype == "chisq > 0.975") %>% nrow #check

  message(paste0(outliers, " outliers at 97.5% limit (", round(outliers/nrow(world_calc)*100, 2), "%)"))

  world_calc <- bind_cols(world_sub %>% dplyr::select(all_of(xy)), world_calc)

  return(world_calc)

}

#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

#' Load spatial libraries
#'
#' @return
#' @export
splibs <- function(){
  library(ggplot2)
  theme_set(theme_bw())
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)

}

#' Make a mahalanobis map
#'
#' @param dat data
#' @param xy coordinate columns
#'
#' @return
#' @export
mahamap <- function (dat, xy = c("X", "Y")){
  splibs()
  worldmap <- ne_countries(scale = "medium", returnclass = "sf")
  worldmap <- worldmap %>% filter(admin != "Antarctica")

  ggplot(data = worldmap) +
    geom_sf()+
    geom_tile(data = dat, aes_string(x=xy[1], y=xy[2], fill="mahatype"))+
    scale_fill_manual(values=c("green", "orange", "red"))+
    theme(panel.background = element_rect(fill = "lightskyblue1"))

}

# ggplot(data = dat, aes(x = X, y=Y, fill=mahatype)) +
#   geom_sf(data = world, inherit.aes = FALSE)+
#   geom_tile()+
#   scale_fill_manual(values=c("green", "orange", "red"))


#' Variant of mahalanobis map
#'
#' Uses gradient
#'
#' @param dat data
#' @param xy coordinate columns
#' @param mask boolean
#'
#' @return ggplot
#' @export
mahagrad <- function (dat, xy = c("X", "Y"), mask = F){
  splibs()
  worldmap <- ne_countries(scale = "medium", returnclass = "sf")

  p <- ggplot(data = worldmap) +
    geom_sf()+
    geom_tile(data = dat, aes_string(x=xy[1], y=xy[2], fill="mahaDistance"))+
    scale_fill_gradient(low = "green", high = "red", trans = "sqrt")+
    theme(panel.background = element_rect(fill = "lightskyblue1"))

  if (mask) {

    mahamask <- dat %>% filter(mahatype == "chisq > 0.975")
    p <- p +
      geom_tile(data = mahamask, aes_string(x=xy[1], y=xy[2]), fill = "grey")
  }

  p

}


#' Maha+mask
#'
#' @param dat data
#' @param xy coordinate columns
#'
#' @return ggplot
#' @export
mahamask <- function (dat, xy = c("X", "Y")){
  splibs()
  worldmap <- ne_countries(scale = "medium", returnclass = "sf")
  worldmap <- worldmap %>% filter(admin != "Antarctica")

  showland <- dat %>% filter(!mahatype == "chisq > 0.975")
  maskland <- dat %>% filter(mahatype == "chisq > 0.975")

  ggplot(data = worldmap) +
    geom_tile(data = showland, aes_string(x=xy[1], y=xy[2], fill="mahaDistance"))+
    geom_tile(data = maskland, aes_string(x=xy[1], y=xy[2]), fill = "grey80")+
    scale_fill_gradient(low = "green", high = "red", guide = F)+ #, trans = "sqrt"
    geom_sf(fill = NA, color = "grey50", size = 0.1)+
    #theme(panel.background = element_rect(fill = "lightskyblue1"))
    theme_void()

}

# darker outliers, lighter grey NA
# ggplot(data = worldmap) +
#   geom_sf(fill = "grey90", color = NA, size = 0.1)+
#   geom_tile(data = showland, aes_string(x=xy[1], y=xy[2], fill="mahaDistance"))+
#   geom_tile(data = maskland, aes_string(x=xy[1], y=xy[2]), fill = "grey20")+
#   scale_fill_gradient(low = "green", high = "red", guide = F)+ #, trans = "sqrt"
#   geom_sf(fill = NA, color = "grey50", size = 0.1)+
#   #theme(panel.background = element_rect(fill = "lightskyblue1"))
#   theme_void()
