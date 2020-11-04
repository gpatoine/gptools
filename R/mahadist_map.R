mahadist <- function (dat, world, vars, xy = c("X", "Y")){
  #check names in both df, stop if not
  if(all(c(vars %in% names(dat), vars %in% names(world)))){
    dat_sub <- dat %>% dplyr::select(all_of(vars)) %>%
      drop_na

    world_sub <- world %>% dplyr::select(all_of(xy), all_of(vars)) %>%
      drop_na

    world_calc <- world_sub %>% dplyr::select(-c(all_of(xy)))

    message("NA values, if any, were removed.")

    mu <- colMeans(dat_sub) #vector of means
    sigma <- cov(dat_sub) #covariance matrix
    limit97 <- qchisq(.975, df=length(dat_sub))
    limit50 <- qchisq(.5, df=length(dat_sub))

    world_calc$mahaDistance <- mahalanobis(world_calc, mu, sigma)
    world_calc$mahatype <- "ok"
    world_calc$mahatype[is.na(world_calc$mahaDistance)] <- NA
    world_calc$mahatype[world_calc$mahaDistance > limit50] <- "chisq > 0.5"
    world_calc$mahatype[world_calc$mahaDistance > limit97] <- "chisq > 0.975"

    world_calc$mahatype <- factor(world_calc$mahatype, levels = c("ok", "chisq > 0.5", "chisq > 0.975"))

    outliers <- length(which(world_calc$mahaDistance > limit97))

    print(paste0(outliers, " outliers at 97.5% limit (", round(outliers/nrow(world_calc)*100, 2), "%)"))

    world_calc <- bind_cols(world_sub %>% dplyr::select(all_of(xy)), world_calc)

    return(world_calc)

  } else {
    stop("variable names not in dataframes")
  }
}

#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

splibs <- function(){
  library(ggplot2)
  theme_set(theme_bw())
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)

}

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
