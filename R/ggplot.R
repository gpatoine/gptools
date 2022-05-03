


#' one1_line
#'
#' add one-one slope to ggplot
#'
#' @return ggplot thingy
#' @export
one1_line <- function() {

  ggplot2::geom_abline(slope = 1)

}


#' Plot correlation
#'
#' @param data data
#' @param x variable
#' @param y variable
#'
#' @return ggplot
#' @export
plot_corr <- function(data, x, y) {

  corr <- data %>% select({{x}}, {{y}}) %>% cor(use = "complete") %>% .[2,1] %>% round(3)

  ggplot(soil_mer, aes({{x}}, {{y}}))+
    geom_point()+
    geom_abline(slope = 1)+
    annotate(geom = "text", x = -Inf, y = Inf,
             label = paste0("r = ", corr),
             hjust = -0.1, vjust = 1.1)

}
