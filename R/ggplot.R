


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

  corr <- data %>% dplyr::select({{x}}, {{y}}) %>% cor(use = "complete") %>% .[2,1] %>% round(3)

  ggplot2::ggplot(data, ggplot2::aes({{x}}, {{y}}))+
    ggplot2::geom_point()+
    ggplot2::geom_abline(slope = 1)+
    ggplot2::annotate(geom = "text", x = -Inf, y = Inf,
             label = paste0("r = ", corr),
             hjust = -0.1, vjust = 1.1)

}



# simple plots ------------------------------------------------------------

#' plot one variable vs index
#'
#' @param x vec
#'
#' @return ggplot
#' @export
pl <- function(x) {

  tibble(value = x,
         index = seq_along(value)) %>%
    ggplot(aes(index, value))+
    geom_point(alpha = 0.5)

}


#' sort-plot one var
#'
#' @param x vec
#'
#' @return ggplot
#' @export
spl <- function(x) {

  pl(sort(na.omit(x)))

}


#' pull-sort-plot in base
#'
#' @param data df
#' @param col name
#'
#' @return NULL
#' @export
psplb <- function(data, col) {

  data %>% pull({{col}}) %>% sort %>% plot

}


#' pull-sort-plot
#'
#' main fct to use
#'
#' @param data df
#' @param col name
#'
#' @return ggplot
#' @export
#'
#' @examples pspl(iris, Sepal.Length)
pspl <- function(data, col) {

  # data %>% pull({{col}}) %>% sort %>%
  #   {ggplot(tibble(index = seq_along(.), value = .),
  #          aes(index, value))+
  #   geom_point(alpha = 0.4)}

  data %>%
    select({{col}}) %>%
    drop_na({{col}}) %>%
    arrange({{col}}) %>%
    mutate(index = seq_len(nrow(.))) %>%
    ggplot(aes(index, {{col}}))+
    geom_point(alpha = 0.5)

}



#' pull-sort-plot string input
#'
#' might be easier to use with map
#'
#' @param data df
#' @param col name
#'
#' @return ggplot
#' @export
#'
#' @examples pspl(iris, "Sepal.Length")
pspls <- function(data, col) {

  data %>%
    select(all_of(col)) %>%
    drop_na(all_of(col)) %>%
    arrange(.data[[col]]) %>%
    mutate(index = seq_len(nrow(.))) %>%
    ggplot(aes_string("index", col))+
    geom_point(alpha = 0.5)
}


#' Export report
#'
#' Mostly for internal use. Does the page layout and exports figures.
#'
#' @param plots list of ggplots
#' @param file path
#' @param plots_per_page 24
#'
#' @return NULL
#' @export
#' @importFrom ggplot2 ggsave
export_report <- function(plots, file, plots_per_page = 24) {

  pl_c <- floor(sqrt(plots_per_page * 0.75))

  pl_r <- plots_per_page / pl_c

  splots <- split(plots, ceiling(seq_along(plots)/plots_per_page))

  sp2 <- purrr::map(splots, ~ cowplot::plot_grid(plotlist = .x, nrow = pl_r, ncol = pl_c))

  ggplot2::ggsave(filename = file,
         plot = gridExtra::marrangeGrob(grobs = sp2, nrow=1, ncol=1, left = "BAS", bottom = "time"),
         width = 210, height = 297,
         units = "mm")

  NULL

}
