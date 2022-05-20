


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

  ggplot(data, aes({{x}}, {{y}}))+
    geom_point()+
    geom_abline(slope = 1)+
    annotate(geom = "text", x = -Inf, y = Inf,
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
