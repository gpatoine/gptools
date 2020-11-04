
#' make_flag_col
#'
#' @param df
#'
#' @return
#' @export
make_flag_col <- function(df) {
  df <- tibble::as.tibble(df)
  df$flag <- as.list(rep(NA_character_, nrow(df)))
  df
}



#' add_flag
#'
#' Only to be used with Cmic dataset with a New_ID column
#'
#' @param df The dataframe
#' @param id
#' @param new_flag
#'
#' @return
#' @export
add_flag <- function(df, id, new_flag){

  if (length(id) > 1) {
    walk(id, ~ add_flag(df, .x, new_flag))

  } else {

    # append if not already in
    app_flag <- function(current, app){
      if(!app %in% current) c(current, app) else current
    }

    cur_flag <- df$flag[df$New_ID == id][[1]]

    if (length(cur_flag) == 1 && is.na(cur_flag)) {
      df$flag[df$New_ID == id][[1]] <- new_flag
    } else {
      df$flag[df$New_ID == id][[1]] <- app_flag(current = cur_flag, app = new_flag)
    }
  }
  df

}


#' get_flag
#'
#' @param df
#' @param id
#'
#' @return
#' @export
get_flag <- function(df, id) {
  df$flag[df$New_ID == id][[1]]
}
