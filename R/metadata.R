#' coldesc
#'
#' @param df data frame
#'
#' @return a descriptive table of the columns
#' @export
coldesc <- function(df){
  Meta <- tibble(Column_names=names(df))
  #datatype
  Meta$Datatype <- sapply(df, typeof)
  Meta$Dataclass <- sapply(df, class)
  #if numeric, range. if factor, number of levels
  #TODO write with dplyr
  Meta$Range <- NA
  for (i in 1:nrow(Meta)) {
    if (Meta$Dataclass[i]=="numeric" | Meta$Dataclass[i]=="integer") {
      Meta$Range[i] <- paste0(signif(range(df[i], na.rm=TRUE), digits = 4), collapse = " - ")
    } else if (Meta$Dataclass[i]=="factor" | Meta$Dataclass[i]=="character") {
      Meta$Range[i] <- paste(length(unique(df[[i]])), "levels")
    } else {Meta$Range[i] <- NA}
  }
  Meta$Perc_complete <- round(colSums(!is.na(df))/nrow(df)*100, 1)
  return(Meta)
}
