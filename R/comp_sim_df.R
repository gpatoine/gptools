#functions to compare two similar dataframes. Used to check import errors and such.
#Care: doesn't compare NA values
#source("C:/Users/gp63dyte/Dropbox/iDiv/R tips/functions/comp_sim_df.R")


#' comp_sim_df
#'
#' @param df1
#' @param df2
#'
#' @return dataframe with details about differences
#' @export
comp_sim_df <- function(df1, df2){
  #sqldf
  #df1only and df2only are reordered automatically by... first columns?
  #so the order of the first columns need to be maintained, which is a big drawback
  df1only <- sqldf('SELECT * FROM df1 EXCEPT SELECT * FROM df2')

  df2only <- sqldf('SELECT * FROM df2 EXCEPT SELECT * FROM df1')
  print(paste(nrow(df1only), "and", nrow(df2only), "different rows"))
  if(nrow(df1only)+nrow(df2only)==0) return(NULL)
  #add column: wrongcol
  df1only$wrongcol <- NA
  for (i in 1:nrow(df1only)) {

    #snippets not needed anymore
    #df1only %>% select(-last_col())
    #all.equal((df1only %>% select(-last_col()) %>% slice(i)), df2only %>% slice(i))
    #df1only[which(isFALSE(df1only[i,-ncol(df1only)] == df2only[i,]))])

    mismatch <- sapply(seq_along(df2only), function(x) identical(df1only[i,x], df2only[i,x]))
    df1only$wrongcol[i] <- list(names(df2only)[!mismatch])
  }

  #new df: one line per site, per unique(wrongcol), site, wrongcol, value1, value2
  DiffIssues <- unique(df1only %>% select(Site_ID, wrongcol))
  #TODO fix this: unique is an issue if there are many rows with same Site_ID and wrongcol
  DiffIssues$value1 <- NA
  DiffIssues$value2 <- NA

  #retrieve values from original two dataframes, using row names
  for (i in 1:nrow(DiffIssues)) {
    DiffIssues$value1[i] <- df1only[row.names(DiffIssues[i,]),DiffIssues$wrongcol[[i]][1]]
    DiffIssues$value2[i] <- df2only[row.names(DiffIssues[i,]),DiffIssues$wrongcol[[i]][1]]
    #print(i)
  }
  return(DiffIssues)
}


# df1$`Sample post processing`
#
# df2NotIndf1[as.numeric(row.names(DiffIssues[i,])),""]
# df2NotIndf1[as.numeric(row.names(DiffIssues[i,])),DiffIssues$wrongcol[[i]][1]]



# Second approach, cell by cell ----------------------------------------------------

#' comp_for
#'
#' Goes cell by cell, for all columns and rows
#' One issue is that the dfs need to have the same row (and column) order.
#' dplyr::arrange may not always solve the problem, and so sqldf provides a good alternative,
#' but doesn't give the row number.
#'
#' @param df1
#' @param df2
#' @param id unique id column name maintained between dataframes
#'
#' @return
#' @export
comp_for <- function(df1, df2, id = NULL) {
  if(!identical(dim(df1), dim(df2))) stop("dfs of different dimensions")

  diffs <- NULL

  for (icol in seq_len(length(df1))) {
    # icol <- 11
    # print(icol)
    for (irow in seq_len(nrow(df1))) {
      # irow <- 9
      # print(irow)
      if(!identical(df1[irow, icol], df2[irow, icol])){

        one_diff <- tibble(colnum = as.integer(icol),
                           colname = names(df1)[icol],
                           rownum = as.integer(irow),
                           val1 = as.character(df1[irow, icol]),
                           val2 = as.character(df2[irow, icol]))

        diffs <- diffs %>% bind_rows(one_diff)
      }
    }
  }

  #Note: id is taken from df1. important if id itself changed between dfs
  if(!is.null(id)){
    diffs2 <- diffs %>% mutate(id = df1[rownum, id, drop = TRUE]) %>%
      select(rownum, colnum, id, colname, val1, val2)
  }

  diffs2
}


# Third approach ----------------------------------------------------------

#' dupsBetweenGroups
#'
#' http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/
#'
#' @param df
#' @param idcol
#'
#' @return
#' @export
dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to

  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)

  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]

  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)

  # With duplicates within each group filtered out, find duplicates between groups.
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]

  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently

  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)

  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])

  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1

  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]

  # Undo the original sort
  dupBetween[sortorder] <- dupBetween

  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}

#' comp_btw
#'
#' @param df1
#' @param df2
#' @param id character
#'
#' @return
#' @export
comp_btw <- function(df1, df2, id = NULL) {
  df1 <- df1 %>% mutate(tag_btw = "one", inner_row_id = seq_len(nrow(df1)))
  df2 <- df2 %>% mutate(tag_btw = "two", inner_row_id = seq_len(nrow(df2)))

  both <- bind_rows(df1, df2)
  copy_both <- both

  dupRows <- dupsBetweenGroups(both %>% select(-inner_row_id), idcol = "tag_btw")

  #returns df of differences, i.e. rbind(df1only, df2only) with more data
  diffs <- both[!dupRows,]

  list_mismatch <- function(df, irow) {
    subdf <- df[df$inner_row_id == irow,] %>% select(-tag_btw)

    # check two rows
    if (nrow(subdf) != 2) {
      stop("Wrong nrow in subdf. Row number is ", irow)
    }

    # check same id
    if ((!is.null(id)) && (subdf[1, id] != subdf[2, id])) {
      stop("Different id values in subdf. Row number is ", irow)
    }

    mismatch <- map_lgl(seq_along(subdf), ~ identical(subdf[1,.x], subdf[2,.x]))
    names(subdf)[!mismatch] # %>% toString #could remove toString to have a list, and change map_chr below
  }

  diffs <- diffs %>% mutate(wrongcol = map(inner_row_id, ~ list_mismatch(diffs, .x))) #map_chr

  diffs %>% select(tag_btw, inner_row_id, wrongcol, everything()) %>% arrange(inner_row_id)
}


# more note: For sure, if the order is different, there is no way to say which
# rows are supposed to match. We can they only print the full rows.
# There are therefore two functions, one for ordered rows with more details, one without.
#
#

# examples ----------------------------------------------------------------

# library(tidyverse)
# library(here)
#
# source(here("functions/comp_sim_df.R"))
#
# guess_encoding(read_lines_raw(here("data/soil_3_12_skr_20190826.csv")))
#
# soil_mod <- read_csv(here("data/old_soil_Tcomp/5-soil_3_12_gp_20200630.csv"),
#                      na = c("NA", "", "n.a.", "na", "not available", "not measured",
#                             "-"),
#                      col_types = cols(.default = col_character()),
#                      locale = locale(encoding = "UTF-8")) %>%
#   arrange(Site_ID, Treatment, Plot, Year, Month, Day, Sample_ID,
#           Soil_Horizon_or_Tea_or_Litter_type, Layer_depth__start_, Layer_depth__end_,
#           Silt, Clay, Sand, pH_CaCl2, TOC, Total_N)
#
# guess_encoding(read_lines_raw(here("data/old_soil_Tcomp/5-soil_3_12.csv")))
#
# soil_orig <- read_csv(here("data/old_soil_Tcomp/5-soil_3_12.csv"),
#                       na = c("NA", "", "n.a.", "na", "not available", "not measured",
#                              "-"),
#                       col_types = cols(.default = col_character()),
#                       locale = locale(encoding = "ISO-8859-2")) %>%
#   arrange(Site_ID, Treatment, Plot, Year, Month, Day, Sample_ID,
#           Soil_Horizon_or_Tea_or_Litter_type, Layer_depth__start_, Layer_depth__end_,
#           Silt, Clay, Sand, pH_CaCl2, TOC, Total_N)
#
# #metadata(soil_mod) %>% View
#
# mod1 <- soil_mod %>% select(-Site_name, -Treatment)
# ori1 <- soil_orig %>% select(-Site_name, -Treatment)
#
#
# tc_comp_sim <- comp_sim_df(df1 = ori1, df2 = mod1)# %>% View(title = "diff_old")
# map_dbl(tc_comp_sim$wrongcol, length) %>% sum #62
#
# tc_for <- comp_for(df1 = ori1, df2 = mod1, id = "Site_ID")
#
# tc_btw <- comp_btw(ori1, mod1) #, id = "Site_ID"
# map_dbl(tc_btw$wrongcol, length) %>% sum/2 #95
#
# all(mod1$Plot %in% ori1$Plot)
# all(ori1$Plot %in% mod1$Plot)
#
# id_for <- tc_for$id %>% unique %>% sort
# id_sim <- tc_comp_sim$Site_ID %>% unique %>% sort
# id_btw <- tc_btw$Site_ID %>% unique %>% sort
#
# all.equal(id_sim, id_for)
#
# all(id_sim %in% id_btw)
# id_btw[!id_btw %in% id_sim]
