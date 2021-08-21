library(dplyr)
library(pdftools)


#' Get and filter the information of a PDF file as a data frame
#' 
#' Get, filter, sort and collapse the data set of the text obtained from a
#' PDF file.
#'
#' @param pdf_input Path to the PDF file.
#'
#' @return A data frame with the information of the text from the PDF file.
#' @export
pdf_read_and_filter <- function(pdf_input, ...) {
  
  dat_full <- pdf_data(pdf_input)
  
  filter_questions <- function(df) {
    filter(df, ...)
  }
  dat <- lapply(dat_full, filter_questions)
  
  # Split columns
  split_columns <- function(df, x_limit = 295) {
    left <- filter(df, x < x_limit)
    right <- filter(df, x >= x_limit)
    list(left, right)
  }
  
  # Sort values within each column
  sort_column <- function(df) {
    arrange(df, y, x)
  }
  
  split_sort_join <- function(df_page) {
    res <- split_columns(df_page)
    res <- lapply(res, sort_column)
    do.call(bind_rows, res)
  }
  dat <- lapply(dat, split_sort_join)
  dat <- do.call(bind_rows, dat)
  dat
}
