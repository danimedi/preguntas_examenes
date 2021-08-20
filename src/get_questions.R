library(pdftools)
library(dplyr)
library(stringr)
library(rmarkdown)


#' Extract questions from the PDF file
#' 
#' Extract the text of the questions in the PDF file and split the questions
#' to return a list of data frames with the text for all the questions and
#' the choices present in the PDF file.
#'
#' @param pdf_input Path of the PDF file.
#'
#' @return A list of data frames with the text of each question.
#' @export
extract_questions <- function(pdf_input) {
  
  dat_full <- pdf_data(pdf_input)
  
  # Deal with pages and columns to create a unified data set -------------
  
  filter_questions <- function(df) {
    filter(df, height == 10 | (height == 11 & text == "MIR"))
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
  
  # Identify questions and choices and collapse into text -----------------
  
  # Split by questions
  frameshift <- function(x, offset) {
    x[((1:length(x))-1-offset) %% length(x)+1]
  }
  i_beginning <- which(
    dat$text == "MIR" &
      dat$height == 11 &
      str_detect(frameshift(dat$text, offset = -1), "^\\d+[.]$")
  )
  split_by_index <- function(df, i, right) {
    j <- cut(
      seq_len(nrow(df)),
      breaks = c(-Inf, i, Inf),
      include.lowest = TRUE,
      labels = FALSE,
      right = right
    )
    split(df, j)
  }
  dat <- split_by_index(dat, i_beginning, right = FALSE)
  
  # "MIR" text no longer needed
  dat <- lapply(dat, function(df) filter(df, height == 10))
  
  # Split questions in question and choices and collapse those into text
  split_question <- function(df) {
    i <- str_which(df[-1, ]$text, "^1[.]$")[[1]] + 1
    question <- df[1:(i-1), ]
    choices <- df[i:nrow(df), ]
    list(question, choices)
  }
  split_choices <- function(df) {
    i <- which(str_detect(df$text, "^\\d+[.]$") & !frameshift(df$space, offset = 1))
    split_by_index(df, i, right = FALSE)
  }
  collapse_to_text <- function(df) {
    paste0(df$text, collapse = " ")
  }
  split_and_collapse_question <- function(df_question) {
    x <- split_question(df_question)
    question <- x[[1]]
    choices <- x[[2]]
    choices <- split_choices(choices)
    choices <- lapply(choices, collapse_to_text)
    paste0(
      "_", collapse_to_text(question), "\n\n",
      paste0(choices, collapse = "\n")
    )
  }
  dat <- lapply(dat, split_and_collapse_question)
  dat
}



#' Save the text of the questions in a list to a PDF file
#' 
#' Save the output of the function [extract_questions()] as a PDF file.
#'
#' @param df_list List obtained from the function [extract_questions()].
#' @param pdf_output Path to the output PDF file.
#'
#' @export
write_questions <- function(df_list, pdf_output) {
  # Collapse into text
  text <- paste0(df_list, collapse = "\n\n\n")
  
  create_pdf <- function(text, output_file) {
    temp_file <- gsub("[.]pdf$", ".Rmd", output_file)
    options(encoding = "UTF-8")
    cat(text, file = temp_file, sep = "\n\n\n")
    render(temp_file, pdf_document())
    file.remove(temp_file)
  }
  create_pdf(text, pdf_output)
}
