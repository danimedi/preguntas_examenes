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
  
  # "MIR" is used to separate questions
  dat <- pdf_read_and_filter(
    pdf_input,
    height == 10 | (height == 11 & text == "MIR")
  )
  
  # Split by questions
  i_beginning <- which(
    dat$text == "MIR" &
      dat$height == 11 &
      str_detect(frameshift(dat$text, offset = -1), "^\\d+[.]$")
  )
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
