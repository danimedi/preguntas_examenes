library(stringr)
library(dplyr)


#' Extract answers from the PDF file
#' 
#' Extract the text of the answers in the PDF file and return a list of
#' data frames with the text for all the answers.
#'
#' @param pdf_input Path of the PDF file.
#'
#' @return A list of data frames with the text of each answer.
#' @export
extract_answers <- function(pdf_input) {
  
  dat <- pdf_read_and_filter(
    pdf_input,
    height == 11 | (height == 10 & str_detect(text, "^\\d+[.]$"))
  )
  
  # Split by questions
  i_beginning <- which(
    dat$height == 10 &
      frameshift(dat$height, offset = 1) == 11
  )
  dat <- split_by_index(dat, i_beginning, right = FALSE)
  dat <- dat[-1]  # The first data set does not contain question information
  
  question_number <- lapply(dat, function(df) df$text[1])
  answer <- lapply(dat, function(df) {
    i <- which(df$text == "Respuesta:")
    df$text[i + 1]
  })
  
  dat <- lapply(dat, function(df) filter(df, height == 11))
  dat <- lapply(dat, function(df) {
    i <- which(df$text == "Respuesta:")
    df[seq_len(i-1), ]
  })
  
  collapse_to_text <- function(df) {
    paste0(df$text, collapse = " ")
  }
  dat <- lapply(seq_along(dat), function(i) {
    text <- collapse_to_text(dat[[i]])
    paste0(
      "_", question_number[[i]], " ",
      text, "\n\n",
      "Respuesta: *", answer[[i]], "*"
    )
  })
  dat
}
