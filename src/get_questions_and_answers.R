#' Get a list with the questions and answers from a PDF file with questions
#' 
#' Return a list with questions and answers from a chapter of the PDF file of
#' the questions.
#'
#' @param pdf_input Path of the PDF file (chapter).
#'
#' @return A list for each question containing the information of the question
#'   and the answer.
#' 
#' @export
get_questions_and_answers <- function(pdf_input) {
  questions <- extract_questions(pdf_input)
  answers <- extract_answers(pdf_input)
  if (length(questions) == length(answers)) {
    lapply(seq_along(questions), function(i) {
      list(
        question = questions[[i]],
        answer = answers[[i]]
      )
    })
  } else {
    stop("Lengths of questions and answers are different")
  }
}
