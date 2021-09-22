#' Sample questions randomly by blocks to create a question bank
#' 
#' Get questions with their answers randomly in blocks of a certain length.
#'
#' @param question_list List with the information of the questions and answers.
#' @param block_size Number of questions in each block.
#' @param seed Set a seed to reproduce the random sampling of the questions.
#'
#' @return A list with elements for each block containing the information of
#'   questions and answers.
#' @export
get_question_bank <- function(question_list, block_size = 40, seed = 1) {
  set.seed(seed)
  
  question_list <- unlist(question_list, use.names = FALSE, recursive = FALSE)
  question_list <- sample(question_list)
  
  n_questions <- length(question_list)
  i <- seq.int(from = 1, to = n_questions, by = block_size)
  i <- cut(
    seq_len(n_questions), c(-Inf, i, Inf),
    labels = FALSE, right = FALSE, include.lowest = TRUE
  )
  question_bank <- split(question_list, i)
  question_bank
}


#' Save the question bank as PDF files
#' 
#' Create PDF files for the blocks contained in the question bank generated
#' using the function [get_question_bank()].
#'
#' @param question_bank List with question and answers for each block,
#'   output of the function [get_question_bank()].
#' @param dir_output Path to the directory where the PDF files will be saved.
#'
#' @export
write_question_bank <- function(question_bank, dir_output) {
  k <- 1
  for (block in question_bank) {
    prefix <- formatC(k, width = 3, format = "d", flag = "0")
    k <- k + 1
    
    write_list_safe <- function(x, filename) {
      if (!basename(filename) %in% list.files(dir_output)) {
        tryCatch(
          write_list(x, filename),
          error = function(cnd) {
            warning(
              "File ", basename(filename), ": ",
              "Problems writing the PDF"
            )
          }
        )
      }
    }
    
    questions <- lapply(block, \(x) x$question)
    filename <- file.path(dir_output, paste0(prefix, "_questions.pdf"))
    write_list_safe(questions, filename)
    
    answers <- lapply(block, \(x) x$answer)
    filename <- file.path(dir_output, paste0(prefix, "_answers.pdf"))
    write_list_safe(answers, filename)
  }
}

