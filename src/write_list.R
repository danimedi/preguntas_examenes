#' Generate a PDF file from a list of texts
#' 
#' Used to save the outuput of functions like [extract_questions()] and
#' [extract_answers()] into a PDF file.
#'
#' @param text_list List of character vectors.
#' @param pdf_output Path to the PDF file.
#'
#' @export
write_list <- function(text_list, pdf_output) {
  # Collapse into text
  text <- paste0(text_list, collapse = "\n\n\n")
  
  create_pdf <- function(text, output_file) {
    temp_file <- gsub("[.]pdf$", ".Rmd", output_file)
    options(encoding = "UTF-8")
    cat(text, file = temp_file, sep = "\n\n\n")
    render(temp_file, pdf_document())
    file.remove(temp_file)
  }
  create_pdf(text, pdf_output)
}
