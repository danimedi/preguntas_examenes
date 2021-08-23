library(pdftools)


#' Split the PDF book into chapters
#' 
#' Generate PDF files for each of the chapters of the book "Libro gordo" that
#' contains the questions.
#'
#' @param pdf_book Path to the PDF file (book).
#' @param dir_output Path to the directory which will contain the PDF files of
#'   the chapters.
#'
#' @export
split_by_chapters <- function(pdf_book, dir_output) {
  pages <- seq_len(824)
  i <- cut(
    pages,
    breaks = c(
      -Inf,
      13, 85, 103, 187, 229, 299, 339, 337, 433, 447, 507,
      545, 605, 647, 663, 679, 711, 741, 777, 803, 822,
      Inf
    ),
    labels = FALSE,
    right = FALSE
  )
  chapters_pages <- split(pages, i)
  # First and last part does not contain useful information
  chapters_pages <- chapters_pages[c(-1, -length(chapters_pages))]
  
  chapters_names <- c(
    "cardiología y cirugía cardiovascular", "dermatología",
    "digestivo y cirugía general", "endocrinología", "estadística y epidemiología",
    "ginecología y obstetricia", "hematología", "infecciosas y microbiología",
    "inmunología", "miscelánea", "nefrología", "neumología y cirugía torácica",
    "neurología y neurocirugía", "oftalmología", "otorrinolaringología",
    "pediatría", "psiquiatría", "reumatología", "traumatología y cirugía ortopédica",
    "urología"
  )
  
  for (i in seq_along(chapters_pages)) {
    file_name <- file.path(dir_output, paste0(chapters_names[[i]], ".pdf"))
    pdf_subset(pdf_book, pages = chapters_pages[[i]], output = file_name)
  }
}
