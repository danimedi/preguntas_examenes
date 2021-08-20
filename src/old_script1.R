library(tabulizer)
library(stringr)
library(rmarkdown)

text <- extract_text("small.pdf")
questions <- str_split(text, "MIR \\d{4}\r\n")[[1]]
questions <- questions[-1]

get_questions <- function(questions) {
  questions %>%
    str_extract("(.|\r|\n)*(\r\n\\d[.].*)") %>%
    str_remove("^\\d+[.] ")
}
questions <- get_questions(questions)

fix_subtitles <- function(questions) {
  questions %>%
    str_remove("\r\nTema \\d+[.](\n|\r|.)*?(?=\r\n\\d+[.] )")
}
questions <- fix_subtitles(questions)

collapse_new_lines <- function(questions) {
  questions %>%
    str_replace_all("\r\n", "\n") %>%
    str_replace("\n1.", "\n\n1.")  # Extra space before alternatives
}
questions <- collapse_new_lines(questions)

fix_line_breaks <- function(questions) {
  questions %>%
    str_remove_all("-\n")
}
questions <- fix_line_breaks(questions)

create_pdf <- function(questions, output_file) {
  temp_file <- gsub("[.]pdf$", ".Rmd", output_file)
  options(encoding = "UTF-8")
  cat(questions, file = temp_file, sep = "\n\n\n")
  render(temp_file, pdf_document())
  file.remove(temp_file)
}
create_pdf(questions, "my_text.pdf")

# There is a problem removing some parts with the explanation of things
# included in some questions

