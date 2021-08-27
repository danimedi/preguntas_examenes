for (file in list.files("src", full.names = TRUE)) source(file)


# Split book into chapters -----------------------------
split_by_chapters("data/questions.pdf", dir_output = "data/chapters")


# Extract questions and answers from PDF ---------------------------
questions <- extract_questions("data/test_dummy.pdf")
answers <- extract_answers("data/test_dummy.pdf")
# write_list(questions, "output/test_dummy.pdf")

for (file in list.files("data/chapters", full.names = TRUE)) {
  tryCatch({
    res <- get_questions_and_answers(file)
    file_name <- sub("(.*)\\..*$", "\\1", basename(file)) |>
      paste0(".rds") |>
      {\(x) file.path("output", x)}()
    saveRDS(res, file_name)
  }, error = \(cnd) print(paste("Error in", file)))
}

