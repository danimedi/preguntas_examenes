for (file in list.files("src", full.names = TRUE)) source(file)


# Split book into chapters -----------------------------
split_by_chapters("data/questions.pdf", dir_output = "data/chapters")


# Extract questions and answers from PDF ---------------------------
questions <- extract_questions("data/test_dummy.pdf")
answers <- extract_answers("data/test_dummy.pdf")

write_list(questions, "output/test_dummy.pdf")

if (length(questions) == length(answers)) {
  questions_and_answers <- lapply(seq_along(questions), function(i) {
    list(
      question = questions[[i]],
      answer = answers[[i]]
    )
  })
  # Save this into a file
  saveRDS(questions_and_answers, "output/output_dummy.rds")
}

