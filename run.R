for (file in list.files("src", full.names = TRUE)) source(file)


# Split book into chapters -----------------------------
split_by_chapters("data/questions.pdf", dir_output = "data/chapters")


# Extract questions and answers from PDF ---------------------------
questions <- extract_questions("data/test_dummy.pdf")
answers <- extract_answers("data/test_dummy.pdf")
# write_list(questions, "output/test_dummy.pdf")

chapters_paths <- list.files("data/chapters", full.names = TRUE)
full_info <- lapply(chapters_paths, get_questions_and_answers)
names(full_info) <- sub("(.*)\\..*$", "\\1", basename(chapters_paths))
saveRDS(full_info, "data/full_info.rds")


# Get and write question bank ------------------------
full_info <- readRDS("data/full_info.rds")
question_bank <- get_question_bank(full_info)
write_question_bank(question_bank, "output")

