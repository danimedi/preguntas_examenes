for (file in list.files("src", full.names = TRUE)) source(file)

# Extract questions and save them into a PDF file
questions <- extract_questions("data/test_dummy.pdf")
write_list(questions, "output/output_dummy.pdf")

# Extract answers of the same questions
answers <- extract_answers("data/test_dummy.pdf")
