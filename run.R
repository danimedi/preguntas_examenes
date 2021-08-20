# Extract questions and save them into a PDF file
source("src/get_questions.R")
questions <- extract_questions("data/test_dummy.pdf")
write_questions(questions, "output/output_dummy.pdf")
