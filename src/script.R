library(dplyr)
library(pdftools)
library(ggplot2)
library(stringr)

dat_full <- pdf_data("small.pdf")

get_questions_and_answers <- function(df, limit = 295) {
  filter_questions <- function(df) {
    # The questions have a font size of 10 (height == 10)
    # We use the text "MIR" to separate answers (check later)
    filter(df, height == 10 | (height == 11 & text == "MIR"))
  }
  res <- lapply(df, filter_questions)
  
  # ANOTHER OPTION IS USING ANOTHER MORE COMPLEX ALGORITHM TO SORT USING
  # BOTH COORDINATES, X AND Y
  
  # Distinguish between left and right columns by the coordinates
  sort_by_column <- function(df, x_limit) {
    bind_rows(filter(df, x < x_limit), filter(df, x >= x_limit))
  }
  res <- lapply(res, sort_by_column, x_limit = limit)
  res <- do.call(bind_rows, res)
  res
}
dat <- get_questions_and_answers(dat_full)

# Make a graph
check_limit <- function(df, limit) {
  df <- do.call(bind_rows, df)
  df <- mutate(df, column = if_else(x < limit, "left", "right"))
  ggplot(df) +
    geom_boxplot(aes(column, x)) +
    geom_hline(yintercept = limit, color = "red")
}
check_limit(dat, limit = 295)

# Separate questions using the "MIR" text, followed by a number (this is the
# beginning of a question)
frameshift <- function(x, offset) x[((1:length(x))-1-offset) %% length(x)+1]
i_beginning <- which(
  dat$text == "MIR" &
    dat$height == 11 &
    str_detect(frameshift(dat$text, offset = -1), "^\\d+[.]$")
)
dat <- lapply(seq_along(i_beginning), function(i) {
  if (i != length(i_beginning)) {
    dat[i_beginning[[i]]:(i_beginning[[i+1]]-1), ]
  } else {
    dat[i_beginning[[i]]:nrow(dat), ]
  }
})

# Some questions have the choices disconnected, this means that there is a "MIR"
# between question and choices
check_choices <- function(df) {
  all(c("1.", "2.", "3.") %in% df$text)
}
x <- lapply(dat, check_choices)
i <- which(!unlist(x))
join_missing_choices <- function(df_list, i) {
  for (j in i) {
    df_list[[j]] <- bind_rows(df_list[j], df_list[j+1])
    df_list[[j+1]] <- NULL
  }
  df_list
}
dat <- join_missing_choices(dat, i)

# Now we do not need the "MIR" text anymore
dat <- lapply(dat, function(df) filter(df, height == 10))


split_question <- function(df) {
  i <- which(str_detect(df$text, "^1[.]$"))[[1]]
  question <- df[1:(i-1), ]
  choices <- df[i:nrow(df), ]
  i <- str_detect(choices$text, "^\\d+[.]$")
  choices <- choices[!i, ]
  list(question, choices)
}
split_question(dat[[8]])
lapply(dat, split_question)


# SEPARATE THE CHOICES, NOTICE "." AT THE END AND `space` is FALSE AT THE END OF
# THE CHOICE
split_choices <- function(df) {
  i <- which(str_detect(df$text, "^.*[.]$") & !df$space)
  lapply(seq_along(i), function(j) {
    if (j == 1) {
      df[1:i[[j]], ]
    } else {
      df[(i[[j-1]]+1):i[[j]], ]
    }
  })
}
split_choices(split_question(dat[[1]])[[2]])


# COLLAPSE TEXT IN ROWS TO PLAIN TEXT
collapse_to_text <- function(x) {
  str_c(x, collapse = " ")
}


# REMOVE THE HYPHEN

