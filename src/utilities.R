
# Frameshift a vector (upwards or downwards)
frameshift <- function(x, offset) {
  x[((1:length(x))-1-offset) %% length(x)+1]
}


# Split a data frame by rows using an index (of the rows)
split_by_index <- function(df, i, right) {
  j <- cut(
    seq_len(nrow(df)),
    breaks = c(-Inf, i, Inf),
    include.lowest = TRUE,
    labels = FALSE,
    right = right
  )
  split(df, j)
}
