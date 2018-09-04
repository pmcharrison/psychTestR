# Converts a data.frame into a hash table.
hash_df <- function(x, markdown) {
  stopifnot(is.scalar.logical(markdown))
  if (!is.data.frame(x))
    stop("input must be a dataframe ")
  if (!is.character(x$key))
    stop("input must have character column called 'key")
  y <- new.env()
  for (i in seq_len(nrow(x))) {
    key <- x$key[i]
    value <- as.list(x[i, ])
    value$key <- NULL
    if (markdown) value <- lapply(value, function(x) {
      x <- gsub("\\\\", "\n\n", x)
      markdown::markdownToHTML(text = x, fragment.only = TRUE)
    })
    y[[key]] <- value
  }
  y
}

# Converts a hash table, as created by hash_df(), into a data.frame.
unhash_df <- function(x) {
  y <- as.list(x)
  keys <- names(y)
  values <- lapply(y, function(x) as.data.frame(x, stringsAsFactors = FALSE)) %>%
    do.call(rbind, .)
  row.names(values) <- NULL
  cbind(key = keys, values, stringsAsFactors = FALSE)
}
