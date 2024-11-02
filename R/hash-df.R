# Converts a data.frame into a hash table.
hash_df <- function(x, markdown) {
  stopifnot(is.scalar.logical(markdown))
  if (!is.data.frame(x))
    stop("input must be a `data.frame` ")
  if (!is.character(x$key))
    stop("input must have character column called `key`")
  y <- new.env()
  for (i in seq_len(nrow(x))) {
    key <- enc2utf8(x$key[i])
    value <- as.list(x[i, ])
    value$key <- NULL
    value <- lapply(value, enc2utf8)
    if (markdown) value <- lapply(value, parse_markdown)
    y[[key]] <- value
  }
  y
}

parse_markdown <- function(x) {
  stopifnot(is.scalar.character(x))
  has_paragraphs <- grepl("(\\\\\\\\)|(<p>)|(\\n)", x)
  if (has_paragraphs)
    x <- gsub("\\\\", "\n\n", x, fixed = TRUE)
  #message("markdownToHTML version")
  #res <- enc2utf8(markdown::markdownToHTML(text = x, fragment.only = T, encoding = "UTF_8"))
  #message("renderMarkdown version")
  res <- markdown::mark(
    file = NULL,
    output = NULL,
    text = x,
    format = "html"
  )

  if (!has_paragraphs)
    res <- gsub("(<p>)|(</p>)|(\\n)", "", res)
  res
}

# Converts a hash table, as created by hash_df(), into a data.frame.
unhash_df <- function(x) {
  y <- as.list(x)
  keys <- names(y)
  values <- lapply(y, function(x) as.data.frame(x, stringsAsFactors = FALSE)) %>%
    do.call(rbind, .)
  row.names(values) <- NULL
  res <- cbind(key = keys, values, stringsAsFactors = FALSE)
  res <- res[order(res$key), ]
  row.names(res) <- NULL
  res
}
