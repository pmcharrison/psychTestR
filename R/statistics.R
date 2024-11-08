# statistics.analyse_participation <- function(opt) {
#   df <- statistics.tabulate_files(opt)
#   list(
#     num_all <- nrow(df),
#     num_complete <- sum(df$complete),
#     num_incomplete <- sum(!df$complete)
#   )
#
#   warning("`p_id` may only contain alphanumerics and underscores, and maxchar")
#
# }
#
# # "id=1&p_id=PeterHarrison&save_id=1&final=true.rds"

list_results_files <- function(results_dir, full.names = FALSE) {
  pattern <- "^id=.*\\.rds$"
  list.files(results_dir, pattern = pattern, full.names = full.names)
}

tabulate_results <- function(opt, include_pilot) {
  stopifnot(is.scalar.logical(include_pilot))
  df <- data.frame(file = list_results_files(opt$results_dir),
                   stringsAsFactors = FALSE)
  cols <- c("id", "p_id", "save_id", "pilot", "complete")
  if (nrow(df) > 0L) {
    df <- tidyr::extract(
      df, "file", cols,
      "(?:id=)([[0-9]]*)(?:&p_id=)([[A-Za-z0-9_]]*)(?:&save_id=)([[0-9]]*)(?:&pilot=)([[a-z]]*)(?:&complete=)([[a-z]]*)",
      remove = FALSE)
  } else {
    for (col in cols) df[[col]] <- character()
  }
  for (col in c("pilot", "complete"))
    df[[col]] <- as.logical(toupper(df[[col]]))
  for (col in c("id", "save_id"))
    df[[col]] <- as.integer(df[[col]])
  for (col in c("p_id"))
    df[[col]] <- as.character(df[[col]])
  if (!include_pilot) df <- df[!df$pilot, , drop = FALSE]
  df$full_file <- file.path(opt$results_dir, df$file)
  df
}
