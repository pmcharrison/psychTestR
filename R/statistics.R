# statistics.analyse_participation <- function(opt) {
#   df <- statistics.tabulate_files(opt)
#   list(
#     num_all <- nrow(df),
#     num_complete <- sum(df$complete),
#     num_incomplete <- sum(!df$complete)
#   )
#
#   warning("p_id may only contain alphanumerics and underscores, and maxchar")
#
# }
#
# # "id=1&p_id=PeterHarrison&save_id=1&final=true.rds"

get_results_files <- function(opt, full.names = FALSE, target_path = tempfile("dir")) {
  opt$repository$get_folder("results", target_path)
  pattern <- "^id=.*\\.rds$"
  list.files(target_path, pattern = pattern, full.names = full.names)
}

