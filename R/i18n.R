#' @export
i18_dict <- R6::R6Class(
  "i18_dict",
  public = list(
    initialize = function(x) {
      i18_check(x)
      private$dict <- hash_df(x)
    },
    as.data.frame = function() unhash_df(private$dict),
    translate = function(key, language, allow_missing = FALSE) {
      stopifnot(is.scalar(key),
                is.scalar(language),
                is.scalar.logical(allow_missing))
      key <- as.character(key)
      val <- private$dict[[key]][[language]]
      if (is.null(val)) {
        if (allow_missing)
          val <- key else
            stop(sprintf("no translation found for '%s' in language '%s'",
                         key, language))
      }
      val
    }
  ),
  private = list(dict = NULL)
)

# Checks the validity of the dictionary
i18_check <- function(x) {
  if (!is.data.frame(x))
    stop("input to i18n_dict() must be a dataframe ")
  if (!is.character(x$key))
    stop("input to i18n_dict() must have a character column called 'key")
  if (sum(names(x) == "key") > 1)
    stop("input to i18n_dict() must have exactly one character column called 'key")
  if (!all(sapply(x, is.character)))
    stop("all columns of input to i18n_dict() must be character class")
}
