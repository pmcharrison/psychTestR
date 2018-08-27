#' @export
i18n_dict <- R6::R6Class(
  "i18n_dict",
  public = list(
    initialize = function(x) {
      i18n_check(x)
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
i18n_check <- function(x) {
  if (!is.data.frame(x))
    stop("input to i18n_dict() must be a dataframe ")
  if (!is.character(x$key))
    stop("input to i18n_dict() must have a character column called 'key")
  if (sum(names(x) == "key") > 1)
    stop("input to i18n_dict() must have exactly one character column called 'key")
  if (!all(sapply(x, is.character)))
    stop("all columns of input to i18n_dict() must be character class")
}

i18n_global_dict <- R6::R6Class(
  "i18n_global_dict",
  public = list(
    initialize = function(x) private$val <- NULL,
    get = function() private$val,
    set = function(val) {
      stopifnot(is(val, "i18n_dict"))
      private$val <- val
    },
    reset = function() private$val <- NULL
  ),
  private = list(val = NULL)
)

I18N_GLOBAL_DICT <- i18n_global_dict$new()
