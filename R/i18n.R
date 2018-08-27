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
    },
    print = function(...) {
      df <- self$as.data.frame()
      n <- nrow(df)
      cat(sprintf("i18n dictionary (%i terms):\n\n", n))
      print(head(df))
      cat("\n")
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

i18n_state <- R6::R6Class(
  "i18n_state",
  public = list(
    dict = NULL,
    lang = NULL,
    initialize = function() {
      self$dict <- NULL
      self$lang <- NULL
    },
    set = function(dict, lang) {
      stopifnot(is.null(dict) ||
                  is(dict, "i18n_dict") ||
                  identical(dict, "identity"),
                is.null.or(lang, is.scalar.character))
      self$dict <- dict
      self$lang <- lang
    },
    reset = function() {
      self$dict <- NULL
      self$lang <- NULL
    },
    translate = function(key) {
      if (is.null(self$dict)) stop("cannot translate, no dictionary defined")
      if (is.null(self$lang)) stop("cannot translate, no language defined")
      if (identical(self$dict, "identity")) key else
        self$dict$translate(key = key, language = self$lang)
    }
  )
)

I18N_STATE <- i18n_state$new()

# Translate
i18n <- function(key) {
  I18N_STATE$translate(key)
}

selected_i18n_dict <- R6::R6Class(
  "selected_i18n_dict",
  public = list(initialize = function() private$val <- NULL,
                get = function() private$val,
                set = function(val) {
                  stopifnot(is.null(val) || is(val, "i18n_dict"))
                  private$val <- val
                },
                reset = function() private$val <- NULL),
  private = list(val = NULL)
)
SELECTED_I18N_DICT <- selected_i18n_dict$new()

#' @export
with_i18n <- function(dict, expr) {
  old_dict <- SELECTED_I18N_DICT$get()
  SELECTED_I18N_DICT$set(dict)
  tryCatch(
    res <- eval(expr),
    error = function(e) {
      SELECTED_I18N_DICT$set(old_dict)
      stop(e)
    })
  SELECTED_I18N_DICT$set(old_dict)
  res
}
