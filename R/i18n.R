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
      if (WITH_I18N$get()) stop(with_i18n_error())
      if (is.null(self$dict)) stop(missing_dict_error())
      if (is.null(self$lang)) stop(missing_lang_error())
      if (identical(self$dict, "identity")) key else
        self$dict$translate(key = key, language = self$lang)
    }
  )
)

I18N_STATE <- i18n_state$new()

with_i18n_error <- function() {
  msg <- "i18n() cannot be evaluated within with_i18n()"
  condition(c("with_i18n_error", "error"),
            message = msg)
}

missing_dict_error <- function() {
  msg <- "cannot translate, no dictionary defined"
  condition(c("missing_dict_error", "error"),
            message = msg)
}

missing_lang_error <- function() {
  msg <- "cannot translate, no language defined"
  condition(c("missing_lang_error", "error"),
            message = msg)
}

# Translate
#' @export
i18n <- function(...) {
  x <- as.character(c(...))
  vapply(x, function(y) I18N_STATE$translate(y), character(1),
         USE.NAMES = FALSE)
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

with_i18n_flag <- R6::R6Class(
  "with_i18n_flag",
  public = list(initialize = function() private$val <- FALSE,
                get = function() private$val,
                set = function(val) private$val <- val),
  private = list(val = FALSE)
)
WITH_I18N <- with_i18n_flag$new()


#' @export
with_i18n <- function(dict, expr) {
  old_dict <- SELECTED_I18N_DICT$get()
  SELECTED_I18N_DICT$set(dict)
  WITH_I18N$set(TRUE)
  tryCatch(
    res <- eval(expr),
    error = function(e) {
      SELECTED_I18N_DICT$set(old_dict)
      WITH_I18N$set(FALSE)
      stop(e)
    })
  SELECTED_I18N_DICT$set(old_dict)
  WITH_I18N$set(FALSE)
  res
}

#' @export
i18_support <- function(expr) {
  parent <- sys.frame(-1)
  tryCatch(
    eval(expr, envir = parent),
    with_i18n_error = function(e)
      reactive_page(function(...) eval(expr, envir = parent))
  )
}
