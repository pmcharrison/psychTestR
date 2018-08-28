#' @export
i18n_dict <- R6::R6Class(
  "i18n_dict",
  public = list(
    initialize = function(x) {
      i18n_check(x)
      private$languages <- setdiff(names(x), "key")
      private$dict <- hash_df(x)
    },
    as.data.frame = function() unhash_df(private$dict),
    list_languages = function() private$languages,
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
  private = list(dict = NULL, languages = NULL)
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
      # if (WITH_I18N$get()) stop(with_i18n_error())
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

# selected_i18n_dict <- R6::R6Class(
#   "selected_i18n_dict",
#   public = list(initialize = function() private$val <- NULL,
#                 get = function() private$val,
#                 set = function(val) {
#                   stopifnot(is.null(val) || is(val, "i18n_dict"))
#                   private$val <- val
#                 },
#                 reset = function() private$val <- NULL),
#   private = list(val = NULL)
# )
# SELECTED_I18N_DICT <- selected_i18n_dict$new()
#
# with_i18n_flag <- R6::R6Class(
#   "with_i18n_flag",
#   public = list(initialize = function() private$val <- FALSE,
#                 get = function() private$val,
#                 set = function(val) private$val <- val),
#   private = list(val = FALSE)
# )
# WITH_I18N <- with_i18n_flag$new()


#' @export
i18n_timeline <- R6::R6Class(
  "i18n_timeline",
  public = list(
    initialize = function(x) {
      stopifnot(is.list(x))
      private$..languages <- names(x)
      private$data <- as.environment(x)
    },
    get = function(language) {
      if (!is.scalar.character(language))
        stop("'language' must be a scalar character")
      if (!language %in% self$languages)
        stop("'language'", language, " not supported by timeline ",
             "(valid languages: ", paste(self$languages, collapse = ", "), ")")
      private$data[[language]]
    },
    print = function(...) {
      cat("psychTestR i18n timeline\n")
      cat(sprintf("  %i languages: %s\n\n",
                  length(self$languages),
                  paste(self$languages, collapse = ", ")))
    }
  ),
  private = list(data = NULL, ..languages = NULL),
  active = list(languages = function() private$..languages)
)

#' @export
new_i18n_timeline <- gtools::defmacro(dict, x, expr = {
  langs <- dict$list_languages()
  if (length(langs) == 0) stop("zero languages in dictionary")
  timeline <- list()
  for (i in seq_along(langs)) {
    lang <- langs[i]
    timeline[[i]] <- psychTestR:::with_i18n_state(dict = dict, lang = lang, x = x)
  }
  names(timeline) <- langs
  i18n_timeline$new(timeline)
})

with_i18n_state <- gtools::defmacro(dict, lang, x, expr = {
  old_state <- list(dict = psychTestR:::I18N_STATE$dict,
                    lang = psychTestR:::I18N_STATE$lang)
  psychTestR:::I18N_STATE$set(dict = dict, lang = lang)
  tryCatch(
    res <- eval(x),
    error = function(e) {
      psychTestR:::I18N_STATE$set(dict = old_state$dict,
                                  lang = old_state$lang)
      stop(e)
    }
  )
  psychTestR:::I18N_STATE$set(dict = old_state$dict,
                              lang = old_state$lang)
  res
})
