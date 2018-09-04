#' @export
i18n_dict <- R6::R6Class(
  "i18n_dict",
  public = list(
    initialize = function(x, markdown = TRUE) {
      i18n_check(x)
      private$..languages <- setdiff(names(x), "key")
      private$dict <- hash_df(x, markdown = markdown)
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
  private = list(dict = NULL, ..languages = NULL),
  active = list(languages = function() private$..languages)
)

#' @export
as.data.frame.i18n_dict <- function(x, ...) {
  x$as.data.frame()
}

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
i18n <- function(..., sub = character()) {
  stopifnot(length(sub) == 0L || !is.null(names(sub)))
  x <- as.character(c(...))
  res <- vapply(x, function(y) I18N_STATE$translate(y), character(1),
                USE.NAMES = FALSE)
  if (length(sub) > 0L) {
    from <- paste("{{", names(sub), "}}", sep = "")
    to <- as.character(sub)
    for (i in seq_along(sub))
      res <- gsub(res, pattern = from[i], replacement = to[i], fixed = TRUE)
  }
  res
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

# with_i18n_flag <- R6::R6Class(
#   "with_i18n_flag",
#   public = list(initialize = function() private$val <- FALSE,
#                 get = function() private$val,
#                 set = function(val) private$val <- val),
#   private = list(val = FALSE)
# )
# WITH_I18N <- with_i18n_flag$new()


timeline <- R6::R6Class(
  "timeline",
  public = list(
    initialize = function(x) {
      stopifnot(is.list(x))
      private$..length <- if (length(x) == 0) 0L else unique(vapply(x, length, integer(1)))
      if (length(private$..length) > 1L) stop("inconsistent timeline lengths between languages")
      private$..languages <- names(x)
      private$data <- as.environment(x)
    },
    get = function(language, i = NULL) {
      if (!is.scalar.character(language))
        stop("'language' must be a scalar character")
      if (!language %in% self$languages)
        stop("'language'", language, " not supported by timeline ",
             "(valid languages: ", paste(self$languages, collapse = ", "), ")")
      if (!is.null.or(i, is.scalar.integerlike))
        stop("'i' must either be NULL or a scalar integer")
      lst <- private$data[[language]]
      if (is.null(i)) lst else {
        n <- length(lst)
        if (i < 1 || i > n) stop("element ", i, " doesn't exist in timeline")
        lst[[i]]
      }
    },
    print = function(...) {
      cat("psychTestR timeline\n")
      cat(sprintf("  %i languages: %s\n",
                  length(self$languages),
                  paste(self$languages, collapse = ", ")))
      cat(sprintf("  length: %i elements\n", self$length))
      cat("\n")
    }
  ),
  private = list(data = NULL, ..languages = NULL, ..length = NULL),
  active = list(languages = function() private$..languages,
                length = function() private$..length)
)

#' @export
c.timeline <- function(...) {
  input <- list(...)
  if (length(input) == 1L) {
    return(input[[1]])
  } else {
    Reduce(function(x, y) {
      x_is_timeline <- is(x, "timeline")
      y_is_timeline <- is(y, "timeline")
      lst <- if (x_is_timeline && y_is_timeline) {
        langs <- sort(intersect(x$languages, y$languages))
        sapply(langs, function(lang) {
          c(x$get(lang), y$get(lang))
        }, simplify = FALSE)
      } else if (x_is_timeline && !y_is_timeline) {
        langs <- sort(x$languages)
        sapply(langs, function(lang) {
          c(x$get(lang), y)
        }, simplify = FALSE)
      } else if (!x_is_timeline && y_is_timeline) {
        langs <- sort(y$languages)
        sapply(langs, function(lang) {
          c(x, y$get(lang))
        }, simplify = FALSE)
      } else stop("this shouldn't happen")
      timeline$new(lst)
    }, input)
  }
}

#' @export
new_timeline <- gtools::defmacro(x, dict = NULL, default_lang = "EN", expr = {
  stopifnot(psychTestR:::is.null.or(dict, function(z) is(dict, "i18n_dict")),
            psychTestR:::is.scalar.character(default_lang))
  local({
    langs <- if (is.null(dict)) default_lang else dict$languages
    res <- list()
    for (i in seq_along(langs)) {
      lang <- langs[i]
      res[[i]] <- if (is.null(dict)) x else psychTestR:::with_i18n_state(dict = dict, lang = lang, x = x)
    }
    names(res) <- langs
    psychTestR:::timeline$new(res)
  })
})

with_i18n_state <- gtools::defmacro(dictionary, language, x, expr = {
  local({
    old_state <- list(dict = psychTestR:::I18N_STATE$dict,
                      lang = psychTestR:::I18N_STATE$lang)
    psychTestR:::I18N_STATE$set(dict = dictionary, lang = language)
    # browser()
    tryCatch(
      res <- eval(x),
      error = function(e) {
        # browser()
        psychTestR:::I18N_STATE$set(dict = old_state$dict,
                                    lang = old_state$lang)
        stop(e)
      }
    )
    psychTestR:::I18N_STATE$set(dict = old_state$dict,
                                lang = old_state$lang)
    res
  })
})
