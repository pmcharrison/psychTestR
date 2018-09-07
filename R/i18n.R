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
i18n <- function(x, html = TRUE, sub = character()) {
  stopifnot(length(sub) == 0L || !is.null(names(sub)),
            is.scalar.character(x),
            is.scalar.logical(html))
  res <- I18N_STATE$translate(x)
  if (length(sub) > 0L) {
    from <- paste("{{", names(sub), "}}", sep = "")
    to <- as.character(sub)
    for (i in seq_along(sub))
      res <- gsub(res, pattern = from[i], replacement = to[i], fixed = TRUE)
  }
  if (html) shiny::HTML(res) else res
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

# You can drop languages from a timeline but you can't add them.
timeline <- R6::R6Class(
  "timeline",
  public = list(
    initialize = function(x) {
      stopifnot(is.list(x))
      private$..length <- if (length(x) == 0) 0L else
        unique(vapply(x, length, integer(1)))
      if (length(private$..length) > 1L)
        stop("inconsistent timeline lengths between languages")
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
    drop_languages = function(drop) {
      stopifnot(is.character(drop))
      rm(list = drop, envir = private$data)
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
  private = list(data = NULL, ..length = NULL),
  active = list(languages = function() sort(names(private$data)),
                length = function() private$..length)
)

#' @export
length.timeline <- function(x) {
  x$length
}

#' @export
c.timeline <- function(...) {
  input <- list(...)
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

#' @export
new_timeline <- gtools::defmacro(x, dict = NULL, default_lang = "EN", expr = {
  stopifnot(psychTestR:::is.null.or(dict, function(z) is(dict, "i18n_dict")),
            psychTestR:::is.scalar.character(default_lang))
  local({
    langs <- if (is.null(dict)) default_lang else dict$languages
    res <- list()
    for (i in seq_along(langs)) {
      lang <- langs[i]
      tmp <- if (is.null(dict)) x else
        psychTestR:::with_i18n_state(dict = dict, lang = lang, x = x)
      if (psychTestR::is.timeline(tmp)) {
        return(psychTestR:::format_new_timeline(tmp, langs))
      } else {
        res[[i]] <- psychTestR:::format_test_element_list(tmp, lang)
      }
    }
    names(res) <- langs
    psychTestR:::timeline$new(res)
  })
})

# To be called from within new_timeline()
format_new_timeline <- function(input, langs) {
  stopifnot(is(input, "timeline"),
            is.character(langs))
  if (!all(langs) %in% input$languages)
    stop("new_timeline() was called with dictionary languages ",
         paste(langs, collapse = ", "),
         " but the input <x> evaluated to a timeline where these ",
         "languages were not available ",
         "(available languages: ", paste(input$languages, collapse = ", "),
         ")")
  drop <- setdiff(input$languages, langs)
  input$drop_languages(drop)
  input
}

# To be called from within new_timeline()
# Timelines are permitted within the input list.
# However, an error will be thrown if these timelines don't
# support the required languages.
format_test_element_list <- function(input, lang) {
  stopifnot(is.scalar.character(lang))
  x <- if (psychTestR::is.test_element(input)) list(input) else input
  if (!is.list(x)) stop("new_timeline() received an input that wasn't a ",
                        "test element or a list")
  format_test_element_list.check_classes(x)
  x <- format_test_element_list.dissolve_timelines(x, lang)
  x
}

format_test_element_list.check_classes <- function(x) {
  if (!all(vapply(x, function(y) {
    psychTestR::is.test_element(y) ||
      psychTestR::is.timeline(y)
  }, logical(1)))) {
    classes <- vapply(x, class, character(1))
    stop("new_timeline() received a list as input, ",
         "but not all elements were test elements. ",
         "Here is the class list: ",
         paste(classes, collapse = ", "))
  }
}

format_test_element_list.dissolve_timelines <- function(x, lang) {
  l <- lapply(x, function(y) {
    if (is.test_element(y)) {
      y
    } else if (is.timeline(y)) {
      if (!(lang %in% y$languages)) {
        stop("argument <x> to new_timeline() produced a list ",
             "containing a timeline, but this timeline ",
             "did not support the language ", lang, " ",
             "(supported languages: {",
             paste(y$languages, collapse = ", "), "})")
      }
      y$get(lang)
    } else stop("this shouldn't happen")
  })
  do.call(what = c, args = l)
}

#' @export
is.timeline <- function(x) is(x, "timeline")

with_i18n_state <- gtools::defmacro(dictionary, language, x, expr = {
  local({
    old_state <- list(dict = psychTestR:::I18N_STATE$dict,
                      lang = psychTestR:::I18N_STATE$lang)
    psychTestR:::I18N_STATE$set(dict = dictionary, lang = language)
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
})
