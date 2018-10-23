#' Dictionary
#'
#' Dictionaries allow psychTestR tests to
#' support multiple languages during test administration.
#' These dictionaries (class name: \code{i18n_dict})
#' define mappings between string keys (e.g. \code{prompt1})
#' and (potentially) multiple outputs,
#' each corresponding to different languages (e.g. English, German).
#' The dictionary is passed as an argument to \code{\link{new_timeline}},
#' and provides the context for the evaluation of calls to
#' \code{\link{i18n}}, responsible for translating individual terms.
#' @section Methods:
#' \code{i18n_dict$new()} defines and returns a new dictionary
#' which can be saved as an object.
#' Its first argument, \code{x}, should be a data frame defining the dictionary.
#' Each row of this data frame should correspond to a term to be translated.
#' It should have a column entitled 'key', which defines the identifying
#' keys for each term.
#' All other columns should provide translations into different languages,
#' with the language being identified by the column name,
#' according to ISO 639-2 conventions.
#' The second argument, \code{markdown}, is a scalar Boolean
#' that determines whether the data frame is formatted in
#' Markdown or not (Markdown allows for efficient and readable
#' text markup, e.g. italic and bold).
#' Text is parsed according to standard Markdown conventions,
#' with one addition: two successive backslashes
#' (written "\\" in a text file, or "\\\\" in R)
#' are interpreted as a new paragraph.
#'
#' \code{x$as.data.frame}, where \code{x} is a dictionary object,
#' converts the dictionary back to a data frame representation.
#'
#' \code{x$translate(key, language, allow_missing)}
#' uses the dictionary \code{x}
#' to translate \code{key} into \code{language}.
#' If \code{allow_missing} is \code{TRUE}, then \code{NULL} is returned
#' when no translation is found, otherwise an error is thrown.
#' @export
i18n_dict <- R6::R6Class(
  "i18n_dict",
  public = list(
    initialize = function(x, markdown = TRUE) {
      i18n_check_df(x)
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
i18n_check_df <- function(x) {
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
      if (is.null(self$dict) || is.null(self$lang)) {
        warning("undefined i18n dictionary/language, key left untranslated",
                call. = FALSE)
        key
      } else if (identical(self$dict, "identity")) {
        key
      } else {
        self$dict$translate(key = key, language = self$lang)
      }
    }
  )
)

I18N_STATE <- i18n_state$new()

# with_i18n_error <- function() {
#   msg <- "i18n() cannot be evaluated within with_i18n()"
#   condition(c("with_i18n_error", "error"),
#             message = msg)
# }

# missing_dict_error <- function() {
#   msg <- "cannot translate, no dictionary defined"
#   condition(c("missing_dict_error", "error"),
#             message = msg)
# }

# missing_lang_error <- function() {
#   msg <- "cannot translate, no language defined"
#   condition(c("missing_lang_error", "error"),
#             message = msg)
# }

#' Translate
#'
#' Translates text into a language determined by the current testing session.
#' Translation is carried out using a dictionary defined at test creation
#' (see \code{\link{i18n_dict}}).
#' The function can typically only be used within a call
#' to \code{\link{new_timeline}}, which defines the dictionary
#' in which to look up \code{i18n}.
#' Calling \code{i18n} outside this context will typically result in an error.
#' @param x Character scalar identifying the term to be translated.
#' @param html Boolean; whether or not the translation output should
#' be parsed as HTML or as plain text (the default is \code{TRUE},
#' corresponding to HTML parsing).
#' @param sub A named character vector or list defining substitutions
#' to make within the translation output. The text of the translation output
#' may contain passages such as \code{{{var1}}};
#' if \code{sub = c(var1 = "Hello")},
#' then \code{{{var1}}} will be replaced with the text \code{Hello}.
#' @return The translated output (typically a character scalar
#' or a \code{shiny::HTML()} output.
#' @export
i18n <- function(x, html = TRUE, sub = character()) {
  i18n_check(as.list(environment()))
  res <- I18N_STATE$translate(x)
  if (length(sub) > 0L) {
    from <- paste("{{", names(sub), "}}", sep = "")
    to <- as.character(sub)
    for (i in seq_along(sub))
      res <- gsub(res, pattern = from[i], replacement = to[i], fixed = TRUE)
  }
  if (html) shiny::HTML(res) else res
}

i18n_check <- function(x) {
  if (!(is.null(x$sub) || is.vector(x$sub)))
    stop("argument <sub> for function i18n_check() must be a vector, ",
         "instead got ", utils::capture.output(print(x$sub)))
  if (length(x$sub) > 0 && is.null(names(x$sub)))
    stop("argument <sub> for function i18n_check() was missing names, ",
         "instead got ", utils::capture.output(print(x$sub)))
  if (!is.scalar.character(x$x))
    stop("argument <x> for function i18_check() must be a character scalar, ",
         "instead got ", utils::capture.output(print(x$x)))
  if (!is.scalar.logical(x$html))
    stop("argument <html> for function i18_check() must be a logical scalar, ",
         "instead got ", utils::capture.output(print(x$html)))
}

#' Timeline
#'
#' Timelines are series of psychTestR test elements that chain
#' together to form a test.
#' They support internationalisation,
#' defining parallel series of test elements for the available languages.
#' @section Creation:
#' Timelines are created using \code{\link{new_timeline}}.
#' @section Manipulation:
#' Timelines can be combined with other timelines and with test elements
#' using \code{\link{c}}.
#' @section Usage:
#' Timelines are ultimately passed to \code{\link{make_test}}.
#' @section Other methods:
#' \code{x$get(language, i)} returns a list of test elements corresponding
#' to \code{language} as extracted from the timeline \code{x},
#' or, if \code{i} is not \code{NULL}, the ith such test element.
#'
#' \code{x$drop_languages(drop)} removes support for a set of languages
#' from timeline \code{x}, where \code{drop} is the character vector
#' of languages to remove.
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

#' New timeline
#'
#' Creates a new timeline.
#' Timelines allow tests to support multiple languages.
#' @param x Expression defining a series of test elements.
#' This expression will be evaluated once for every language
#' provided in the dictionary argument (\code{dict}).
#' A call to \code{\link{i18n}(key)}, where \code{key}
#' is a term defined in \code{dict},
#' will be translated to its definitions in each respective language.
#' @param dict Dictionary object as created by \code{\link{i18n_dict}$new}.
#' @param default_lang If no dictionary is supplied, then
#' \code{new_timeline()} assumes that the current language is
#' \code{default_lang}.
#' @note Debugging is difficult within \code{new_timeline()}
#' because of its underlying macro definition.
#' When building a test, we recommend defining small timelines
#' first and subsequently combining them.
#' This helps to narrow down the source of any errors.
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
  if (!all(langs %in% input$languages))
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

#' Is it a timeline object?
#'
#' Checks whether an object is a timeline object.
#' @param x Object to check
#' @return \code{TRUE} if object is a timeline object, \code{FALSE} otherwise.
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
