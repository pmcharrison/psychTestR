setOldClass("shiny.tag")
setOldClass("shiny.tag.list")

setClassUnion("function_or_null", members = c("function", "NULL"))
setClassUnion("character_or_null", members = c("character", "NULL"))
setClassUnion("shiny_tag_or_null", members = c("shiny.tag", "NULL"))

setClass("test_element", slots = c("i18n_dict", "next_elt"))

setMethod("initialize", "test_element", function(.Object, ...) {
  .Object@i18n_dict <- I18N_STATE$dict
  callNextMethod()
})

#' @export
as.list.test_element <- function(x, ...) {
  methods::slotNames(x) %>%
    {purrr::set_names(., .)} %>%
    purrr::map(~ slot(x, .))
}

#' Is object a test element?
#'
#' Checks whether an object is a test element.
#' @param x Object to check.
#' @export
is.test_element <- function(x) is(x, "test_element")

#' @export
c.test_element <- function(...) {
  join(...)
}

#' Join test elements
#'
#' Joins a series of test elements or timelines into
#' one list or timeline.
#' Often one can equivalently use `c(...)` in such situations,
#' but this approach is safer.
#'
#' @param ... Test elements or timelines to combine.
#'
#' @return A list of test elements if the input didn't contain
#' any timelines, otherwise a timeline.
#'
#' @export
join <- function(...) {
  input <- list(...)
  input <- Filter(f = Negate(is.null), input)
  stopifnot(all(vapply(input, function(x) {
    is(x, "test_element") || is(x, "timeline") || is.list(x)
  }, logical(1))))
  Reduce(function(x, y) {
    if (is(x, "timeline") || is(y, "timeline")) {
      c.timeline(x, y)
    } else if (is(x, "test_element") && is(y, "test_element")) {
      list(x, y)
    } else if (is(x, "test_element") && is(y, "list")) {
      c(list(x), y)
    } else if (is(x, "list") && is(y, "test_element")) {
      c(x, list(y))
    } else if (is(x, "list") && is(y, "list")) {
      c(x, y)
    } else stop("this shouldn't happen")
  }, input)
}

# setMethod("show", signature(object = "test_element"),
#           definition = function(object) {
#             cat("i18n dictionary: ")
#             print(object@i18n_dict)
#             callNextMethod()
#           })

setClass("page",
         slots = list(ui = "shiny.tag",
                      admin_ui = "shiny_tag_or_null",
                      label = "character_or_null",
                      final = "logical",
                      get_answer = "function_or_null",
                      save_answer = "logical",
                      validate = "function_or_null",
                      on_complete = "function_or_null"),
         contains = "test_element")

setClass("reactive_page",
         slots = list(fun = "function"),
         prototype = list(fun = function(state) page),
         contains = "test_element")

#' Reactive page
#'
#' Creates a reactive page.
#' A reactive page is defined by a function that is called once the participant
#' reaches a given location in the timeline.
#' This function must be idempotent, i.e. calling it multiple times should
#' have the same effect as calling it once).
#' @param fun Function that returns a \code{page} argument.
#' This function's argument list must include \code{...}.
#' It should also include one or more named arguments that provide
#' information for generating the new page: potential arguments
#' include \code{state}, corresponding to the participant's
#' \code{state} object,
#' \code{answer}, which is the most recent participant response,
#' and \code{opt}, which is the test's option list as created by
#' \code{test_options}.
#' The function should always return an object of class \code{page}.
#' @param next_elt (Logical scalar) Whether to go to the next element
#' in the timeline once this page is completed.
#' This will typically be \code{TRUE},
#' except for special cases such as pages that load
#' testing sessions from file (\code{\link{get_p_id}}).
#' @export
reactive_page <- function(fun, next_elt = TRUE) {
  new("reactive_page", fun = fun, next_elt = next_elt)
}

setClass("code_block",
         slots = list(fun = "function"),
         contains = "test_element",
         prototype = list(fun = function(...) NULL))

#' Code block
#'
#' Creates a code block.
#' @param fun Function to execute within code block.
#' This function's argument list must include \code{...}.
#' It may also include any of the following named arguments:
#' \code{state}, the participant's state object;
#' \code{opt}, which is the test's option list as created by \code{test_options()};
#' \code{input}, the current page's Shiny input object;
#' \code{output}, the current page's Shiny output object;
#' \code{session}, the current Shiny session object;
#' \code{elts}, the timeline (i.e. list of test elements).
#' @param next_elt (Logical scalar) Whether to go to the next element
#' in the timeline once this page is completed.
#' This will typically be \code{TRUE},
#' except for special cases such as pages that load
#' testing sessions from file (\code{\link{get_p_id}}).
#' @export
code_block <- function(fun, next_elt = TRUE) {
  new("code_block", fun = fun, next_elt = next_elt)
}

setMethod(
  "show",
  signature(object = "page"),
  definition = function(object) {
    cat("psychTestR page\n")
    view_page(object)
  })

view_page <- function(object) {
  htmltools::html_print(shiny::div(
    shiny::includeCSS(system.file(shinythemes::shinytheme("yeti"),
                                  package = "shinythemes")),
    shiny::fluidRow(shiny::wellPanel(
      shiny::h3("<App title>", align = "center")),
      style = paste("padding: 3px; padding-left: 10px; padding-right: 10px;",
                    "background-color: white;")),
    shiny::fluidRow(
      id = "content",
      shiny::column(2),
      shiny::column(8, shiny::wellPanel(
        align = "center",
        object@ui,
        style = "background-color: white"
      )),
      shiny::column(2)
    )))
}

setMethod(
  "show",
  signature(object = "code_block"),
  definition = function(object) {
    cat("psychTestR code block\n")
    cat("Function: ")
    print(object@fun)
  }
)

setMethod(
  "show",
  signature(object = "reactive_page"),
  definition = function(object) {
    cat("psychTestR reactive page\n")
    cat("Function: ")
    print(object@fun)
    view_page(final_page(shiny::em("reactive page")))
  }
)

#' New page
#'
#' This is the most general way to create a psychTestR page.
#' @param ui Page UI. Can be either a character scalar (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#' @param admin_ui Optional UI component for the admin panel.
#' @param label Page label (character scalar).
#' @param final Whether or not the page is the final page in the test.
#' @param get_answer Optional function for extracting the participant's
#' answer from the current page.
#' The argument list should include \code{...}, \code{input},
#' and optionally \code{state}, the participant's \code{state} object.
#' The function should extract the answer from the Shiny
#' \code{input} object and return it as its output.
#' @param save_answer Whether or not to save the answer.
#' @param validate Optional validation function.
#' The argument list should include \code{...},
#' and any of:
#' \code{answer}, the participant's most recent answer;
#' \code{state}, the participant's state object;
#' \code{input}, the current page's Shiny input object;
#' \code{opt}, the test's option list as created by \code{test_options()};
#' \code{session}, the current Shiny session object.
#' It should return \code{TRUE} for a successful validation;
#' for an unsuccessful validation, it should return either \code{FALSE}
#' or a character scalar error message.
#' If validation fails then the page will be refreshed, usually
#' to give the user a chance to revise their input.
#' @param on_complete Optional function to execute on leaving the page
#' (after successful validation).
#' The argument list should include \code{...},
#' and any of:
#' \code{state}, the participant's state object;
#' \code{answer}, the participant's most recent answer;
#' \code{input}, the current page's Shiny input object;
#' \code{session}, the current Shiny session object;
#' \code{opt}, the test's option list as created by \code{test_options()}.
#' @param next_elt (Logical scalar) Whether to go to the next element
#' in the timeline once this page is completed.
#' This will typically be \code{TRUE},
#' except for special cases such as pages that load
#' testing sessions from file (\code{\link{get_p_id}}).
#' @export
page <- function(ui, admin_ui = NULL, label = NULL,
                 final = FALSE, get_answer = NULL,
                 save_answer = FALSE, validate = NULL, on_complete = NULL,
                 next_elt = TRUE) {
  ui <- tagify(ui)
  stopifnot(
    is(ui, "shiny.tag"),
    is.null(admin_ui) || is(admin_ui, "shiny.tag") || is.scalar.character(admin_ui),
    is.null.or(label, is.scalar.character),
    is.scalar.logical(final),
    is.null.or(get_answer, is.function),
    is.scalar.logical(save_answer),
    is.null.or(validate, is.function),
    is.null.or(on_complete, is.function),
    is.scalar.logical(next_elt))
  if (save_answer && !is.scalar.character(label))
    stop("if `save_answer` is `TRUE` then a character scalar label ",
         "must be provided")
  if (is.scalar.character(admin_ui)) admin_ui <- tagify(admin_ui)
  new("page", ui = ui, admin_ui = admin_ui, label = label, final = final,
      get_answer = get_answer,
      save_answer = save_answer, validate = validate, on_complete = on_complete,
      next_elt = next_elt)
}

#' New one-button page
#'
#' Creates a page with a prompt and one button which, when clicked,
#' advances to the next page.
#' This is typically used for giving the participant information
#' about the test.
#'
#' @param body Page body. Can be either a character scalar (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#'
#' @param button_text Text to display on the button.
#' Should be a character scalar.
#'
#' @inheritParams page
#'
#' @export
one_button_page <- function(body, admin_ui = NULL, button_text = "Next",
                            on_complete = NULL) {
  body <- tagify(body)
  stopifnot(is.scalar.character(button_text))
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, admin_ui = admin_ui, on_complete = on_complete)
}

#' New final page
#'
#' Creates a page that concludes the test.
#'
#' @param body Page body. Can be either a character scalar (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#'
#' @inheritParams page
#'
#' @export
final_page <- function(body, admin_ui = NULL) {
  body <- shiny::tags$div(
    tagify(body),
    shiny::includeScript(system.file("js/allow-navigate-away.js",
                                     package = "psychTestR",
                                     mustWork = TRUE))
  )
  page(ui = body, admin_ui = admin_ui, final = TRUE)
}

#' Text input page
#'
#' Creates a page where the participant puts their
#' answer in a text box.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to display (character scalar or Shiny tag object).
#'
#' @param one_line Whether the answer box only has one line of text.

#' @param placeholder Placeholder text for the text box (character scalar).
#'
#' @param button_text Text for the submit button (character scalar).
#'
#' @param width Width of the text box (character scalar, should be valid HTML).
#'
#' @param height Height of the text box (character scalar, should be valid HTML).
#'
#' @inheritParams page
#'
#' @export
text_input_page <- function(label, prompt,
                            one_line = TRUE,
                            save_answer = TRUE,
                            placeholder = NULL,
                            button_text = "Next",
                            width = "300px",
                            height = "100px", # only relevant if one_line == FALSE
                            validate = NULL,
                            on_complete = NULL,
                            admin_ui = NULL) {
  stopifnot(is.scalar.character(label),
            is.scalar.logical(one_line))
  text_input <- if (one_line) {
    shiny::textInput("text_input", label = NULL,
                     placeholder = placeholder,
                     width = width)
  } else {
    shiny::textAreaInput("text_input", label = NULL,
                         placeholder = placeholder,
                         width = width,
                         height = height)
  }
  get_answer <- function(input, ...) input$text_input
  body = shiny::div(
    onload = "document.getElementById('text_input').value = '';",
    tagify(prompt),
    text_input
  )
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, label = label, get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}

#' Slider page
#'
#' Creates a page where the participant responds by manipulating a slider.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to display (character scalar or Shiny tag object).
#'
#' @param button_text Text for the submit button (character scalar).
#'
#' @inheritParams shiny::sliderInput
#'
#' @inheritParams page
#'
#' @note The RStudio preview function seems not to work
#' for slider pages on some machines.
#'
#' @export
slider_page <- function(label, prompt,
                        min, max, value,
                        save_answer = TRUE,
                        button_text = "Next",
                        on_complete = NULL,
                        admin_ui = NULL,
                        step = NULL, round = FALSE,
                        ticks = TRUE, animate = FALSE,
                        width = NULL, sep = ",",
                        pre = NULL, post = NULL,
                        timeFormat = NULL,
                        timezone = NULL, dragRange = TRUE) {
  stopifnot(is.scalar.character(label))

  slider <- shiny::sliderInput(
    "slider",
    label = NULL,
    min = min,
    max = max,
    value = value,
    step = step,
    round = round,
    ticks = ticks,
    animate = animate,
    width = width,
    sep = sep,
    pre = pre,
    post = post,
    timeFormat = timeFormat,
    timezone = timezone,
    dragRange = dragRange
  )

  get_answer <- function(input, ...) input$slider

  body = shiny::div(
    tagify(prompt),
    slider
  )
  ui <- shiny::div(body, trigger_button("next", button_text))

  page(ui = ui, label = label, get_answer = get_answer, save_answer = save_answer,
       on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}

#' Get participant ID
#'
#' A psychTestR page that gets the participant to enter their ID.
#'
#' @param prompt Prompt to display (character scalar or Shiny tag object)
#'
#' @param placeholder Placeholder text for the text box (character scalar).
#'
#' @param button_text Text for the submit button (character scalar).
#'
#' @param width Width of the text box (character scalar, should be valid HTML).
#'
#' @inheritParams page
#'
#' @note Participant IDs must be between 1 and 100 characters long,
#' and solely comprise alphanumeric characters and underscores.
#'
#' @export
get_p_id <- function(prompt = "Please enter your participant ID.",
                     placeholder = "e.g. 10492817",
                     button_text = "Next",
                     width = "300px",
                     validate = "auto",
                     admin_ui = NULL) {
  get_answer <- function(input, ...) input$p_id
  text_input <- shiny::textInput("p_id", label = NULL,
                                 placeholder = placeholder,
                                 width = width)
  body = shiny::div(tagify(prompt), text_input)
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, get_answer = get_answer, save_answer = FALSE,
       validate = get_p_id.validate(validate),
       on_complete = get_p_id.on_complete,
       admin_ui = admin_ui,
       next_elt = FALSE)
}

get_p_id.on_complete <- function(state, input, session, opt, ...) {
  p_id <- answer(state)
  success <- try_resume_session(p_id, state, session, opt,
                                ask_to_confirm_resume = TRUE,
                                reset_if_resume_fails = FALSE)
  if (!success) increment_elt_index(state)
}

get_p_id.validate <- function(validate) {
  if (is.function(validate)) {
    validate
  } else if (identical(validate, "auto")) {
    function(answer, ...) {
      if (is_p_id_valid(answer)) TRUE else describe_valid_p_id()
    }
  } else stop("Unrecognised validation method.")
}

is_p_id_valid <- function(p_id) {
  stopifnot(is.scalar.character(p_id))
  n <- nchar(p_id)
  n > 0L && n <= 100L && grepl("^[A-Za-z0-9_]*$", p_id)
}

describe_valid_p_id <- function() {
  paste0(
    "Participant IDs must be between 1 and 100 characters long, ",
    "and solely comprise alphanumeric characters ",
    "and underscores.")
}

#' New NAFC page
#'
#' Creates an n-alternative forced choice page.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for button IDs
#' and for button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param labels Optional vector of labels for the NAFC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param arrange_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#'
#' @param hide_response_ui Whether to begin with the response interface hidden
#' (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id HTML ID for the response user interface.
#'
#' @inheritParams page
#' @inheritParams make_ui_NAFC
#'
#' @export
NAFC_page <- function(label, prompt, choices, labels = NULL,
                      save_answer = TRUE,
                      arrange_vertically = length(choices) > 2L,
                      hide_response_ui = FALSE,
                      response_ui_id = "response_ui",
                      on_complete = NULL,
                      admin_ui = NULL,
                      button_style = "") {
  stopifnot(is.scalar.character(label),
            is.character(choices), length(choices) > 0L,
            is.scalar.logical(arrange_vertically))
  ui <- shiny::div(
    tagify(prompt),
    make_ui_NAFC(choices,
                 labels = labels,
                 hide = hide_response_ui,
                 arrange_vertically = arrange_vertically,
                 id = response_ui_id,
                 button_style = button_style))
  get_answer <- function(input, ...) input$last_btn_pressed
  validate <- function(answer, ...) !is.null(answer)
  page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}

#' Make NAFC buttons
#'
#' Creates HTML code for n-alternative forced-choice response options.
#'
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for button IDs
#' and for button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param labels Optional vector of labels for the NAFC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param hide Whether the response buttons should be hidden
#' (possibly to be shown later).
#'
#' @param arrange_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#'
#' @param id HTML ID for the div containing the response buttons.
#'
#' @param button_style Button CSS style information (character scalar).
#'
#' @export
make_ui_NAFC <- function(choices, labels = NULL, hide = FALSE,
                         arrange_vertically = length(choices) > 2L,
                         id = "response_ui", button_style = "") {
  stopifnot(is.character(choices), length(choices) > 0L, is.scalar.logical(hide),
            is.null(labels) ||
              ((is.character(labels) || is.list(labels)) &&
                 length(labels) == length(choices)))
  if (is.null(labels)) {
    labels <- if (is.null(names(choices))) choices else names(choices)
  }
  shiny::tags$div(id = id,
                  style = if (hide) "visibility: hidden" else "visibility: inherit",
                  mapply(function(id, label) {
                    trigger_button(inputId = id, label = label, style = button_style)
                  }, choices, labels, SIMPLIFY = F, USE.NAMES = F) %>%
                    (function(x) if (arrange_vertically) lapply(x, shiny::tags$p) else x))
}

#' New radiobutton NAFC page
#'
#' Creates a radiobutton n-alternative forced choice page.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?") or an object of
#' class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radiobutton IDs and for
#' radiobutton labels.
#' If named, then values will be used for radiobutton IDs and names will be used
#' for radiobutton labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the radiobutton NAFC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param failed_validation_message (Character scalar) Text to be displayed
#' when validation fails.
#'
#' @param save_answer (Boolean scalar) Whether or not to save the answer.
#'
#' @param hide_response_ui (Boolean scalar) Whether to begin with the response
#' interface hidden (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id (Character scalar) HTML ID for the response user interface.
#'
#' @inheritParams page
#' @inheritParams make_ui_radiobutton_NAFC
#'
#' @export
radiobutton_NAFC_page <-
  function(label,
           prompt,
           choices,
           subprompt = "",
           labels = NULL,
           trigger_button_text = "Continue",
           failed_validation_message = "Answer missing!",
           save_answer = TRUE,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL) {
    stopifnot(
      is.scalar.character(label),
      is.scalar.character(trigger_button_text),
      is.scalar.character(failed_validation_message),
      is.character(choices),
      length(choices) > 0L
    )
    ui <- shiny::div(
      tagify(prompt),
      make_ui_radiobutton_NAFC(
        label,
        choices,
        subprompt = subprompt,
        labels = labels,
        trigger_button_text = trigger_button_text,
        hide = hide_response_ui,
        id = response_ui_id
      )
    )
    get_answer <- function(input, ...)
      input[[label]]
    validate <- function(answer, ...)
      if (!is.null(answer)) {
        TRUE
      } else {
        failed_validation_message
      }
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make NAFC radiobuttons
#'
#' Creates HTML code for n-alternative forced-choice response radiobutton options.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radiobutton IDs and for
#' button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param hide (Boolean scalar) Whether the radiobuttons should be hidden (possibly to be shown later).
#'
#' @param id (Character scalar) HTML ID for the div containing the radiobuttons.
#'
#' @export
make_ui_radiobutton_NAFC <-
  function(label,
           choices,
           subprompt = "",
           labels = NULL,
           trigger_button_text = "Continue",
           hide = FALSE,
           id = "response_ui") {
    stopifnot(
      is.character(choices) && length(choices) > 0L,
      is.scalar.logical(hide),
      is.null(labels) ||
        ((is.character(labels) || is.list(labels)) &&
           length(labels) == length(choices)
        )
    )
    if (is.null(labels)) {
      labels <- if (is.null(names(choices)))
        choices
      else
        names(choices)
    }
    labels <-
      purrr::map(labels, function(label)
        shiny::tags$span(style = "font-size: 15px; line-height: 15px;", label))

    subprompt_div <- NULL
    if (subprompt != "")
      subprompt_div <-
      shiny::tags$div(style = "text-align: center;", shiny::tags$strong(subprompt))
    radiobuttons_div <-
      shiny::tags$div(
        style = "text-align: left;",
        subprompt_div,
        shiny::tags$div(
          style = "display: table; margin: 0 auto;",
          shiny::tags$div(
            style = "display: inline-block; width: 100%;",
            shiny::radioButtons(
              label,
              "",
              choiceNames = labels,
              choiceValues = choices,
              selected = 0
            )
          )
        )
      )
    shiny::tags$div(
      id = id,
      style = "display: inline-block",
      radiobuttons_div,
      psychTestR::trigger_button("next", trigger_button_text)
    )
  }

#' Make NAFC video page
#'
#' Creates an n-alternative forced choice page with a video prompt.
#'
#' @param url URL to the video.
#' Can be an absolute URL (e.g. "http://mysite.com/video.mp4")
#' or a URL relative to the /www directory (e.g. "video.mp4").
#'
#' @param type Video type (e.g. 'mp4'). Defaults to the provided file extension.
#'
#' @param video_width Video width, as passed to HTML (e.g. '50px').
#'
#' @param arrange_choices_vertically
#' Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#'
#' @param wait Whether to wait for the video to finish before displaying
#' the response buttons.
#'
#' @param loop Whether the video should loop.
#'
#' @inheritParams NAFC_page
#' @inheritParams audio_NAFC_page
#' @inheritParams make_ui_NAFC
#'
#' @export
video_NAFC_page <- function(label, prompt, choices, url,
                            labels = NULL,
                            type = tools::file_ext(url),
                            save_answer = TRUE,
                            on_complete = NULL,
                            video_width = "100%",
                            arrange_choices_vertically = length(choices) > 2L,
                            wait = TRUE,
                            loop = FALSE,
                            admin_ui = NULL,
                            btn_play_prompt = if (!show_controls) "Click here to play",
                            show_controls = FALSE,
                            allow_download = FALSE) {
  stopifnot(is.scalar.character(label),
            is.character(choices), is.scalar.character(url),
            is.scalar.character(url), is.scalar.character(video_width),
            is.scalar.logical(arrange_choices_vertically),
            is.scalar.logical(wait))
  video_ui <- shiny::tags$div(
    shiny::tags$video(
      shiny::tags$head(shiny::tags$script(shiny::HTML(media.js$media_not_played))),
      shiny::tags$source(src = url, type = paste0("video/", type)),
      id = "media", width = video_width, preload = "auto",
      autoplay = "autoplay", style = "max-width: 500px",
      playsinline = "playsinline",
      loop = if (loop) "loop",
      oncanplaythrough = media.js$show_media_btn,
      onplay = paste0(media.js$media_played, media.js$hide_media_btn),
      onended = if (wait) media.js$show_responses else "null",
      controls = if (show_controls) "controls",
      controlsList = if (!allow_download) "nodownload",
      disablePictureInPicture = "disablePictureInPicture"),
    media_mobile_play_button(btn_play_prompt))
  prompt2 <- shiny::div(tagify(prompt), video_ui)
  NAFC_page(label = label, prompt = prompt2, choices = choices, labels = labels,
            save_answer = save_answer,
            on_complete = on_complete,
            arrange_vertically = arrange_choices_vertically,
            hide_response_ui = wait, response_ui_id = "response_ui",
            admin_ui = admin_ui)
}

media.js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

media_mobile_play_button <- function(btn_play_prompt) shiny::tags$p(
  shiny::tags$strong(btn_play_prompt,
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media.js$play_media))

#' Make NAFC audio page
#'
#' Creates an n-alternative forced choice page with an audio prompt.
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param url URL to the audio
#' Can be an absolute URL (e.g. "http://mysite.com/audio.mp3")
#' or a URL relative to the /www directory (e.g. "audio.mp3").
#'
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#'
#' @param arrange_choices_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#'
#' @param wait Whether to wait for the audio to finish before displaying
#' the response buttons.
#'
#' @param loop Whether the audio should loop.
#'
#' @param btn_play_prompt
#' Text to display as a prompt for starting the audio.
#' Ordinarily the participant will not see this,
#' but it might appear if the internet connection is poor,
#' or if the participant refreshes the page.
#' Only used if show_controls is FALSE.
#'
#' @param show_controls
#' Whether or not to show audio controls to the participant,
#' so that they can control audio playback.
#'
#' @param allow_download
#' Whether the participant is given a button to download
#' the audio file; only relevant if show_controls is TRUE.
#'
#' @param autoplay
#' Whether the audio should start playing immediately
#' default is "autoplay"
#'
#' @inheritParams NAFC_page
#'
#' @export
audio_NAFC_page <- function(label, prompt, choices, url,
                            labels = NULL,
                            type = tools::file_ext(url),
                            save_answer = TRUE,
                            on_complete = NULL,
                            arrange_choices_vertically = length(choices) > 2L,
                            wait = TRUE, loop = FALSE,
                            admin_ui = NULL,
                            btn_play_prompt = if (!show_controls) "Click here to play",
                            show_controls = FALSE,
                            allow_download = FALSE,
                            button_style = "",
                            autoplay = "autoplay") {
  stopifnot(is.scalar.character(label),
            is.character(choices), is.scalar.character(url),
            is.scalar.character(url),
            is.scalar.logical(arrange_choices_vertically),
            is.scalar.logical(wait), is.scalar.logical(loop))
  audio_ui <- shiny::tags$div(shiny::tags$audio(
    shiny::tags$head(shiny::tags$script(shiny::HTML(media.js$media_not_played))),
    shiny::tags$source(src = url, type = paste0("audio/", type)),
    id = "media",
    preload = "auto",
    autoplay = if(nchar(autoplay) > 0) "autoplay",
    loop = if (loop) "loop",
    oncanplaythrough = media.js$show_media_btn,
    onplay = paste0(media.js$media_played, media.js$hide_media_btn),
    onended = if (wait) media.js$show_responses else "null",
    controls = if (show_controls) "controls",
    controlsList = if (!allow_download) "nodownload"
  ), media_mobile_play_button(btn_play_prompt))

  prompt2 <- shiny::div(tagify(prompt), audio_ui)
  NAFC_page(label = label, prompt = prompt2, choices = choices, labels = labels,
            save_answer = save_answer,
            on_complete = on_complete,
            arrange_vertically = arrange_choices_vertically,
            hide_response_ui = wait, response_ui_id = "response_ui",
            admin_ui = admin_ui,
            button_style = button_style)
}

#' Make volume calibration page
#'
#' Creates a page for the participant to calibrate their volume,
#' using example audio, and the volume controls on their computer.
#'
#' @param url URL to the audio.
#' Can be an absolute URL (e.g. "http://mysite.com/audio.mp3")
#' or a URL relative to the /www directory (e.g. "audio.mp3").
#'
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#'
#' @param prompt Prompt to be displayed. If left \code{NULL},
#' a sensible English prompt is provided.
#'
#' @param button_text Button text (character scalar).
#'
#' @param wait Wait parameter for HTML-5 audio element (logical scalar).
#'
#' @param loop Loop parameter for HTML-5 audio element (logical scalar).
#'
#' @param show_controls show_controls parameter for HTML-5 audio element (logical scalar).
#'
#' @inheritParams page
#' @inheritParams audio_NAFC_page
#'
#' @export
volume_calibration_page <- function(url, type = tools::file_ext(url),
                                    prompt = NULL,
                                    button_text = "Next",
                                    on_complete = NULL,
                                    admin_ui = NULL,
                                    btn_play_prompt = "Click here to play",
                                    wait = FALSE, loop = TRUE, show_controls = FALSE) {
  if (is.null(prompt)) prompt <- shiny::div(
    shiny::p(
      "You should hear some audio playing.",
      "Please adjust the volume to a comfortable level before continuing."
    ),
    shiny::p(
      "If you cannot make the audio play at a comfortable level,",
      "please do not continue, but instead ask the researcher for help."
    ),
    shiny::p("")
  )
  audio_NAFC_page(label = "volume_calibration",
                  prompt = prompt, choices = button_text,
                  save_answer = FALSE,
                  on_complete = on_complete,
                  url = url, type = type,
                  wait = wait, loop = loop,
                  admin_ui = admin_ui,
                  btn_play_prompt = btn_play_prompt,
                  show_controls = show_controls)
}

#' New checkbox page
#'
#' Creates a checkbox page.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?") or an object of
#' class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for checkbox IDs and for
#' checkbox labels.
#' If named, then values will be used for checkbox IDs and names will be used
#' for checkbox labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the checkbox choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param failed_validation_message (Named character vector) Text to be displayed
#' when validation fails, names must match the validation type ("one" or "all"), see "force_answer".
#'
#' @param force_answer (Boolean scalar or character) Require at least one checkbox (value "one") or all checkboxes ("all") to be
#' ticked. If no selection is required set to "no" (default). If boolean, this translate to option "one" (TRUE) or "no" (FALSE).
#'
#' @param javascript (Character scalar) JavaScript code to be added for
#' controlling checkbox behaviour.
#'
#' @param save_answer (Boolean scalar) Whether or not to save the answer.
#'
#' @param hide_response_ui (Boolean scalar) Whether to begin with the response
#' interface hidden (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id (Character scalar) HTML ID for the response user
#' interface.
#'
#' @inheritParams page
#' @inheritParams make_ui_checkbox
#'
#' @export
checkbox_page <-
  function(label,
           prompt,
           choices,
           subprompt = "",
           labels = NULL,
           trigger_button_text = "Continue",
           failed_validation_message = c(one = "Choose at least one answer!", all = "Please check all boxes."),
           force_answer = c( "no", "one", "all", "first"),
           javascript = "",
           save_answer = TRUE,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL) {
    stopifnot(
      is.scalar.character(label),
      is.character(choices) && length(choices) > 0L,
      is.scalar.character(trigger_button_text),
      is.character(failed_validation_message),
      is.character(force_answer) || is.scalar.logical(force_answer),
      is.scalar.character(javascript)
    )
    if(is.logical(force_answer)){
      force_answer <- ifelse(force_answer, "one", "no")
    }
    force_answer <- match.arg(force_answer)[1]
    ui <- shiny::div(
      tagify(prompt),
      make_ui_checkbox(
        label,
        choices,
        subprompt = subprompt,
        labels = labels,
        trigger_button_text = trigger_button_text,
        javascript = javascript,
        hide = hide_response_ui,
        id = response_ui_id
      )
    )
    get_answer <- function(input, ...)
      if (is.null(input[[label]])) {
        ""
      } else {
        input[[label]]
      }
    validate <- function(answer, ...){
      ret <- TRUE
      if(force_answer == "one"){
        if (!any(nzchar(answer))){
          if("one" %in% names(failed_validation_message)){
            ret <- failed_validation_message[["one"]]
          }
          else{
            ret <- failed_validation_message[[1]]
          }
        }
      }
      else if(force_answer == "first"){
        if (answer[1] != choices[1]){
          if("first" %in% names(failed_validation_message)){
            ret <- failed_validation_message[["first"]]
          }
          else{
            ret <- failed_validation_message[[1]]
          }
        }
      }
      else if (force_answer == "all"){
        if (sum(nzchar(answer)) != length(choices)) {
          if("all" %in% names(failed_validation_message)){
            ret <- failed_validation_message[["all"]]
          }
          else{
            ret <- failed_validation_message[1]
          }
        }
      }
      ret
    }
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make checkboxes
#'
#' Creates HTML code for checkbox response options.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for checkbox IDs and for
#' checkbox labels.
#' If named, then values will be used for checkbox IDs and names
#' will be used for checkbox labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the NOMC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param javascript (Character scalar) JavaScript code to be added for
#' controlling checkbox behaviour.
#'
#' @param hide (Boolean scalar) Whether the checkboxes should be hidden
#' (possibly to be shown later).
#'
#' @param id (Character scalar) HTML ID for the div containing the checkboxes.
#'
#' @export
make_ui_checkbox <-
  function(label,
           choices,
           subprompt = "",
           labels = NULL,
           trigger_button_text = "Continue",
           javascript = "",
           hide = FALSE,
           id = "response_ui") {
    stopifnot(
      is.character(choices) && length(choices) > 0L,
      is.scalar.logical(hide),
      is.null(labels) ||
        ((is.character(labels) || is.list(labels)) &&
           length(labels) == length(choices))
    )
    if (is.null(labels)) {
      labels <- if (is.null(names(choices))) {
        choices
      } else {
        names(choices)
      }
    }
    labels <-
      purrr::map(labels, function(label)
        shiny::tags$span(style = "font-size: 15px; line-height: 15px;", label))

    subprompt_div <- NULL
    if (subprompt != "") {
      subprompt_div <-
        shiny::tags$div(id = id,
                        style = "text-align: center;",
                        shiny::tags$strong(subprompt))
    }

    checkboxes_div <-
      shiny::tags$div(
        style = "text-align: left;",
        if (javascript != "")
          shiny::tags$script(shiny::HTML(javascript)),
        subprompt_div,
        shiny::checkboxGroupInput(label, "",
                                  choiceNames = labels, choiceValues = choices)
      )

    shiny::tags$div(
      id = id,
      style = "display: inline-block",
      checkboxes_div,
      psychTestR::trigger_button("next", trigger_button_text)
    )
  }

#' Make dropdown list page
#'
#' Creates a page where the response is to be selected from a dropdown list.
#'
#' @param label Page label (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param choices Character vector of choices for the participant.
#' If names are provided, then these names will be used for display,
#' whereas the values will be stored in the results.
#'
#' @param alternative_choice Whether or not to give the participant
#' the option of providing a free-text response instead of
#' selecting one of the dropdown options.
#'
#' @param alternative_text Prompt for the free-text box (only relevant
#' if \code{alternative_choice} is set to \code{TRUE}).
#'
#' @param next_button_text Text to display on the next-page button
#' (character scalar).
#'
#' @param max_width_pixels Maximum width of the response UI, in pixels.
#'
#' @inheritParams page
#'
#' @export
dropdown_page <- function(label, prompt, choices,
                          alternative_choice = FALSE,
                          alternative_text = "Other (please state)",
                          save_answer = TRUE,
                          validate = dropdown_page.validate(alternative_choice,
                                                            alternative_text),
                          on_complete = NULL,
                          next_button_text = "Next",
                          max_width_pixels = 200,
                          admin_ui = NULL) {
  stopifnot(is.scalar.character(label),
            is.character(choices),
            is.scalar.logical(alternative_choice),
            is.scalar.character(alternative_text),
            is.scalar.numeric(max_width_pixels),
            is.null.or(validate, is.function))
  prompt <- tagify(prompt)
  style <- sprintf("max-width:%ipx", round(max_width_pixels))
  choices <- c(choices, if (alternative_choice) alternative_text)
  dropdown <- shiny::selectizeInput("dropdown", label = NULL,
                                    choices = choices, multiple = FALSE)
  text_box <- if (alternative_choice) {
    shiny::textInput("text_alternative", NULL, placeholder = alternative_text)
  }
  button <- trigger_button("next", next_button_text)
  response_ui <- shiny::div(style = style, dropdown, text_box, button)
  ui <- shiny::div(prompt, response_ui)
  get_answer <- dropdown_page.get_answer(alternative_text)
  page(ui = ui, label = label, get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}

dropdown_page.validate <- function(alternative_choice, alternative_text) {
  if (alternative_choice) {
    function(state, input, ...) {
      if (input$dropdown == alternative_text &&
          input$text_alternative == "") {
        sprintf(
          "If you select '%s', you must fill in the text box.",
          alternative_text)
      } else if (input$dropdown != alternative_text &&
                 input$text_alternative != "") {
        sprintf(
          "If you fill in the test box, you must select '%s'.",
          alternative_text)
      } else TRUE
    }
  }
}

dropdown_page.get_answer <- function(alternative_text) {
  function(input, ...) {
    if (input$dropdown == alternative_text)
      input$text_alternative else
        input$dropdown
  }
}

#' Trigger button
#'
#' A version of \code{shiny::actionButton} that triggers
#' the next psychTestR page.

#' @param enable_after Number of seconds after which responses should be permitted.
#'
#' @param style CSS style information (character scalar).
#'
#' @inheritParams shiny::actionButton
#'
#' @export
trigger_button <- function(inputId, label, icon = NULL, width = NULL,
                           enable_after = 0, style = "",
                           ...) {
  checkmate::qassert(enable_after, "N1[0,)")
  inputId <- htmltools::htmlEscape(inputId, attribute = TRUE)
  shiny::tagList(
    shiny::actionButton(
      inputId = inputId, label = label,
      icon = icon, width = width,
      onclick = "trigger_button(this.id);",
      disabled = TRUE,
      style = style,
      ...),
    shiny::tags$script(
      sprintf("setTimeout(function() {
                 document.getElementById('%s').disabled = false;
               }, %i);",
              inputId, round(enable_after * 1e3))
    ))
}

#' New results section
#'
#' Returns a test element that initialises a new results section.
#'
#' @param label Label to give the new results section.
#' @export
new_results_section <- function(label) {
  stopifnot(is.scalar.character(label))
  code_block(function(state, ...) register_next_results_section(state, label))
}

#' Save results to disk (test element version)
#'
#' Returns a test element that saves the current participant's results to disk.
#'
#' @param complete Whether or not the participant
#' has now completed the test (scalar Boolean).
#'
#' @export
elt_save_results_to_disk <- function(complete) {
  stopifnot(is.scalar.logical(complete))
  code_block(function(state, opt, ...) {
    save_results_to_disk(complete, state, opt)
  })
}

#' Save results to disk
#'
#' Saves the current participant's results to disk.
#' This function can be called e.g. within a code block.
#' @param complete Whether or not the participant
#' has now completed the test (scalar Boolean).
#' @param state The participant's \code{state} object.
#' @param opt Test options as created by \code{test_options()}.
#' @param ... Further arguments are allowed but ignored (for back-compatibility).
#' @export
save_results_to_disk <- function(complete, state, opt, ...) {
  dir <- opt$results_dir
  R.utils::mkdirs(dir)
  if (!test_permissions(dir)) stop(
    "Insufficient permissions to write to directory ", dir, ".")
  if (!is.null(previous_save_path(state))) unlink(previous_save_path(state))
  filename <- save_results_to_disk.get_filename(state, dir, complete)
  path <- file.path(dir, filename)
  results <- get_results(state, complete = complete, add_session_info = TRUE)
  saveRDS(results, path)
  if (complete) notify_new_participant(opt)
  previous_save_path(state) <- path
  save_id(state) <- save_id(state) + 1L
}

save_results_to_disk.get_filename <- function(state, dir, complete) {
  sprintf(
    "id=%s&p_id=%s&save_id=%s&pilot=%s&complete=%s.rds",
    id = format(length(list_results_files(dir)) + 1L,
                scientific = FALSE),
    p_id = format(p_id(state), scientific = FALSE),
    save_id = format(save_id(state), scientific = FALSE),
    pilot = tolower(pilot(state)),
    complete = tolower(complete))
}

notify_new_participant <- function(opt) {
  enabled <- opt$notify_new_participant
  stopifnot(is.scalar.logical(enabled))
  if (enabled) {
    results <- tabulate_results(opt, include_pilot = FALSE)
    num_complete <- sum(results$complete)
    title <- sprintf("N = %i", num_complete)
    msg <- sprintf("Participant number %i%s just completed the experiment '%s'.",
                   num_complete,
                   if (!is.null(opt$max_num_participants)) {
                     sprintf("/%i", opt$max_num_participants)
                   } else "",
                   opt$title[1])
    async_pushbullet(title = title, body = msg, opt = opt)
  }
}

#' Loop
#'
#' Repeatedly show a series of test elements to the participant
#' while a condition is satisfied.
#' The elements are shown at least once; at the end of each pass,
#' the test function is checked.
#'
#' @param test Test function to execute.
#' Argument list should include \code{...};
#' further permitted arguments are:
#' - \code{state}, the participant's \code{state} object;
#' - \code{input}, the Shiny \code{input} object for the current page;
#' - \code{output}, the Shiny \code{output} object for the current page;
#' - \code{session}, the Shiny \code{session} object;
#' - \code{opt}, test options as created by \code{test_options()}.
#'
#' @param logic List of psychTestR test elements.
#'
#' @md
#' @name loop_while-deprecated
#' @seealso \code{\link{psychTestR-deprecated}}
#' @keywords internal
NULL

#' @rdname psychTestR-deprecated
#' @section \code{loop_while}:
#' We recommend that new projects use \code{\link{while_loop}},
#' which better resembles while loops in traditional programming languages.
#' @export
loop_while <- function(test, logic) {
  .Deprecated(new = "while_loop",
              package = "psychTestR")
  if (!is.function(test)) stop("`test` must be a function")
  if (!(is.list(logic) ||
        is.test_element(logic) ||
        is.timeline(logic))) {
    stop("`logic` must be either a test element, a list, or a timeline")
  }
  if (is.test_element(logic)) logic <- list(logic)
  if (length(logic) == 0L) stop("`logic` may not be empty")
  n <- length(logic)
  elt <- code_block(function(state, elts, input, output, session, opt, ...) {
    res <- test(state = state, input = input, output = output,
                session = session, opt = opt)
    if (!is.scalar.logical(res)) stop("`test` did not return a ",
                                      "logical scalar")
    if (res){
      skip_n_pages(state, - (n + 1L))
    }
  })
  join(logic, elt)
}

#' While loop
#'
#' This function creates a "while loop" in the participant's testing
#' session. This "while loop" corresponds to the following procedure:
#' 1. Check whether a condition (termed \code{test}) is satisfied.
#' 2. If the condition is satisfied,
#' execute a series of test elements (termed \code{logic}),
#' otherwise exit the while loop.
#' 3. Loop back to step 1.
#'
#' @note
#' The previous version of this function, \code{loop_while},
#' always executed the series of test elements at least once,
#' even if the condition was never satisfied.
#'
#' @param test Test function to execute.
#' Argument list should include \code{...};
#' further permitted arguments are:
#' \code{state}, the participant's \code{state} object;
#' \code{input}, the Shiny \code{input} object for the current page;
#' \code{output}, the Shiny \code{output} object for the current page;
#' \code{session}, the Shiny \code{session} object;
#' \code{opt}, test options as created by \code{test_options()}.
#'
#' @param logic List of psychTestR test elements.
#'
#' @md
#' @export
while_loop <- function(test, logic) {
  if (!is.function(test)) stop("`test` must be a function")
  if (!(is.list(logic) ||
        is.test_element(logic) ||
        is.timeline(logic))) {
    stop("`logic` must be either a test element, a list, or a timeline")
  }
  if (is.test_element(logic)) logic <- list(logic)
  if (length(logic) == 0L) stop("`logic` may not be empty")

  eval_test <- function(skip_len, skip_when) {
    code_block(function(state, elts, input, output, session, opt, ...) {
      res <- test(state = state, input = input, output = output,
                  session = session, opt = opt)
      if (!is.scalar.logical(res)) stop("`test` did not return a ",
                                        "logical scalar")
      if ((skip_when == "pass" && res) ||
          (skip_when == "fail" && !res))
        skip_n_pages(state, skip_len)
    })
  }

  n <- length(logic)

  join(
    eval_test(skip_len = n + 1, skip_when = "fail"),
    logic,
    eval_test(skip_len = - (n + 1), skip_when = "pass")
  )
}

#' Conditional test block
#'
#' This function evalutes a test function at run time to decide
#' whether to administer a series of test elements to the participant.
#' If the test returns \code{TRUE}, then the test elements are shown,
#' otherwise these test elements are skipped.
#'
#' @param test Function to evaluate at run time.
#' This function must accept the argument \code{...},
#' and optionally the following named arguments:
#' - \code{state}, the participant's state object;
#' - \code{input}, the current page's Shiny input object;
#' - \code{output}, the current page's Shiny output object;
#' - \code{session}, the current Shiny session object;
#' - \code{opt}, the test's option list as created by \code{\link{test_options}}.
#'
#' @param logic
#' Either a single test element, a list of test elements, or a timeline,
#' which will be displayed conditionally on the basis of the outcome
#' of \code{test}.
#'
#' @return
#' A list of test elements, or equivalently a timeline, which can be combined
#' with other test elements or timelines.
#'
#' @md
#' @export
conditional <- function(test, logic) {
  if (!is.function(test)) stop("`test` must be a function")
  if (!(is.list(logic) ||
        is.test_element(logic) ||
        is.timeline(logic))) {
    stop("`logic` must be either a test element, a list, or a timeline")
  }
  if (is.test_element(logic)) logic <- list(logic)
  if (length(logic) == 0L) stop("`logic` may not be empty")
  if (is.list(logic)) {
    if (!all(purrr::map_lgl(logic, is.test_element))) {
      stop("If `conditional()` is passed a list, every list element must be a test element.")
    }
  }

  n <- length(logic)

  eval_test <- code_block(function(state, elts, input, output, session, opt, ...) {
    res <- test(state = state, input = input, output = output,
                session = session, opt = opt)
    if (!is.scalar.logical(res)) stop("`test` did not return a ",
                                      "logical scalar")
    if (!res) {
      skip_n_pages(state, n)
    }
  })

  join(
    eval_test,
    logic
  )
}


#' Create module
#'
#' In psychTestR, modules are ways of wrapping sequences of test elements
#' into coherent logical units.
#' Putting a sequence of test elements into a module has three main consequences:
#' 1. Readability: It makes it clear to the reader that this sequence
#' of test elements forms a single logical unit.
#' 2. Results organisation:
#' Any results generated in this module will be assigned to a special
#' section in the psychTestR results object,
#' labelled with the name of the module.
#' 3. Protected local environment:
#' The module will receive a fresh local environment where it can create
#' its own local variables (see \code{\link{set_local}}).
#' This local environment is protected from other modules,
#' which is useful to avoid unexpected side effects when
#' multiple modules are chained together.
#'
#' In many cases modules will typically be used in one flat layer.
#' However, it is perfectly possible to nest modules to arbitrary depths;
#' at any point in time, only the local variables from the lowest-level module
#' will be visible.
#' The results object will use a composite label derived
#' by concatenating the names of the modules, separated by periods,
#' for example \code{parent.child.grandchild}.
#'
#' @md
#'
#' @inheritParams begin_module
#'
#' @param ... The psychTestR test elements that will constitute the module.
#'
#' @seealso This function wraps the low-level functions
#' \code{\link{begin_module}} and \code{\link{end_module}}.
#'
#' @export
module <- function(label, ...) {
  join(
    begin_module(label),
    join(...),
    end_module()
  )
}

#' Begin module
#'
#' Returns a code block that begins a psychTestR module.
#' Modules have their own set of local variables.
#' They also have identifying labels that are stored alongside
#' result entries created during the module.
#'
#' @param label Label for the module; must be limited
#' to alphanumeric characters and underscores.
#'
#' @note Usually it is better to call \code{\link{module}} instead.
#'
#' @export
begin_module <- function(label) {
  stopifnot(is.scalar.character(label),
            grepl("^[A-Za-z0-9_]*$", label))
  code_block(function(state, ...) {
    enter_local_environment(state)
    set_local(".module", label, state, allow_dots = TRUE)
    results_label <- get_results_label(state)
    set_local(".results_label", results_label, state, allow_dots = TRUE)
    register_next_results_section(state, results_label)
  })
}

#' End module
#'
#' Returns a code block that ends a psychTestR module.
#' Modules have their own set of local variables.
#' They also have identifying labels that are stored alongside
#' result entries created during the module.
#'
#' @note Usually it is better to call \code{\link{module}} instead.
#'
#' @export
end_module <- function() {
  code_block(function(state, ...) {
    leave_local_environment(state)
    new_results_label <- get_local(".results_label", state)
    register_next_results_section(state, new_results_label)
  })
}

#' Finish test and give code
#'
#' psychTestR logic for finishing the current test
#' and giving the participant a randomly
#' generated code that can then be used for organising payment
#' (e.g. for Amazon Mechanical Turk).
#' @param researcher_email The researcher's email address
#' (given in case the participant has any further questions).
#' @export
finish_test_and_give_code <- function(researcher_email) {
  join(
    begin_module("finish"),
    code_block(function(state, opt, ...) {
      code <- generate_id(16)
      save_result(state, "code", code)
      set_local("code", code, state)
      save_results_to_disk(complete = TRUE,
                           state = state,
                           opt = opt)
    }),
    reactive_page(function(state, options, ...) {
      final_page(shiny::div(
        shiny::p("Thank you very much for participating in this study."),
        shiny::p("Your completion code is:",
                 shiny::strong(get_local("code", state))),
        shiny::p(sprintf(paste0(
          "If you have any further questions or feedback, please feel free to ",
          "contact the lead researcher at %s."), researcher_email))))}))
}
