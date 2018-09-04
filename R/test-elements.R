setOldClass("shiny.tag")
setOldClass("shiny.tag.list")
setOldClass("i18n_dict")

setClassUnion("function_or_null", members = c("function", "NULL"))
setClassUnion("character_or_null", members = c("character", "NULL"))
setClassUnion("shiny_tag_or_null", members = c("shiny.tag", "NULL"))
setClassUnion("i18n_dict_or_null", members = c("i18n_dict", "NULL"))

setClass("test_element", slots = list(i18n_dict = "i18n_dict_or_null"))
setMethod("initialize", "test_element", function(.Object, ...) {
  .Object@i18n_dict <- I18N_STATE$dict
  callNextMethod()
})

#' @export
is.test_element <- function(x) is(x, "test_element")

#' @export
c.test_element <- function(...) {
  input <- list(...)
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
#' @export
reactive_page <- function(fun) {
  new("reactive_page", fun = fun)
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
#' @export
code_block <- function(fun) {
  new("code_block", fun = fun)
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
    shiny::includeCSS(system.file(shinythemes::shinytheme("readable"),
                                  package = "shinythemes")),
    shiny::fluidRow(shiny::wellPanel(
      shiny::h3("<App title>", align = "center"))),
    shiny::fluidRow(
      id = "content",
      shiny::column(2),
      shiny::column(8, shiny::wellPanel(align = "center", object@ui)),
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
#' @param ui Page UI. Can be either a scalar character (e.g.
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
#' @export
page <- function(ui, admin_ui = NULL, label = NULL,
                 final = FALSE, get_answer = NULL,
                 save_answer = FALSE, validate = NULL, on_complete = NULL) {
  ui <- tagify(ui)
  stopifnot(
    is(ui, "shiny.tag"),
    is.null(admin_ui) || is(admin_ui, "shiny.tag") || is.scalar.character(admin_ui),
    is.null.or(label, is.scalar.character),
    is.scalar.logical(final),
    is.null.or(get_answer, is.function),
    is.scalar.logical(save_answer),
    is.null.or(validate, is.function),
    is.null.or(on_complete, is.function))
  if (save_answer && !is.scalar.character(label))
    stop("if save_answer is TRUE then a scalar character label ",
         "must be provided")
  if (is.scalar.character(admin_ui)) admin_ui <- tagify(admin_ui)
  new("page", ui = ui, admin_ui = admin_ui, label = label, final = final,
      get_answer = get_answer,
      save_answer = save_answer, validate = validate, on_complete = on_complete)
}

#' New one-button page
#'
#' Creates a page with a prompt and one button which, when clicked,
#' advances to the next page.
#' This is typically used for giving the participant information
#' about the test.
#' @param body Page body. Can be either a scalar character (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#' @param admin_ui See \code{\link{page}}.
#' @param button_text Text to display on the button.
#' Should be a scalar character vector.
#' @param on_complete See \code{\link{page}}.
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
#' @param body Page body. Can be either a scalar character (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#' @param admin_ui See \code{\link{page}}.
#' @export
final_page <- function(body, admin_ui = NULL) {
  body <- tagify(body)
  page(ui = body, admin_ui = admin_ui, final = TRUE)
}

#' Text input page
#'
#' Creates a page where the participant puts their
#' answer in a text box.
#' @param label Label for the current page (character scalar).
#' @param prompt Prompt to display (character scalar or Shiny tag object)
#' @param save_answer See \code{\link{page}}.
#' @param placeholder Placeholder text for the text box (character scalar).
#' @param button_text Text for the submit button (character scalar).
#' @param width Width of the text box (character scalar, should be valid HTML).
#' @param height Height of the text box (character scalar, should be valid HTML).
#' @param validate See \code{\link{page}}.
#' @param on_complete See \code{\link{page}}.
#' @param admin_ui See \code{\link{page}}.
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

#' Get participant ID
#'
#' A psychTestR page that gets the participant to enter their ID.
#' @param prompt Prompt to display (character scalar or Shiny tag object)
#' @param placeholder Placeholder text for the text box (character scalar).
#' @param button_text Text for the submit button (character scalar).
#' @param width Width of the text box (character scalar, should be valid HTML).
#' @param validate See \code{\link{page}}.
#' @param admin_ui See \code{\link{page}}.
#' @note Participant IDs must be between 1 and 100 characters long,
#' and solely comprise alphanumeric characters and underscores.
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
       admin_ui = admin_ui)
}

get_p_id.on_complete <- function(state, input, session, opt, ...) {
  p_id <- answer(state)
  try_resume_session(p_id, state, session, opt,
                     ask_to_confirm_resume = TRUE,
                     reset_if_resume_fails = FALSE)
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
#' Creates an n-alternative forced-foced choice page.
#' @param label Label for the current page (character scalar).
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a scalar character (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for button IDs
#' and for button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#' @param save_answer See \code{\link{page}}.
#' @param arrange_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @param hide_response_ui Whether to begin with the response interface hidden
#' (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#' @param response_ui_id HTML ID for the response user interface.
#' @param on_complete See \code{\link{page}}.
#' @param admin_ui See \code{\link{page}}.
#' @export
NAFC_page <- function(label, prompt, choices, labels = NULL,
                      save_answer = TRUE,
                      arrange_vertically = length(choices) > 2L,
                      hide_response_ui = FALSE,
                      response_ui_id = "response_ui",
                      on_complete = NULL,
                      admin_ui = NULL) {
  stopifnot(is.scalar.character(label),
            is.character(choices), length(choices) > 0L,
            is.scalar.logical(arrange_vertically))
  ui <- shiny::div(
    tagify(prompt),
    make_ui_NAFC(choices,
                 labels = labels,
                 hide = hide_response_ui,
                 arrange_vertically = arrange_vertically,
                 id = response_ui_id))
  get_answer <- function(input, ...) input$last_btn_pressed
  validate <- function(answer, ...) !is.null(answer)
  page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}

#' Make NAFC buttons
#'
#' Creates HTML code for n-alternative forced-choice response options.
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for button IDs
#' and for button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#' @param hide Whether the response buttons should be hidden
#' (possibly to be shown later).
#' @param arrange_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @param id HTML ID for the div containing the response buttons.
#' @export
make_ui_NAFC <- function(choices, labels = NULL, hide = FALSE,
                         arrange_vertically = length(choices) > 2L,
                         id = "response_ui") {
  stopifnot(is.character(choices), length(choices) > 0L, is.scalar.logical(hide),
            is.null.or(labels, is.character))
  if (is.null(labels)) {
    labels <- if (is.null(names(choices))) choices else names(choices)
  }
  shiny::tags$div(id = id,
                  style = if (hide) "visibility: hidden" else "visibility: inherit",
                  mapply(function(id, label) {
                    trigger_button(inputId = id, label = label)
                  }, choices, labels, SIMPLIFY = F, USE.NAMES = F) %>%
                    (function(x) if (arrange_vertically) lapply(x, shiny::tags$p) else x))
}

#' Make NAFC video page
#'
#' Creates an n-alternative forced-foced choice page with a video prompt.
#' @param label Label for the current page (character scalar).
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a scalar character (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for button IDs
#' and for button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#' @param url URL to the video.
#' Can be an absolute URL (e.g. "http://mysite.com/video.mp4")
#' or a URL relative to the /www directory (e.g. "video.mp4").
#' @param type Video type (e.g. 'mp4'). Defaults to the provided file extension.
#' @param save_answer See \code{\link{page}}.
#' @param on_complete See \code{\link{page}}.
#' @param video_width Video width, as passed to HTML (e.g. '50px').
#' @param arrange_choices_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @param wait Whether to wait for the video to finish before displaying
#' the response buttons.
#' @param loop Whether the video should loop.
#' @param admin_ui See \code{\link{page}}.
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
                            admin_ui = NULL) {
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
      onended = if (wait) media.js$show_responses else "null"),
    media_mobile_play_button)
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

media_mobile_play_button <- shiny::tags$p(
  shiny::tags$strong("Click here to play"),
  id = "btn_play_media",
  style = "visibility: hidden",
  onclick = media.js$play_media)

#' Make NAFC audio page
#'
#' Creates an n-alternative forced-foced choice page with an audio prompt.
#' @param label Label for the current page (character scalar).
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a scalar character (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for button IDs
#' and for button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#' @param url URL to the audio
#' Can be an absolute URL (e.g. "http://mysite.com/audio.mp3")
#' or a URL relative to the /www directory (e.g. "audio.mp3").
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#' @param save_answer See \code{\link{page}}.
#' @param on_complete See \code{\link{page}}.
#' @param arrange_choices_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @param wait Whether to wait for the audio to finish before displaying
#' the response buttons.
#' @param loop Whether the audio should loop.
#' @param admin_ui See \code{\link{page}}.
#' @export
audio_NAFC_page <- function(label, prompt, choices, url,
                            labels = NULL,
                            type = tools::file_ext(url),
                            save_answer = TRUE,
                            on_complete = NULL,
                            arrange_choices_vertically = length(choices) > 2L,
                            wait = TRUE, loop = FALSE,
                            admin_ui = NULL) {
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
    autoplay = "autoplay",
    loop = if (loop) "loop",
    oncanplaythrough = media.js$show_media_btn,
    onplay = paste0(media.js$media_played, media.js$hide_media_btn),
    onended = if (wait) media.js$show_responses else "null"
  ), media_mobile_play_button)
  prompt2 <- shiny::div(tagify(prompt), audio_ui)
  NAFC_page(label = label, prompt = prompt2, choices = choices, labels = labels,
            save_answer = save_answer,
            on_complete = on_complete,
            arrange_vertically = arrange_choices_vertically,
            hide_response_ui = wait, response_ui_id = "response_ui",
            admin_ui = admin_ui)
}

#' Make volume calibration page
#'
#' Creates a page for the participant to calibrate their volume,
#' using example audio, and the volume controls on their computer.
#' @param url URL to the audio.
#' Can be an absolute URL (e.g. "http://mysite.com/audio.mp3")
#' or a URL relative to the /www directory (e.g. "audio.mp3").
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#' @param prompt Prompt to be displayed. If left \code{NULL},
#' a sensible English prompt is provided.
#' @param button_text Button text (scalar character).
#' @param admin_ui See \code{\link{page}}.
#' @export
volume_calibration_page <- function(url, type = tools::file_ext(url),
                                    prompt = NULL,
                                    button_text = "Next",
                                    on_complete = NULL,
                                    admin_ui = NULL) {
  if (is.null(prompt)) prompt <- shiny::div(
    shiny::p(
      "You should hear some audio playing.",
      "Please adjust the volume to a comfortable level before continuing."
    ),
    shiny::p(
      "If you cannot make the audio play at a comfortable level,",
      "please do not continue, but instead ask the researcher for help."
    )
  )
  audio_NAFC_page(label = "volume_calibration",
                  prompt = prompt, choices = button_text,
                  save_answer = FALSE,
                  on_complete = on_complete,
                  url = url, type = type,
                  wait = FALSE, loop = TRUE,
                  admin_ui = admin_ui)
}

#' Make dropdown list page
#'
#' Creates a page where the response is to be selected from a dropdown list.
#' @param label Page label (scalar character).
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a scalar character (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#' @param choices Character vector of choices for the participant.
#' If names are provided, then these names will be used for display,
#' whereas the values will be stored in the results.
#' @param alternative_choice Whether or not to give the participant
#' the option of providing a free-text response instead of
#' selecting one of the dropdown options.
#' @param alternative_text Prompt for the free-text box (only relevant
#' if \code{alternative_choice} is set to \code{TRUE}).
#' @param save_answer See \code{\link{page}}.
#' @param validate See \code{\link{page}}.
#' @param on_complete See \code{\link{page}}.
#' @param next_button_text Text to display on the next-page button
#' (character scalar).
#' @param max_width_pixels Maximum width of the response UI, in pixels.
#' @param admin_ui See \code{\link{page}}.
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
#' @param inputId See \link[shiny]{actionButton}
#' @param label See \link[shiny]{actionButton}
#' @param icon See \link[shiny]{actionButton}
#' @param width See \link[shiny]{actionButton}
#' @export
trigger_button <- function(inputId, label, icon = NULL, width = NULL) {
  inputId <- htmltools::htmlEscape(inputId, attribute = TRUE)
  shiny::actionButton(
    inputId = inputId, label = label,
    icon = icon, width = width,
    onclick = "trigger_button(this.id);")
}

#' New results section
#'
#' Returns a test element that initialises a new results section.
#' @param label Label to give the new results section.
#' @export
new_results_section <- function(label) {
  stopifnot(is.scalar.character(label))
  code_block(function(state, ...) register_next_results_section(state, label))
}

#' Save results to disk (test element version)
#'
#' Returns a test element that saves the current participant's results to disk.
#' @param complete Whether or not the participant
#' has now completed the test (scalar Boolean).
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
#' @param complete Whether the participant completed the test.
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
    id = format(length(list.files(dir, pattern = "\\.rds$")) + 1L,
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
                   opt$title)
    async_pushbullet(title = title, body = msg, opt = opt)
  }
}

#' Loop
#'
#' Repeatedly show a series of test elements to the participant
#' while a condition is satisfied.
#' The elements are shown at least once; at the end of each pass,
#' the test function is checked.
#' @param test Test function to execute.
#' Argument list should include \code{...};
#' further permitted arguments are:
#' \code{state}, the participant's \code{state} object;
#' \code{input}, the Shiny \code{input} object for the current page;
#' \code{output}, the Shiny \code{output} object for the current page;
#' \code{session}, the Shiny \code{session} object;
#' \code{opt}, test options as created by \code{test_options()}.
#' @param logic List of psychTestR test elements.
#' @export
loop_while <- function(test, logic) {
  if (!is.function(test)) stop("<test> must be a function")
  if (!(is.list(logic) || is(logic, "test_element"))) {
    stop("<logic> must be either a test element or a list")
  }
  if (!is.list(logic)) logic <- list(logic)
  if (length(logic) == 0L) stop("<logic> may not be empty")
  n <- length(logic)
  elt <- code_block(function(state, elts, input, output, session, opt, ...) {
    res <- test(state = state, input = input, output = output,
                session = session, opt = opt)
    if (!is.scalar.logical(res)) stop("<test> did not return a ",
                                      "scalar logical")
    if (!res) skip_n_pages(state, - (n + 1L))
  })
  c(logic, elt)
}

#' Begin module
#'
#' Returns a code block that begins a psychTestR module.
#' Modules have their own set of local variables.
#' They also have identifying labels that are stored alongside
#' result entries created during the module.
#' @param label Module label (scalar character).
#' @export
begin_module <- function(label) {
  stopifnot(is.scalar.character(label))
  code_block(function(state, ...) {
    enter_local_environment(state)
    register_next_results_section(state, label)
  })
}

#' End module
#'
#' Returns a code block that ends a psychTestR module.
#' Modules have their own set of local variables.
#' They also have identifying labels that are stored alongside
#' result entries created during the module.
#' @param label Module label (scalar character).
#' @export
end_module <- function() {
  code_block(function(state, ...) {
    leave_local_environment(state)
    register_next_results_section(state, "results")
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
  c(
    begin_module("finish"),
    code_block(function(state, ...) {
      code <- generate_id(16)
      save_result(state, "code", code)
      set_local("code", code, state)
      save_results_to_disk(complete = TRUE)
    }),
    reactive_page(function(state, options, ...) {
      final_page(shiny::div(
        shiny::p("Thank you very much for participating in this study."),
        shiny::p("Your completion code is:",
                 shiny::strong(get_local("code", state))),
        shiny::p(sprintf(
          "If you have any further questions or feedback, please feel free to",
          "contact the research team at %s.", researcher_email))))}))
}
