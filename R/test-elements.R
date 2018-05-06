setOldClass("shiny.tag")
setOldClass("shiny.tag.list")

setClass("test_element")

setClassUnion("function_or_null", members = c("function", "NULL"))

setClass("page",
         slots = list(ui = "shiny.tag",
                      final = "logical",
                      get_answer = "function_or_null",
                      on_complete = "function_or_null",
                      validate = "function_or_null"),
         contains = "test_element")

setMethod(
  "show",
  signature(object = "page"),
  definition = function(object) {
    cat("\npsychTest page\n")
    htmltools::html_print(shiny::div(
      shiny::includeCSS(system.file(shinythemes::shinytheme("readable"),
                                    package = "shinythemes")),
      shiny::fluidRow(shiny::wellPanel(shiny::h3("<App title>", align = "center"))),
      shiny::fluidRow(
        id = "content",
        shiny::column(2),
        shiny::column(8, shiny::wellPanel(align = "center", object@ui)),
        shiny::column(2)
      )))})

setClass("reactive_page",
         slots = list(fun = "function"),
         prototype = list(fun = function(state) page),
         contains = "test_element")

#' Careful - function must be idempotent (calling it multiple times should
#' have the same effect as calling it once).
#' @export
reactive_page <- function(fun) {
  new("reactive_page", fun = fun)
}

setClass("code_block",
         slots = list(fun = "function"),
         contains = "test_element",
         prototype = list(fun = function(...) NULL))

#' @export
code_block <- function(fun) {
  new("code_block", fun = fun)
}

#' New page
#'
#' This is the most general way to create a psychTest page.
#' @param ui Page UI. Can be either a scalar character (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#' @param final Whether or not the page is the final page in the test.
#' @param on_complete Optional function to run when page completes.
#' Should take the following form: function(state, input) ...
#' where \code{state} is the app's persistent state (a Shiny reactiveValues object)
#' and \code{input} is the input produced by the page UI.
#' @param validate Optional validation function.
#' Should take the same form as \code{on_complete}, and return
#' \code{TRUE} for a successful validation and \code{FALSE}
#' for an unsuccessful validation.
#' If validation fails then the page will be refreshed, usually
#' to give the user a chance to revise their input.
#' @export
page <- function(ui, final = FALSE, get_answer = NULL, on_complete = NULL,
                 validate = NULL) {
  ui <- tagify(ui)
  stopifnot(
    is.scalar.logical(final),
    is.null.or(get_answer, is.function),
    is.null.or(on_complete, is.function),
    is.null.or(validate, is.function))
  new("page", ui = ui, final = final, get_answer = get_answer,
      on_complete = on_complete, validate = validate)
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
#' @param button_text Text to display on the button.
#' Should be a scalar character vector.
#' @export
one_button_page <- function(body, button_text = "Next") {
  body <- tagify(body)
  stopifnot(is.scalar.character(button_text))
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui)
}

#' New final page
#'
#' Creates a page that concludes the test.
#' @param body Page body. Can be either a scalar character (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#' @export
final_page <- function(body) {
  body <- tagify(body)
  page(ui = body, final = TRUE)
}

#' @export
text_input_page <- function(label, prompt,
                            placeholder = NULL,
                            button_text = "Next",
                            width = "300px",
                            validate = NULL,
                            on_complete = text_input_page.autosave(label, prompt)) {
  stopifnot(is.scalar.character(label))
  text_input <- shiny::textInput("text_input", label = NULL,
                                 placeholder = placeholder,
                                 width = width)
  get_answer <- function(input, ...) input$text_input
  body = shiny::div(tagify(prompt), text_input)
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, get_answer = get_answer, validate = validate,
       on_complete = on_complete)
}

#' @export
text_input_page.autosave <- function(label, prompt) {
  function(state, ...) {
    metadata <- list(type = "text_input_page", prompt = prompt)
    save_result(place = state, label = label, value = answer(state),
                metadata = metadata)
  }
}

#' @export
get_p_id_page <- function(prompt = "Please enter your participant ID.",
                          placeholder = "e.g. 10492817",
                          button_text = "Next",
                          width = "300px",
                          validate = "auto") {
  validate_2 <- get_p_id_page.validate(validate)
  get_answer <- function(input, ...) input$p_id
  on_complete <- get_p_id_page.on_complete
  text_input <- shiny::textInput("p_id", label = NULL,
                                 placeholder = placeholder,
                                 width = width)
  body = shiny::div(tagify(prompt), text_input)
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, get_answer = get_answer,
       validate = validate_2,
       on_complete = on_complete)
}

get_p_id_page.on_complete <- function(state, input, session, options, ...) {
  p_id <- input$p_id
  try_resume_session(p_id, state, session, options,
                     ask_to_confirm_resume = FALSE,
                     reset_if_resume_fails = FALSE)
}

get_p_id_page.validate <- function(validate) {
  if (is.function(validate)) {
    validate
  } else if (identical(validate, "auto")) {
    function(state, input, ...) {
      valid <- nchar(input$p_id) > 0L
      if (valid) TRUE else "Please enter your participant ID before proceeding."
    }
  } else stop("Unrecognised validation method.")
}

#' New NAFC page
#'
#' Creates an n-alternative forced-foced choice page.
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a scalar character (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for button IDs
#' and for button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#' @param arrange_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @param ... Further parameters to be passed to \code{\link{page}}.
#' @export
NAFC_page <- function(label, prompt, choices,
                      on_complete = NAFC_page.autosave(label, prompt),
                      arrange_vertically = TRUE,
                      hide_response_ui = FALSE,
                      response_ui_id = "response_ui",
                      ...) {
  stopifnot(is.scalar.character(label),
            is.character(choices), length(choices) > 0L,
            is.scalar.logical(arrange_vertically))
  ui <- shiny::div(
    tagify(prompt),
    make_ui_NAFC(choices,
                 hide = hide_response_ui,
                 arrange_vertically = arrange_vertically,
                 id = response_ui_id))
  get_answer <- function(input, ...) input$last_btn_pressed
  page(ui = ui, get_answer = get_answer, on_complete = on_complete,
       final = FALSE)
}

NAFC_page.autosave <- function(label, prompt) {
  function(state, ...) {
    metadata <- list(type = "NAFC", prompt = prompt)
    save_result(place = state, label = label, value = answer(state),
                metadata = metadata)
  }
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
#' @param id HTML ID for the div containing the response buttons.
#' @param arrange_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @export
make_ui_NAFC <- function(choices, hide = FALSE, arrange_vertically = TRUE,
                         id = "response_ui") {
  stopifnot(is.character(choices), length(choices) > 0L, is.scalar.logical(hide))
  labels <- if (is.null(names(choices))) choices else names(choices)
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
#' @param video_width Video width, as passed to HTML (e.g. '50px').
#' @param arrange_choices_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @param wait Whether to wait for the video to finish before displaying
#' the response buttons.
#' @param loop Whether the video should loop.
#' @param ... Further parameters to be passed to \code{\link{page}}.
#' @export
video_NAFC_page <- function(label, prompt, choices, url,
                            on_complete = video_NAFC_page.autosave(label, prompt, url),
                            type = tools::file_ext(url),
                            video_width = "100%",
                            arrange_choices_vertically = TRUE,
                            wait = TRUE,
                            loop = FALSE,
                            ...) {
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
      oncanplaythrough = media.js$show_video_btn,
      onplay = media.js$media_played,
      autoplay = "autoplay", style = "max-width: 500px",
      playsinline = "playsinline", onplay = media.js$hide_media_btn,
      if (loop) "loop",
      onended = if (wait) media.js$show_responses else "null"),
    media_mobile_play_button)
  prompt2 <- shiny::div(tagify(prompt), video_ui)
  NAFC_page(label = label, prompt = prompt2, choices = choices, on_complete = on_complete,
            arrange_vertically = arrange_choices_vertically,
            hide_response_ui = wait, response_ui_id = "response_ui", ...)
}

video_NAFC_page.autosave <- function(label, prompt, url) {
  function(state, ...) {
    metadata <- list(type = "video_NAFC", prompt = prompt, url = url)
    save_result(place = state, label = label, value = answer(state),
                metadata = metadata)
  }
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
#' @param arrange_choices_vertically Whether to arrange the response buttons vertically
#' (the default) as opposed to horizontally.
#' @param wait Whether to wait for the audio to finish before displaying
#' the response buttons.
#' @param loop Whether the audio should loop.
#' @param ... Further parameters to be passed to \code{\link{page}}.
#' @export
audio_NAFC_page <- function(label, prompt, choices, url,
                            type = tools::file_ext(url),
                            on_complete = audio_NAFC_page.autosave(label, prompt, url),
                            arrange_choices_vertically = TRUE,
                            wait = TRUE, loop = FALSE, ...) {
  stopifnot(is.scalar.character(label),
            is.character(choices), is.scalar.character(url),
            is.scalar.character(url),
            is.scalar.logical(arrange_choices_vertically),
            is.scalar.logical(wait), is.scalar.logical(loop))
  audio_ui <- shiny::tags$audio(
    shiny::tags$source(src = url, type = paste0("audio/", type)),
    id = "audio_stimulus", preload = "auto",
    autoplay = "autoplay",
    loop = if (loop) "loop",
    onended = if (wait) media.js$show_responses else "null")
  prompt2 <- shiny::div(tagify(prompt), audio_ui)
  NAFC_page(label = label, prompt = prompt2, choices = choices, on_complete = on_complete,
            arrange_vertically = arrange_choices_vertically,
            hide_response_ui = wait, response_ui_id = "response_ui", ...)
}

audio_NAFC_page.autosave <- function(label, prompt, url) {
  function(state, ...) {
    metadata <- list(type = "audio_NAFC", prompt = prompt, url = url)
    save_result(place = state, label = label, value = answer(state),
                metadata = metadata)
  }
}

#' Make volume calibration page
#'
#' Creates a page for the participant to calibrate their volume,
#' using example audio, and the volume controls on their computer.
#' @param prompt Prompt to be displayed. If left \code{NULL},
#' a sensible English prompt is provided.
#' @param url URL to the audio
#' Can be an absolute URL (e.g. "http://mysite.com/audio.mp3")
#' or a URL relative to the /www directory (e.g. "audio.mp3").
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#' @param ... Further parameters to be passed to \code{\link{page}}.
#' @export
volume_calibration_page <- function(url, type = tools::file_ext(url),
                                    prompt = NULL,
                                    button_text = "Next", ...) {
  if (is.null(prompt)) prompt <- paste0(
    "You should hear some audio playing. ",
    "Please adjust the volume to a comfortable level before continuing."
  )
  audio_NAFC_page(prompt = prompt, choices = button_text,
                  url = url, type = type, on_complete = NULL,
                  wait = FALSE, loop = TRUE, ...)
}

#' Make dropdown list page
#'
#' Creates a page where the response is to be selected from a dropdown list.
#' @param ... Further parameters to be passed to \code{\link{page}}.
#' @export
dropdown_page <- function(label, prompt, choices,
                          alternative_choice = FALSE,
                          alternative_text = "Other (please state)",
                          on_complete = dropdown_page.autosave(
                            label, prompt, alternative_text),
                          validate = dropdown_page.validate(alternative_choice,
                                                            alternative_text),
                          next_button_text = "Next",
                          max_width_pixels = 200,
                          ...) {
  stopifnot(is.scalar.character(label),
            is.character(choices),
            is.scalar.logical(alternative_choice),
            is.scalar.character(alternative_text),
            is.scalar.numeric(max_width_pixels),
            is.null.or(on_complete, is.function),
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
  page(ui = ui, get_answer = get_answer, on_complete = on_complete,
       validate = validate, final = FALSE)
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
    if (input$dropdown == alternative_text) input$text_alternative else input$dropdown
  }
}

dropdown_page.autosave <- function(label, prompt, alternative_text) {
  function(state, input, ...) {
    metadata <- list(type = "dropdown_page", prompt = prompt)
    save_result(place = state, label = label, value = answer(state), metadata = metadata)
  }
}

# Version of actionButton that also triggers the next page
#' @export
trigger_button <- function(inputId, label, icon = NULL, width = NULL, ...) {
  inputId <- htmltools::htmlEscape(inputId, attribute = TRUE)
  shiny::actionButton(
    inputId = inputId, label = label,
    icon = icon, width = width,
    onclick = "trigger_button(this.id);")
}

#' @export
new_results_section <- function(label) {
  stopifnot(is.scalar.character(label))
  code_block(function(state, ...) register_next_results_section(state, label))
}


# code_block.save_data <- function(mode = "local")

#' @export
save_results_to_disk <- function(final) {
  stopifnot(is.scalar(final))
  code_block(function(state, options, ...) {
    dir <- options$results_dir
    R.utils::mkdirs(dir)
    if (!test_permissions(dir)) {
      stop("Insufficient permissions to write to directory ", dir, ".")
    }
    id <- length(list.files(dir, pattern = "\\.rds$")) + 1L
    p_id <- p_id(state)
    save_id <- save_id(state)
    previous_save_path <- previous_save_path(state)
    if (!is.null(previous_save_path)) unlink(previous_save_path)
    filename <- sprintf("id=%s&p_id=%s&save_id=%s&final=%s.rds",
                        format(id, scientific = FALSE),
                        format(p_id),
                        format(save_id, scientific = FALSE),
                        tolower(final))
    path <- file.path(dir, filename)
    results <- get_results(state, add_session_info = TRUE)
    saveRDS(results, path)
    previous_save_path(state) <- path
    save_id(state) <- save_id(state) + 1L
  })
}

#' @export
loop_while <- function(test, logic) {
  if (!is.function(test)) stop("<test> must be a function")
  if (!is.list(logic) || !is(logic, "test_element")) {
    stop("<logic> must be either a test element or a list")
  }
  if (!is.list(logic)) logic <- list(logic)
  if (length(logic) == 0L) stop("<logic> may not be empty")
  n <- length(logic)
  elt <- code_block(function(state, elts, input, output, session, options) {
    res <- test(state = state, input = input, output = output,
                session = session, options = options)
    if (!is.scalar.logical(res)) stop("<test> did not return a ",
                                      "scalar logical")
    if (!res) skip_n_pages(state, - (n + 1L))
  })
  c(logic, elt)
}

#' @export
begin_module <- function() {
  code_block(function(state, ...) {
    enter_local_environment(state)
  })
}

#' @export
end_module <- function() {
  code_block(function(state, ...) {
    leave_local_environment(state)
  })
}
