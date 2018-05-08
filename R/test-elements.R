setOldClass("shiny.tag")
setOldClass("shiny.tag.list")

setClass("test_element")

setClassUnion("function_or_null", members = c("function", "NULL"))
setClassUnion("character_or_null", members = c("character", "NULL"))

setClass("page",
         slots = list(ui = "shiny.tag",
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

setMethod(
  "show",
  signature(object = "page"),
  definition = function(object) {
    cat("psychTest page\n")
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

setMethod(
  "show",
  signature(object = "code_block"),
  definition = function(object) {
    cat("psychTest code block\n")
    print(object@fun)
  }
)

setMethod(
  "show",
  signature(object = "reactive_page"),
  definition = function(object) {
    cat("psychTest reactive page\n")
    print(object@fun)
    print(final_page(shiny::em("reactive page")))
  }
)

#' New page
#'
#' This is the most general way to create a psychTest page.
#' @param ui Page UI. Can be either a scalar character (e.g.
#' "Welcome to the test!") or an object of class "shiny.tag",
#' e.g. \code{shiny::tags$p("Welcome to the test!")}.
#' @param final Whether or not the page is the final page in the test.
#' Should take the following form: function(state, input) ...
#' where \code{state} is the app's persistent state (a Shiny reactiveValues object)
#' and \code{input} is the input produced by the page UI.
#' @param validate Optional validation function.
#' Should return \code{TRUE} for a successful validation and \code{FALSE}
#' for an unsuccessful validation.
#' If validation fails then the page will be refreshed, usually
#' to give the user a chance to revise their input.
#' @export
page <- function(ui, label = NULL, final = FALSE, get_answer = NULL,
                 save_answer = FALSE, validate = NULL, on_complete = NULL) {
  ui <- tagify(ui)
  stopifnot(
    is.null.or(label, is.scalar.character),
    is.scalar.logical(final),
    is.null.or(get_answer, is.function),
    is.scalar.logical(save_answer),
    is.null.or(validate, is.function),
    is.null.or(on_complete, is.function))
  if (save_answer && !is.scalar.character(label))
    stop("if save_answer is TRUE then a scalar character label ",
         "must be provided")
  new("page", ui = ui, label = label, final = final, get_answer = get_answer,
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
#' @param button_text Text to display on the button.
#' Should be a scalar character vector.
#' @export
one_button_page <- function(body, button_text = "Next", on_complete = NULL) {
  body <- tagify(body)
  stopifnot(is.scalar.character(button_text))
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, on_complete = on_complete)
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
                            one_line = TRUE,
                            save_answer = TRUE,
                            placeholder = NULL,
                            button_text = "Next",
                            width = "300px",
                            height = "100px", # only relevant if one_line == FALSE
                            validate = NULL,
                            on_complete = NULL) {
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
  body = shiny::div(tagify(prompt), text_input)
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, label = label, get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE)
}

#' @export
get_p_id <- function(prompt = "Please enter your participant ID.",
                     placeholder = "e.g. 10492817",
                     button_text = "Next",
                     width = "300px",
                     validate = "auto") {
  get_answer <- function(input, ...) input$p_id
  text_input <- shiny::textInput("p_id", label = NULL,
                                 placeholder = placeholder,
                                 width = width)
  body = shiny::div(tagify(prompt), text_input)
  ui <- shiny::div(body, trigger_button("next", button_text))
  page(ui = ui, get_answer = get_answer, save_answer = FALSE,
       validate = get_p_id.validate(validate),
       on_complete = get_p_id.on_complete)
}

get_p_id.on_complete <- function(state, input, session, opt, ...) {
  p_id <- answer(state)
  try_resume_session(p_id, state, session, opt,
                     ask_to_confirm_resume = FALSE,
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
#' @export
NAFC_page <- function(label, prompt, choices,
                      save_answer = TRUE,
                      arrange_vertically = length(choices) > 2L,
                      hide_response_ui = FALSE,
                      response_ui_id = "response_ui",
                      on_complete = NULL) {
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
  validate <- function(answer, ...) !is.null(answer)
  page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
       validate = validate, on_complete = on_complete, final = FALSE)
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
make_ui_NAFC <- function(choices, hide = FALSE,
                         arrange_vertically = length(choices) > 2L,
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
#' @export
video_NAFC_page <- function(label, prompt, choices, url,
                            type = tools::file_ext(url),
                            save_answer = TRUE,
                            on_complete = NULL,
                            video_width = "100%",
                            arrange_choices_vertically = length(choices) > 2L,
                            wait = TRUE,
                            loop = FALSE) {
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
      loop = if (loop) "loop",
      onended = if (wait) media.js$show_responses else "null"),
    media_mobile_play_button)
  prompt2 <- shiny::div(tagify(prompt), video_ui)
  NAFC_page(label = label, prompt = prompt2, choices = choices,
            save_answer = save_answer,
            on_complete = on_complete,
            arrange_vertically = arrange_choices_vertically,
            hide_response_ui = wait, response_ui_id = "response_ui")
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
#' @export
audio_NAFC_page <- function(label, prompt, choices, url,
                            type = tools::file_ext(url),
                            save_answer = TRUE,
                            on_complete = NULL,
                            arrange_choices_vertically = length(choices) > 2L,
                            wait = TRUE, loop = FALSE) {
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
  NAFC_page(label = label, prompt = prompt2, choices = choices,
            save_answer = save_answer,
            on_complete = on_complete,
            arrange_vertically = arrange_choices_vertically,
            hide_response_ui = wait, response_ui_id = "response_ui")
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
#' @export
volume_calibration_page <- function(url, type = tools::file_ext(url),
                                    prompt = NULL,
                                    button_text = "Next",
                                    on_complete = NULL) {
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
                  wait = FALSE, loop = TRUE)
}

#' Make dropdown list page
#'
#' Creates a page where the response is to be selected from a dropdown list.
#' @export
dropdown_page <- function(label, prompt, choices,
                          alternative_choice = FALSE,
                          alternative_text = "Other (please state)",
                          save_answer = TRUE,
                          validate = dropdown_page.validate(alternative_choice,
                                                            alternative_text),
                          on_complete = NULL,
                          next_button_text = "Next",
                          max_width_pixels = 200) {
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
       validate = validate, on_complete = on_complete, final = FALSE)
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

# Version of actionButton that also triggers the next page
#' @export
trigger_button <- function(inputId, label, icon = NULL, width = NULL) {
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

#' @export
#' @param complete Whether the participant completed the test.
elt_save_results_to_disk <- function(complete) {
  stopifnot(is.scalar.logical(complete))
  code_block(function(state, opt, ...) {
    save_results_to_disk(complete, state, opt)
  })
}

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

#' @export
loop_while <- function(test, logic) {
  if (!is.function(test)) stop("<test> must be a function")
  if (!(is.list(logic) || is(logic, "test_element"))) {
    stop("<logic> must be either a test element or a list")
  }
  if (!is.list(logic)) logic <- list(logic)
  if (length(logic) == 0L) stop("<logic> may not be empty")
  n <- length(logic)
  elt <- code_block(function(state, elts, input, output, session, opt) {
    res <- test(state = state, input = input, output = output,
                session = session, opt = opt)
    if (!is.scalar.logical(res)) stop("<test> did not return a ",
                                      "scalar logical")
    if (!res) skip_n_pages(state, - (n + 1L))
  })
  c(logic, elt)
}

#' @export
begin_module <- function(label) {
  stopifnot(is.scalar.character(label))
  code_block(function(state, ...) {
    enter_local_environment(state)
    register_next_results_section(state, label)
  })
}

#' @export
end_module <- function() {
  code_block(function(state, ...) {
    leave_local_environment(state)
    register_next_results_section(state, "results")
  })
}

#' @export
finish_test_and_give_code <- function(researcher_email) {
  c(
    begin_module("finish"),
    code_block(function(state, ...) {
      code <- shiny:::createUniqueId(16)
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
