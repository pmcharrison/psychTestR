setOldClass("shiny.tag")
setOldClass("shiny.tag.list")
setClass("app_logic", slots = list(pages = "list")
         # You could validate here to check that there is a 
         # p_id slot
)

setClass("test_element")

setClass("page",
         slots = list(ui = "shiny.tag", # page UI
                      result = "character", # vector of results to save
                      triggers = "character", # inputs that trigger next page
                      final = "logical"), # whether page is final page or not
         contains = "test_element")

# one_btn_page shows a page with some content and 
# a 'Next' button.
setClass("one_btn_page",
         slots = list(body = "shiny.tag"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "one_btn_page",
  definition = function(.Object, body) {
    .Object@body <- body
    .Object@ui <- div(body, actionButton("next", "Next"))
    .Object@triggers <- "next"
    .Object@final <- FALSE
    return(.Object)
  }
)

# final_page shows a page with some content and no action buttons
setClass("final_page",
         slots = list(body = "shiny.tag"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "final_page",
  definition = function(.Object, body) {
    .Object@body <- body
    .Object@ui <- div(body)
    .Object@final <- TRUE
    return(.Object)
  }
)

# video_stimulus shows some content (typically text) with a video
# below it and a variable number of forced-choice response options
setClass("video_stimulus_NAFC",
         slots = list(prompt = "shiny.tag",
                      source = "character",
                      type = "character",
                      response_options = "character",
                      wait = "logical"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "video_stimulus_NAFC",
  definition = function(.Object, prompt, source, type, response_options, wait) {
    .Object@prompt <- prompt
    .Object@source <- source
    .Object@response_options <- response_options
    .Object@wait <- wait
    .Object@triggers <- response_options
    .Object@final <- FALSE
    
    video_ui <- tags$video(# tags$head(tags$script(src = "showResponseUI.js")),
                           tags$source(src = source,
                                       type = paste0("video/", type)),
                           id = "video_stimulus",
                           width = "50%",
                           autoplay = "autoplay",
                           onended = if (wait) {
                             "document.getElementById('response_UI').style.visibility = 'visible';"
                             } else "null")
    response_ui <- make_ui_NAFC(response_options, hidden = wait)
    
    .Object@ui <- div(prompt, video_ui, response_ui)
    .Object@triggers <- "next"
    .Object@final <- FALSE
    return(.Object)
  }
)

make_ui_NAFC <- function(response_options, hidden = FALSE) {
  assertthat::assert_that(is.character(response_options), is.logical(hidden))
  labels <- if (is.null(names(response_options))) {
    response_options
  } else {
    names(response_options)
  }
  withTags(
    div(id = "response_UI",
        style = if (hidden) "visibility: hidden" else "visibility: visible",
        mapply(function(id, label) {
          actionButton(inputId = id, label = label)
        }, response_options, labels, SIMPLIFY = F, USE.NAMES = F)))
}
