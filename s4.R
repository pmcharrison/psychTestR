setOldClass("shiny.tag.list")
setClass("app_logic", slots = list(pages = "list")
         # You could validate here to check that there is a 
         # p_id slot
)

setClass("test_element")

setClass("test_page",
         slots = list(ui = "shiny.tag.list", # page UI
                      result = "character", # vector of results to save
                      triggers = "character", # inputs that trigger next page
                      final = "logical"), # whether page is final page or not
         contains = "test_element")

# test_page_info_text shows a page with some text and 
# a 'Next' button.
setClass("test_page_info_text",
         slots = list(text = "character", next_id = "character"),
         contains = "test_page")
setMethod(
  f = "initialize",
  signature = "test_page_info_text",
  definition = function(.Object, text, next_id) {
    if (length(text) != 1) {
      stop("Length of <text> slot must equal 1.")
    }
    .Object@next_id <- next_id
    .Object@text <- text
    .Object@ui <- fluidPage(tags$p(text),
                            actionButton(next_id, "Next"))
    .Object@result <- character()
    .Object@triggers <- next_id #"next"
    .Object@final <- FALSE
    return(.Object)
  }
)