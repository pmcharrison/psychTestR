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

# one_btn_page shows a page with some text and 
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