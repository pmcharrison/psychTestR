# Version of actionButton that also triggers the next page
#' @export
trigger_button <- function(inputId, label, icon = NULL, width = NULL, ...) {
  shiny::actionButton(
    inputId = inputId, label = label,
    icon = icon, width = width,
    onclick = paste(
      sprintf('Shiny.onInputChange("lastBtnPressed", "%s");',
              inputId),
      "document.getElementById('current_page.ui').style.visibility = 'hidden';",
      'setTimeout(function() {Shiny.onInputChange("nextPage", performance.now());}, 500);'),
    ...)
}
