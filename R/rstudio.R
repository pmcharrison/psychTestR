insert_fun <- function(name) {
  rstudioapi::insertText(paste0(name, "()"))
  pos <- as.integer(rstudioapi::primary_selection(
    x = rstudioapi::getSourceEditorContext()
  )$range$start)
  rstudioapi::setCursorPosition(
    rstudioapi::document_position(pos[1], pos[2] - 1)
  )
}

insert_i18n <- function() {
  insert_fun("i18n")
}

insert_pt_i18n <- function() {
  insert_fun("psychTestR::i18n")
}

insert_psychTestR <- function() {
  rstudioapi::insertText("psychTestR::")
}
