# Modelled on shiny:::createUniqueId
generate_id <- function(bytes, prefix = "", suffix = "") {
  paste(prefix,
        paste(
          format(as.hexmode(
            sample(256, bytes, replace = TRUE) - 1), width = 2),
          collapse = ""),
        suffix, sep = "")
}
