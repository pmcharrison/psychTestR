# From http://adv-r.had.co.nz/beyond-exception-handling.html
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}
