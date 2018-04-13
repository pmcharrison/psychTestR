context("style")

# After https://stackoverflow.com/questions/6216968/r-force-local-scope
# (answer from Tommy)

check_globals <- function(f, label, silent = FALSE) {
  vars <- codetools::findGlobals(f)
  found <- !vapply(vars, exists, logical(1), envir=parent.frame())
  if (!silent && any(found)) {
    warning(label,": Global variables used: ", paste(names(found)[found], collapse=', '), "\n")
    return(invisible(FALSE))
  }
  !any(found)
}

check_fun <- function(label) {
  f <- get(label, mode = "function", envir = parent.frame())
  codetools::checkUsage(f, report = stop)
  res <- check_globals(f, label)
  res
}


test_that("style", {
  functions <- lsf.str("package:psychTest") %>% as.character
  sapply(functions, function(x) expect_true(check_fun(x)))
})
