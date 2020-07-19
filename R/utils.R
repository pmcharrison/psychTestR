is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

#' Is NULL or...?
#'
#' Returns \code{TRUE} if \code{x} is either \code{NULL} or \code{f(x)}
#' is \code{TRUE}.
#'
#' @param x Object to check.
#'
#' @param f Function to apply.
#'
#' @keywords internal
#' @export
is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

#' Display a popup error message
#'
#' Displays a popup error message and terminates the testing session.
#' @param ... Arguments to be pasted together (without separator) to form
#' the error message.
#' @export
display_error <- function(...) {
  msg <- paste(..., collapse = "")
  shinyjs::alert(msg)
  stop(msg)
}

#' @rdname global_local
#' @export
assert_global_is_null <- function(key, state) {
  stopifnot(is.scalar.character(key), is(state, "state"))
  if (is.null(get_global(key, state))) TRUE else {
    stop("global variable <", key, "> in <state> was not NULL")
  }
}

remove_trailing_slashes <- function(x) gsub("/*$", "", x)

dir_copy <- function(dir, target_path) {
  dir <- remove_trailing_slashes(dir)
  target_path <- remove_trailing_slashes(target_path)
  target_parent <- dirname(target_path)

  tmp_dir <- tempfile("dir")
  dir.create(tmp_dir)

  old_dir_name <- basename(dir)
  new_dir_name <- basename(target_path)

  file.copy(
    from = dir,
    to = tmp_dir,
    recursive = TRUE
  )

  file.rename(file.path(tmp_dir, old_dir_name),
              file.path(tmp_dir, new_dir_name))

  file.copy(
    from = file.path(tmp_dir, new_dir_name),
    to = target_parent,
    recursive = TRUE
  )
}

dropbox_download_folder <- function(
  path,
  local_path,
  unzip = TRUE,
  overwrite = FALSE,
  dtoken = rdrop2::drop_auth(),
  progress = interactive(),
  verbose = interactive()) {

  if (unzip && dir.exists(local_path))
    stop("a directory already exists at ", local_path)
  if (!unzip && file.exists(local_path))
    stop("a file already exists at ", local_path)

  path <- remove_trailing_slashes(path)
  local_path <- remove_trailing_slashes(local_path)
  local_parent <- dirname(local_path)
  original_dir_name <- basename(path)
  download_path <- if (unzip) tempfile("dir") else local_path

  if (!dir.exists(local_parent)) stop("target parent directory ", local_parent, " not found")

  url <- "https://content.dropboxapi.com/2/files/download_zip"
  req <- httr::POST(
    url = url,
    httr::config(token = dtoken),
    httr::add_headers(
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = paste0("/", path)),
                                           auto_unbox = TRUE)),
    if (progress) httr::progress(),
    httr::write_disk(download_path, overwrite)
  )
  httr::stop_for_status(req)
  if (verbose) {
    size <- file.size(download_path)
    class(size) <- "object_size"
    message(sprintf("Downloaded %s to %s: %s on disk", path,
                    download_path, format(size, units = "auto")))
  }
  if (unzip) {
    if (verbose) message("Unzipping file...")
    new_dir_name <- basename(local_path)
    unzip_path <- tempfile("dir")
    unzip(download_path, exdir = unzip_path)
    dir_copy(file.path(unzip_path, original_dir_name),
             local_path)
  }

  TRUE
}
