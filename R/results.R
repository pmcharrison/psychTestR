# Classes ####

new_results <- function() {
  x <- list()
  class(x) <- "results"
  x <- register_next_results_section(x, "results")
  # attr(x, "metadata") <- list()
  x
}

#' @export
print.results <- function(x, ...) {
  num_sections <- length(x)
  num_results <- sum(vapply(x, length, integer(1)))
  cat(sprintf("psychTestR results list (%i result%s in %s section%s)\n",
              num_results, if (num_results != 1L) "s" else "",
              num_sections, if (num_sections != 1L) "s" else ""),
      "(visualise with as.list(), as_tibble(), or as.data.frame())\n")
}

#' @export
as.list.results <- function(x, ...) {
  class(x) <- "list"
  attr(x, "new_section") <- NULL
  x
}

#' @export
as_tibble.results <- function(x, ...) {
  y <- unlist(as.list(x), recursive = FALSE)
  y <- purrr::map(y,
                  ~ if (is.atomic(.) && length(.) > 1 && !is.null(names(.)))
                    as.list(.) else .)
  y <- purrr::map(y, ~ if (is.null(names(.)) && length(.) > 1) list(.) else .)
  df <- tibble::as_tibble(y, .name_repair = "minimal")
  stopifnot(nrow(df) == 1)
  session_info_cols <- grepl("^session\\.", names(df))
  df[, c(which(session_info_cols), which(!session_info_cols))]
}

#' @export
as.data.frame.results <- function(x, ...) {
  as.data.frame(tibble::as_tibble(x))
}

# sections <- as.list(x)
# section_labels <- names(x)
# if (is.null(section_labels)) section_labels <- rep(as.character(NA),
#                                                    times = length(sections))
# section_dfs <- mapply(convert_section_to_df, sections, section_labels,
#                       SIMPLIFY = FALSE)
# do.call(plyr::rbind.fill, section_dfs)

# convert_section_to_df <- function(section, section_label) {
#   stopifnot(is.list(section), is.scalar.character(section_label))
#   dfs <- lapply(section, convert_result_to_df)
#   res <- do.call(plyr::rbind.fill, dfs)
#   res <- res[, order(names(res))]
#   res <- cbind(section = section_label, res)
#   res
# }
#
# convert_result_to_df <- function(result) {
#   if (is.list(result)) {
#     elt_lengths <- vapply(result, length, integer(1))
#     for (i in seq_along(elt_lengths)) {
#       elt_length <- elt_lengths[[i]]
#       if (elt_length == 0L) result[[i]] <- NA
#       if (elt_length > 1L) result[[i]] <- list(result[[i]])
#     }
#   } else {
#     result <- list(result = result)
#   }
#   as.data.frame(lapply(result, I))
# }

# Accessing results ####

#' Access results
#'
#' Accesses the \code{results} slot in \code{state}.
#' @param state The participant's \code{state} object.
#' @export
results <- function(state) {
  state$passive$results
}

#' Set results
#'
#' Sets the \code{results} slot in \code{state}.
#' @param state The participant's \code{state} object.
#' @param value Value to set the \code{results} slot to.
#' @export
`results<-` <- function(state, value) {
  state$passive$results <- value
}

# Register next results section ####

#' Register next results section
#' @param x Where should the new section be registered?
#' Methods exist for \code{results} objects and for \code{state} objects.
#' @param label Label for the new section.
#' @export
register_next_results_section <- function(x, label) {
  stopifnot(is.null(label) || is.scalar.character(label))
  UseMethod("register_next_results_section")
}

#' @export
register_next_results_section.state <- function(x, label) {
  results(x) <- register_next_results_section(results(x), label)
  NULL
}

#' @export
register_next_results_section.results <- function(x, label) {
  attr(x, "new_section") <- label
  x
}

# Saving results ####

#' Save result
#'
#' Saves a new datum into the participant's results store.
#'
#' @param place Place to save the results.
#' Methods exist for \code{results} objects and for \code{state} objects.
#' @param label Label for the new datum.
#' @param value Value of the new datum.
#' @export
save_result <- function(place, label, value) UseMethod("save_result")

save_result.results <- function(place, label, value) {
  stopifnot(is.scalar.character(label))
  num_sections <- length(place)
  new_section <- num_sections == 0L || !is.null(attr(place, "new_section"))
  index_1 <- if (new_section) num_sections + 1L else num_sections
  index_2 <- if (new_section) 1L else length(place[[index_1]]) + 1L
  if (new_section) {
    new_section_label <- attr(place, "new_section")
    place[[index_1]] <- list()
    if (!is.null(new_section_label)) names(place)[index_1] <- new_section_label
    attr(place, "new_section") <- NULL
  }
  place[[index_1]][[index_2]] <- value
  names(place[[index_1]])[[index_2]] <- label
  place
}

#' @export
save_result.state <- function(place, label, value) {
  stopifnot(is.scalar.character(label))
  place$passive$results <- save_result.results(place$passive$results, label, value)
  place
}

Repository <- R6::R6Class("Repository", public = list(
  is_slow = NA,
  dirs = c("results", "deleted-results"),

  initialize = function(is_slow) {
    self$is_slow <- is_slow
  },

  deposit_results = function(results, key, opt, ...) {
    path <- tempfile()
    saveRDS(results, path)
    self$deposit_file(path, "results", key, opt, ...)
  },

  get_results = function(key, opt, ...) {
    path <- tempfile()
    self$get_file(key, "results", path, opt, ...)
    readRDS(path)
  },

  prepare = function(opt, ...) stop("not implemented"),
  deposit_file = function(local_file, dir, key, opt, ...) stop("not implemented"),
  get_file = function(dir, key, target_path, opt, ...) stop("not implemented"),
  file_exists = function(dir, key, opt, ...) stop("not implemented"),
  delete_file = function(dir, key, opt, ...) stop("not implemented"),

  check = function(opt) {
    self$prepare(opt)

    dir <- "results"
    tmp_file_in <- tempfile()
    tmp_file_out <- tempfile()
    key <- uuid::UUIDgenerate()
    file_content <- as.character(Sys.time())
    writeLines(file_content, tmp_file_in)

    testthat::expect(
      !self$file_exists(dir, key, opt),
      failure_message = "repository$file_exists should return FALSE for non-existent files"
    )
    self$deposit_file(tmp_file_in, dir, key, opt)
    testthat::expect(
      self$file_exists(dir, key, opt),
      failure_message = "repository$file_exists should return TRUE once a file has been deposited"
    )
    self$get_file(dir, key, tmp_file_out, opt)
    testthat::expect(
      testthat::compare(readLines(tmp_file_out), file_content)$equal,
      failure_message = "repository$get_file returned unexpected contents"
    )
    self$delete_file(dir, key, opt)
    testthat::expect(
      !self$file_exists(dir, key, opt),
      failure_message = "repository$file_exists should return FALSE once a file has been deleted"
    )
  }
))

LocalRespository <- R6::R6Class(
  "LocalRespository",
  inherit = Repository,

  public = list(
    root_dir = NA_character_,

    initialize = function(root_dir) {
      super$initialize(is_slow = FALSE)
      self$root_dir <- root_dir
    },

    prepare = function(opt, ...) {
      for (dir in self$dirs) {
        R.utils::mkdirs(file.path(self$root_dir, dir))
      }
    },

    path_in_repository = function(dir, key, opt) {
      file.path(self$root_dir, dir, key)
    },

    deposit_file = function(local_file, dir, key, opt, ...) {
      file.copy(local_file, self$path_in_repository(dir, key, opt))
    },

    get_file = function(dir, key, target_path, opt, ...) {
      file.copy(self$path_in_repository(dir, key, opt), target_path)
    },

    file_exists = function(dir, key, opt, ...) {
      file.exists(self$path_in_repository(dir, key, opt))
    },

    delete_file = function(dir, key, opt) {
      file.remove(self$path_in_repository(dir, key, opt))
    }
  )
)

DropboxRepository <- R6::R6Class(
  "DropboxRepository",
  inherit = Repository,

  public = list(
    root_dir = NA_character_,
    token_path = NA_character_,
    dropbox_key = NA_character_,
    dropbox_secret = NA_character_,

    initialize = function(root_dir,
                          token_path = "dropbox-token.rds",
                          dropbox_key = "mmhfsybffdom42w",
                          dropbox_secret = "l8zeqqqgm1ne5z0") {
      super$initialize(is_slow = TRUE)
      self$root_dir <- root_dir
      self$token_path <- token_path
      self$dropbox_key <- dropbox_key
      self$dropbox_secret <- dropbox_secret
      self$mute_dropbox_notifications <- mute_dropbox_notifications
    },

    check = function(opt, ...) {
      message("Checking that Dropbox repository is accessible...")
      super$check(opt, ...)
      message("Dropbox check complete.")
    },

    prepare = function(opt, ...) {
      self$authenticate()
      if (!self$dropbox_exists(self$root_dir)) {
        stop("directory '", self$root_dir, "' not found in Dropbox, ",
             "you must create this manually")
      }
      for (dir in self$dirs) {
        full_path <- file.path(self$root_dir, dir)
        if (!self$dropbox_exists(full_path)) {
          rdrop2::drop_create(full_path)
        }
      }
    },

    authenticate = function() {
      rdrop2::drop_auth(rdstoken = self$token_path,
                        cache = FALSE,
                        key = self$dropbox_key,
                        secret = self$dropbox_secret)
    },

    dropbox_exists = function(path) {
      self$authenticate()
      tryCatch(
        rdrop2::drop_exists(path),
        error = function(e) {
          if (e$message == "Conflict (HTTP 409).") FALSE else stop(e)
        }
      )
    },

    path_in_repository = function(dir, key) {
      file.path(self$root_dir, dir, key)
    },

    get_dropbox_token = function() {
      if (!file.exists(self$token_path))
        stop("Couldn't find ", self$token_path, ". Do you need to call repo$new_dropbox_token()?")
      readRDS(self$token_path)
    },

    # @inheritParams rdrop2::drop_auth
    new_dropbox_token = function() {
      token <- rdrop2::drop_auth(
        new_user = TRUE,
        key = self$dropbox_key,
        secret = self$dropbox_secret
      )
      saveRDS(token, self$token_path)
    },

    deposit_file = function(local_file, dir, key, opt, ...) {
      self$authenticate()
      tmp_dir <- tempfile("dir")
      R.utils::mkdirs(tmp_dir)
      new_local_path <- file.path(tmp_dir, key)
      file.copy(local_file, new_local_path)
      rdrop2::drop_upload(new_local_path,
                          file.path(self$root_dir, dir),
                          autorename = FALSE)
    },

    get_file = function(dir, key, target_path, opt, ...) {
      self$authenticate()
      rdrop2::drop_download(
        self$path_in_repository(dir, key),
        target_path,
        overwrite = TRUE
      )
    },

    file_exists = function(dir, key, opt, ...) {
      self$dropbox_exists(self$path_in_repository(dir, key))
    },

    delete_file = function(dir, key, opt) {
      self$authenticate()
      rdrop2::drop_delete(self$path_in_repository(dir, key))
    }
  )
)
