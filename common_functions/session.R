loadNamespace("assertthat")
loadNamespace("R.utils")

library(shiny)

saveSession <- function(ssuid, session_data, session_dir) {
  assertthat::assert_that(
    is.character(ssuid), assertthat::is.scalar(ssuid),
    is.character(session_dir), assertthat::is.scalar(session_dir)
  )
  ssuid_dir <- file.path(session_dir, ssuid)
  assertthat::assert_that(dir.exists(ssuid_dir))
  data_path <- file.path(ssuid_dir, "data.RDS")
  message("Saving session to path ", data_path)
  message("Current page:")
  print(session_data$current_page)
  saveRDS(session_data, data_path)
  saveTimestamp(session_dir, ssuid)
}

#' Returns NULL if no session was found with that SSUID
loadSession <- function(ssuid, session_dir) {
  assertthat::assert_that(
    is.character(ssuid), assertthat::is.scalar(ssuid),
    is.character(session_dir), assertthat::is.scalar(session_dir)
  )
  path <- file.path(session_dir, ssuid, "data.RDS")
  if (file.exists(path)) {
    message("Loading session from path ", path)
    session_data <- readRDS(path)
    message("Current page:")
    print(session_data$current_page)
    session_data
  } else NULL
}

getNewSSUID <- function(session_dir) {
  ssuid <- NA
  R.utils::mkdirs(session_dir)
  while (is.na(ssuid)) {
    candidate <- shiny:::createUniqueId(16)
    is_unique <- dir.create(file.path(session_dir, candidate), showWarnings = FALSE)
    if (is_unique) {
      ssuid <- candidate
      saveTimestamp(session_dir, ssuid)
    }
  }
  ssuid
}

saveTimestamp <- function(session_dir, ssuid) {
  assertthat::assert_that(
    is.character(ssuid), assertthat::is.scalar(ssuid),
    is.character(session_dir), assertthat::is.scalar(session_dir)
  )
  saveRDS(Sys.time(), file.path(session_dir, ssuid, "timestamp.RDS"))
}

#' Returns NA if no timestamp found
readTimestamp <- function(session_dir, ssuid) {
  assertthat::assert_that(
    is.character(ssuid), assertthat::is.scalar(ssuid),
    is.character(session_dir), assertthat::is.scalar(session_dir)
  )
  path <- file.path(session_dir, ssuid, "timestamp.RDS")
  if (file.exists(path)) {
    readRDS(path)
  } else NA
}

#' Deletes all session caches that are older than a certain amount.
cleanSessionDir <- function(session_dir, timeout_min = 60) {
  assertthat::assert_that(
    is.numeric(timeout_min), assertthat::is.scalar(timeout_min),
    is.character(session_dir), assertthat::is.scalar(session_dir)
  )
  dirs <- list.dirs(session_dir, recursive = FALSE)
  for (dir in dirs) {
    ssuid <- basename(dir)
    time <- readTimestamp(session_dir, ssuid)
    if (!is.na(time) && time < Sys.time() - timeout_min * 60) {
      unlink(dir, recursive = TRUE)
    }
  }
}

manageSession <- function(save, restore, session = getDefaultReactiveDomain(),
                          files) {
  message("Managing the session")
  isolate({
    params <- parseQueryString(session$clientData$url_search)
    ssuid = params[["SSUID"]]
    if (is.null(ssuid)) {
      message("Getting new SSUID")
      ssuid <- getNewSSUID(files$session_dir)
      message("ssuid = ", format(ssuid))
    } else {
      restore(loadSession(ssuid, files$session_dir))
    }
  })
  message("Starting the session")
  session$sendCustomMessage("session_start", ssuid)
  
  observe({
    tryCatch(
      saveSession(ssuid = ssuid, session_data = save(), files$session_dir),
      error = function(e) {
        warning("Failed to save session: ", e)
      }
    )
  })
}
