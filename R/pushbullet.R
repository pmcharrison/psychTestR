async_pushbullet <- function(title, body, opt) {
  email <- opt$pushbullet$email
  apikey <- opt$pushbullet$apikey
  stopifnot(
    is.scalar.character(title),
    is.scalar.character(body),
    is.scalar.character(email),
    is.scalar.character(apikey)
  )
  parallel::mcparallel(RPushbullet::pbPost(
    type = "note",
    title = title,
    body = body,
    email = email,
    apikey = apikey
  ))
}
