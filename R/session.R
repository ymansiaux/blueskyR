#' Get bearer token
#'
#' @param handle Bluesky handle
#' @param password Bluesky password
#' @param login_url Bluesky login URL
#'
#' @return List with access_jwt and did
#' @export
#' @importFrom httr2 request req_body_json req_perform resp_check_status resp_body_json is_online req_headers
#' @details You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/com-atproto-server-create-session}
#' @rdname session
#' @examples
#' \dontrun{
#' create_session()
#' }
#'
create_session <- function(
  handle = Sys.getenv("bluesky_id"),
  password = Sys.getenv("bluesky_pwd"),
  login_url = "https://bsky.social/xrpc/com.atproto.server.createSession"
) {
  if (!is_online()) {
    stop("No internet connection")
  }

  if (is.null(handle) || is.null(password)) {
    stop("Handle and password are required")
  }

  if (handle == "") {
    stop("Handle is required")
  }

  if (password == "") {
    stop("Password is required")
  }

  if (login_url == "") {
    stop("Login URL is required")
  }
  resp <- request(login_url) |>
    req_body_json(list(identifier = handle, password = password)) |>
    req_perform()

  resp_check_status(resp)

  session <- resp_body_json(resp)
  session <- list(
    handle = session$handle,
    access_jwt = session$accessJwt,
    did = session$did,
    refreshJwt = session$refreshJwt,
    created = Sys.time()
  )
  saveRDS(session, "session.rds")
  return(session)
}

#' Refresh session
#'
#' @param refresh_token Refresh token
#' @param refresh_url Bluesky refresh URL
#'
#' @return List with access_jwt and did
#' @export
#' @rdname session
refresh_session <- function(
  refresh_token,
  refresh_url = "https://bsky.social/xrpc/com.atproto.server.refreshSession"
) {
  if (!is_online()) {
    stop("No internet connection")
  }

  if (is.null(refresh_token) || refresh_token == "") {
    stop("Refresh token is required")
  }

  resp <- request(refresh_url) |>
    req_headers(Authorization = paste("Bearer", refresh_token)) |>
    req_body_json(list()) |>
    req_perform()

  resp_check_status(resp)

  session <- resp_body_json(resp)
  return(list(
    access_jwt = session$accessJwt,
    refresh_jwt = session$refreshJwt,
    did = session$did,
    handle = session$handle,
    refreshed = Sys.time()
  ))
}

#' Get token
#'
#' @return Token
#' @export
#' @rdname session
get_token <- function() {
  if (file.exists("session.rds")) {
    session <- readRDS("session.rds")
    if (session$created < Sys.time() - 3600) {
      session <- create_session()
      saveRDS(session, "session.rds")
    }
    return(session$access_jwt)
  }
  return(create_session()$access_jwt)
}
