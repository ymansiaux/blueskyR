#' Get bearer token
#'
#' @param handle Bluesky handle
#' @param password Bluesky password
#' @param login_url Bluesky login URL
#'
#' @return List with access_jwt and did
#' @export
#' @importFrom httr2 request req_body_json req_perform resp_check_status resp_body_json is_online
#' @details You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/com-atproto-server-create-session}
#' @examples
#' \dontrun{
#' get_bearer_token()
#' }
#'
get_bearer_token <- function(
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
    access_jwt <- session$accessJwt
    did <- session$did

    return(list(access_jwt = access_jwt, did = did))
}
