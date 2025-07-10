# Helper functions and mock data for testing

# Network status helpers
online <- function() {
  TRUE
}
offline <- function() {
  FALSE
}

# Mock post data
mock_post <- function(
  uri = "at://did:plc:test123/app.bsky.feed.post/abc123",
  cid = "cid123",
  handle = "test.bsky.app",
  did = "did:plc:test123",
  text = "Test post",
  created_at = "2023-01-01T00:00:00.000Z"
) {
  list(
    uri = uri,
    cid = cid,
    author = list(
      handle = handle,
      did = did,
      createdAt = created_at
    ),
    record = list(
      text = text,
      createdAt = created_at
    ),
    indexedAt = created_at
  )
}

# Mock search response
mock_search_response <- function(posts = list(mock_post()), cursor = NULL) {
  list(
    posts = posts,
    cursor = cursor
  )
}


# Helper to create multiple mock posts
create_mock_posts <- function(n = 5, base_text = "Test post") {
  lapply(1:n, function(i) {
    mock_post(
      uri = paste0("at://did:plc:test", i, "/app.bsky.feed.post/", i),
      cid = paste0("cid", i),
      handle = paste0("test", i, ".bsky.app"),
      did = paste0("did:plc:test", i),
      text = paste0(base_text, " ", i)
    )
  })
}

# Mock httr2 functions
mock_request <- function(url) {
  structure(list(url = url), class = "httr2_request")
}

mock_req_url_query <- function(req, ...) req

mock_req_headers <- function(req, ...) req

mock_req_body_json <- function(req, data) req

mock_req_perform <- function(req, mock_response = NULL) {
  if (is.null(mock_response)) {
    mock_response <- mock_search_response()
  }
  structure(
    list(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(mock_response))
    ),
    class = "httr2_response"
  )
}

mock_resp_check_status <- function(resp) resp

mock_resp_body_json <- function(resp, mock_data = NULL) {
  if (is.null(mock_data)) {
    mock_data <- mock_search_response()
  }
  mock_data
}

mock_last_response <- function(resp) resp
mock_resp_status <- function(resp) "200"

# Mock error response
mock_error_response <- function(
  status_code = 401,
  error_message = "HTTP 401 Unauthorized"
) {
  structure(
    list(
      status_code = status_code,
      body = charToRaw('{"error":"Unauthorized"}')
    ),
    class = "httr2_response"
  )
}

mock_error_resp_body_json <- function(resp) {
  stop("HTTP 401 Unauthorized")
}

# Mock error req_perform
mock_error_req_perform <- function(req) {
  mock_error_response()
}
