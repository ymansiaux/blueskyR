#' Search posts for a given keyword
#'
#' @param keyword Keyword to search for
#' @param access_jwt Access token
#' @param max_posts Maximum number of posts to retrieve
#' @param search_url Search URL
#' @param number_of_posts_per_request Number of posts to retrieve per request
#' @param max_retries Maximum number of retries
#' @param delay_between_retries Delay between retries
#' @param errors_for_retries Errors for retries
#' @details You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/app-bsky-feed-search-posts}
#' Additionnaly more information about the "post" object is available here: \url{https://atproto.blue/en/latest/atproto/atproto_client.models.app.bsky.feed.defs.html#atproto_client.models.app.bsky.feed.defs.PostView}
#' @return List of posts
#' @export
#' @importFrom httr2 request req_url_query req_headers req_perform resp_body_json is_online resp_status last_response req_retry
search_posts <- function(
  keyword,
  access_jwt,
  max_posts = NULL,
  number_of_posts_per_request = 100,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts",
  max_retries = 20,
  delay_between_retries = 5,
  errors_for_retries = c(420, 429, 500, 503)
) {
  if (!is_online()) {
    stop("No internet connection")
  }

  all_posts <- list()
  cursor <- NULL
  post_count <- 0

  repeat {
    req <- request(search_url) |>
      req_url_query(q = keyword, limit = limit)

    if (!is.null(cursor)) {
      req <- req |> req_url_query(cursor = cursor)
    }

    resp <- req |>
      req_headers(Authorization = paste("Bearer", access_jwt)) |>
      req_retry(
        max_tries = max_retries,
        retry_on_failure = TRUE,
        backoff = \(resp) delay_between_retries,
        is_transient = \(resp) resp_status(resp) %in% errors_for_retries
      ) %>%
      req_perform()

    status_code <- resp_status(last_response())

    # Get results
    results <- resp_body_json(resp)
    posts <- results$posts

    # Add posts to our collection
    all_posts <- c(all_posts, posts)
    post_count <- post_count + length(posts)

    # Check if we have more posts
    cursor <- results$cursor

    # Print progress
    cat("Retrieved", post_count, "posts so far...\n")

    # Break conditions
    if (is.null(cursor) || length(posts) == 0) {
      cat("No more posts available.\n")
      break
    }

    if (!is.null(max_posts) && post_count >= max_posts) {
      cat("Reached maximum number of posts.\n")
      break
    }

    Sys.sleep(0.5)
  }

  cat("Total posts retrieved:", length(all_posts), "\n")
  return(all_posts)
}
