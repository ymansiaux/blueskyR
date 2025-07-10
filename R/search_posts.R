#' Search posts for a given keyword
#'
#' @param keyword Keyword to search for
#' @param access_jwt Access token
#' @param cursor Optional cursor to resume from a specific point
#' @param search_url Search URL
#' @param number_of_posts_per_request Number of posts to retrieve per request
#' @param max_retries Maximum number of retries
#' @param delay_between_retries Delay between retries
#' @param errors_for_retries Errors for retries
#' @param verbose Whether to print progress messages
#' @details You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/app-bsky-feed-search-posts}
#' Additionnaly more information about the "post" object is available here: \url{https://atproto.blue/en/latest/atproto/atproto_client.models.app.bsky.feed.defs.html#atproto_client.models.app.bsky.feed.defs.PostView}
#' @return List with posts and next cursor for resumption
#' @export
#' @importFrom httr2 request req_url_query req_headers req_perform resp_body_json is_online resp_status last_response req_retry
search_posts <- function(
  keyword,
  access_jwt,
  cursor = NULL,
  number_of_posts_per_request = 100,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts",
  max_retries = 20,
  delay_between_retries = 5,
  errors_for_retries = c(420, 429, 500, 503),
  verbose = TRUE
) {
  if (!is_online()) {
    stop("No internet connection")
  }

  # Make a single request and return results
  req <- request(search_url) |>
    req_url_query(q = keyword, limit = number_of_posts_per_request)

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

  # Get results
  results <- resp_body_json(resp)
  posts <- results$posts
  next_cursor <- results$cursor

  if (verbose) {
    cat("Retrieved", length(posts), "posts.\n")
    if (!is.null(next_cursor)) {
      cat("Next cursor available for continuation.\n")
    } else {
      cat("No more posts available.\n")
    }
  }

  # Return both posts and next cursor for potential resumption
  return(list(
    posts = posts,
    next_cursor = next_cursor,
    has_more = !is.null(next_cursor) && length(posts) > 0
  ))
}

#' Search posts with automatic pagination using purrr::possibly for robustness
#'
#' @param keyword Keyword to search for
#' @param access_jwt Access token
#' @param cursor Optional cursor to resume from a specific point
#' @param max_posts Maximum number of posts to retrieve
#' @param search_url Search URL
#' @param number_of_posts_per_request Number of posts to retrieve per request
#' @param max_retries Maximum number of retries
#' @param delay_between_retries Delay between retries
#' @param errors_for_retries Errors for retries
#' @param verbose Whether to print progress messages
#' @param delay_between_requests Delay between requests to avoid rate limiting
#' @param max_consecutive_failures Maximum number of consecutive failures before stopping
#' @param otherwise Value to return if a request fails (default: NULL)
#' @details This function automatically handles pagination to retrieve multiple pages of results.
#' It uses purrr::possibly to handle request failures gracefully, allowing the process to continue
#' even if individual requests fail. You can find more information about the Bluesky API here:
#' \url{https://docs.bsky.app/docs/api/app-bsky-feed-search-posts}
#' @return List with posts and final cursor
#' @export
#' @importFrom purrr possibly
search_posts_paginated <- function(
  keyword,
  access_jwt,
  cursor = NULL,
  max_posts = NULL,
  number_of_posts_per_request = 100,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts",
  max_retries = 20,
  delay_between_retries = 5,
  errors_for_retries = c(420, 429, 500, 503),
  verbose = TRUE,
  delay_between_requests = 0.5,
  max_consecutive_failures = 5,
  otherwise = NULL
) {
  if (!is_online()) {
    stop("No internet connection")
  }

  # Create a robust version of search_posts using purrr::possibly
  robust_search_posts <- possibly(
    search_posts,
    otherwise = otherwise
  )

  all_posts <- list()
  current_cursor <- cursor
  post_count <- 0
  request_count <- 0
  failed_requests <- 0

  while (TRUE) {
    request_count <- request_count + 1

    if (verbose) {
      cat("Making request", request_count, "for keyword", keyword, "...\n")
    }
    # Make a single request with robust error handling
    result <- robust_search_posts(
      keyword = keyword,
      access_jwt = access_jwt,
      cursor = current_cursor,
      number_of_posts_per_request = number_of_posts_per_request,
      search_url = search_url,
      max_retries = max_retries,
      delay_between_retries = delay_between_retries,
      errors_for_retries = errors_for_retries,
      verbose = FALSE
    )

    # Check if the request failed
    if (is.null(result)) {
      failed_requests <- failed_requests + 1
      if (verbose) {
        cat(
          "Request",
          request_count,
          "failed for keyword",
          keyword,
          ". Continuing with next request...\n"
        )
      }

      # If we've had too many consecutive failures, we might want to stop
      if (failed_requests >= max_consecutive_failures) {
        if (verbose) {
          cat(
            "Too many consecutive failures for keyword",
            keyword,
            ". Stopping.\n"
          )
        }
        break
      }

      # Wait a bit longer before retrying
      Sys.sleep(delay_between_requests * 2)
      next
    }

    # Reset failed requests counter on success
    failed_requests <- 0

    # Add posts to our collection
    all_posts <- c(all_posts, result$posts)
    post_count <- post_count + length(result$posts)

    if (verbose) {
      cat(
        "Total posts retrieved so far for keyword",
        keyword,
        ":",
        post_count,
        "\n"
      )
    }

    # Check if we have more posts and should continue
    if (!result$has_more) {
      if (verbose) {
        cat("No more posts available for keyword", keyword, ".\n")
      }
      break
    }

    if (!is.null(max_posts) && post_count >= max_posts) {
      if (verbose) {
        cat("Reached maximum number of posts for keyword", keyword, ".\n")
      }
      break
    }

    # Update cursor for next iteration
    current_cursor <- result$next_cursor

    # Add delay between requests
    if (delay_between_requests > 0) {
      Sys.sleep(delay_between_requests)
    }
  }

  if (verbose) {
    cat(
      "Total posts retrieved for keyword",
      keyword,
      ":",
      length(all_posts),
      "\n"
    )
    cat("Total requests made for keyword", keyword, ":", request_count, "\n")
    if (failed_requests > 0) {
      cat("Failed requests for keyword", keyword, ":", failed_requests, "\n")
    }
  }

  return(list(
    posts = all_posts,
    final_cursor = current_cursor,
    total_requests = request_count,
    failed_requests = failed_requests
  ))
}
