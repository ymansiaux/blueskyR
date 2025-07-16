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
  sort = "latest",
  since = NULL,
  until = NULL,
  number_of_posts_per_request = 100,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts",
  max_retries = 20,
  delay_between_retries = 5,
  errors_for_retries = c(420, 500, 503),
  verbose = TRUE
) {
  if (!is_online()) {
    stop("No internet connection")
  }
  # Make a single request and return results
  req <- request(search_url) |>
    req_url_query(q = keyword, limit = number_of_posts_per_request, sort = sort)

  if (!is.null(cursor)) {
    req <- req |> req_url_query(cursor = cursor)
  }
  if (!is.null(since)) {
    req <- req |> req_url_query(since = format_date_for_bluesky(since))
  }
  if (!is.null(until)) {
    req <- req |> req_url_query(until = format_date_for_bluesky(until))
    message("Will retrieve posts until ", until)
  }

  # Must find a way to check for invalid token
  resp <- req |>
    req_headers(Authorization = paste("Bearer", access_jwt)) |>
    req_retry(
      max_tries = max_retries,
      retry_on_failure = TRUE,
      # backoff = \(resp) delay_between_retries,
      is_transient = rate_limited_check,
      after = rerun_after_rate_limit
    ) %>%
    req_perform()

  # Get results
  results <- resp_body_json(resp)
  posts <- results$posts
  next_cursor <- results$cursor
  created_at <- extract_many_posts_created_at(posts)
  if (length(created_at) == 0) {
    min_created_at <- NULL
    max_created_at <- NULL
  } else {
    min_created_at <- min(unlist(created_at))
    max_created_at <- max(unlist(created_at))
  }

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
    results = results,
    next_cursor = next_cursor,
    oldest_message_in_a_query = min_created_at,
    newest_message_in_a_query = max_created_at,
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
  max_posts = NULL,
  get_posts_until = NULL,
  sort = "latest",
  number_of_posts_per_request = 100,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts",
  max_retries = 20,
  delay_between_retries = 5,
  errors_for_retries = c(420, 500, 503),
  verbose = TRUE,
  delay_between_requests = 0.5,
  max_consecutive_failures = 5,
  otherwise = NULL
) {
  if (!is_online()) {
    stop("No internet connection")
  }

  if (!file.exists("session.rds")) {
    stop("Session file not found. Creating a session first.")
    session <- create_session()
    saveRDS(session, "session.rds")
  }
  session <- readRDS("session.rds")
  if (session$created < Sys.time() - 3600) {
    stop("Session expired. Creating a new session.")
    session <- create_session()
    saveRDS(session, "session.rds")
  }
  access_jwt <- session$access_jwt

  # Very simple request to check if the token is valid and the rate limit is not exceeded
  # We don't want a R error to be thrown if the token is invalid or the rate limit is exceeded
  # We just want to know that the token is invalid or the rate limit is exceeded

  simple_request <- request(search_url) |>
    req_url_query(q = keyword, limit = 1, sort = sort) |>
    req_headers(Authorization = paste("Bearer", access_jwt)) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()

  if (resp_status(simple_request) == 401) {
    message("Invalid token. Creating a new session.")
    session <- create_session()
    saveRDS(session, "session.rds")
    access_jwt <- session$access_jwt
  }

  # Create a robust version of search_posts using purrr::possibly
  robust_search_posts <- possibly(
    search_posts,
    otherwise = otherwise
  )

  all_posts <- list()
  current_date_max <- get_posts_until
  # current_cursor <- cursor
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
      cursor = NULL,
      sort = sort,
      until = current_date_max,
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
    current_date_max <- result$min_created_at

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
  created_at <- extract_many_posts_created_at(all_posts)

  return(list(
    posts = all_posts,
    final_date_min = min(unlist(created_at)),
    final_date_max = max(unlist(created_at)),
    total_requests = request_count,
    failed_requests = failed_requests
  ))
}

#' @noRd
rate_limited_check <- function(resp) {
  if (resp_status(resp) == 429) {
    identical(resp_header(resp, "RateLimit-Remaining"), "0")
  } else if (resp_status(resp) == 503) {
    TRUE
  } else {
    FALSE
  }
}

#' @noRd
rerun_after_rate_limit <- function(resp) {
  if (resp_status(resp) == 429) {
    time <- as.numeric(resp_header(resp, "RateLimit-Reset"))
    time - unclass(Sys.time())
  } else {
    return(NA)
  }
}
