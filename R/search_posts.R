#' Search posts for a given keyword
#'
#' @param keyword Keyword to search for
#' @param access_jwt Access token
#' @param max_posts Maximum number of posts to retrieve
#' @param search_url Search URL
#' @details You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/app-bsky-feed-search-posts}
#' Additionnaly more information about the "post" object is available here: \url{https://atproto.blue/en/latest/atproto/atproto_client.models.app.bsky.feed.defs.html#atproto_client.models.app.bsky.feed.defs.PostView}
#' @return List of posts
#' @export
#' @importFrom httr2 request req_url_query req_headers req_perform resp_body_json is_online
search_posts <- function(
    keyword,
    access_jwt,
    max_posts = NULL,
    search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts"
) {
    if (!is_online()) {
        stop("No internet connection")
    }

    all_posts <- list()
    cursor <- NULL
    post_count <- 0

    repeat {
        # Build request with cursor if available
        req <- request(search_url) |>
            req_url_query(q = keyword, limit = 100)

        if (!is.null(cursor)) {
            req <- req |> req_url_query(cursor = cursor)
        }

        req <- req |>
            req_headers(Authorization = paste("Bearer", access_jwt)) |>
            req_perform()

        # Get results
        results <- resp_body_json(req)
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
