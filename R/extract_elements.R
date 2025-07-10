extract_post_author_infos <- function(post) {
    list(
        author_handle = post$author$handle,
        author_did = post$author$did,
        created_at = post$author$createdAt
    )
}

extract_post_text <- function(post) {
    post$record$text
}


extract_post_created_at <- function(post) {
    post$record$createdAt
}

extract_post_langs <- function(post) {
    post$record$langs %>%
        unlist() %>%
        paste(collapse = "|")
}

extract_post_hashtags <- function(post) {
    post$record$facets %>%
        map("features") %>%
        map(function(.x) map(.x, "tag")) %>%
        unlist()
}

extract_post_hashtags_from_text <- function(text) {
    hashtags <- regmatches(text, gregexpr("#\\w+", text))[[1]]
    gsub("#", "", hashtags)
}

create_post_url <- function(post) {
    # Check if post has uri
    if (is.null(post$uri)) {
        return(NULL)
    }

    # Extract post rkey
    post_rkey = sub(".*/", "", post$uri)

    # Construct web URL
    web_url = paste0(
        "https://bsky.app/profile/",
        post$author$did,
        "/post/",
        sub(".*/", "", post$uri)
    )

    return(web_url)
}

create_post_id <- function(post) {
    # Check if post has uri
    if (is.null(post$uri)) {
        return(NULL)
    }

    # Create post URL
    post_url <- create_post_url(post)
    digest::digest(post_url, algo = "md5")
}
