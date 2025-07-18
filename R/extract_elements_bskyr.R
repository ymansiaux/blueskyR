#' Extract elements from many posts
#'
#' @param posts A list of post objects
#' @return A list of lists of elements
#' @export
#' @importFrom stats setNames
#' @importFrom purrr map pluck pmap
#' @rdname extract_elements_bskyr
extract_many_posts_elements_bskyr <- function(posts) {
  author_infos = extract_many_posts_author_infos_bskyr(posts)
  text = extract_many_posts_text_bskyr(posts)
  created_at = extract_many_posts_created_at_bskyr(posts)
  langs = extract_many_posts_langs_bskyr(posts)
  hashtags = extract_many_posts_hashtags_bskyr(posts)
  url = extract_many_posts_url_bskyr(posts)
  id = extract_many_posts_id_bskyr(posts)
  pmap(
    list(author_infos, text, created_at, langs, hashtags, url, id),
    ~ list(
      author_infos = ..1,
      text = ..2,
      created_at = ..3,
      langs = ..4,
      hashtags = ..5,
      url = ..6,
      id = ..7
    )
  ) %>%
    setNames(id)
}

#' Extract author information from many posts
#'
#' @param posts A list of post objects
#' @return A list of lists with author handle, did, and created at
#' @importFrom purrr map pluck
#' @export
#' @rdname extract_elements_bskyr
extract_many_posts_author_infos_bskyr <- function(posts) {
  posts %>%
    pluck("author") %>%
    map(
      ~ list(
        author_handle = pluck(.x, "handle"),
        author_did = pluck(.x, "did"),
        created_at = pluck(.x, "createdAt")
      )
    )
}

#' Extract text from many posts
#'
#' @param posts A list of post objects
#' @return A list of texts
#' @importFrom purrr map pluck
#' @export
#' @rdname extract_elements_bskyr
extract_many_posts_text_bskyr <- function(posts) {
  posts %>%
    pluck("record") %>%
    map("text")
}

#' Extract created at from many posts
#'
#' @param posts A list of post objects
#' @return A list of created ats
#' @importFrom purrr map pluck
#' @export
#' @rdname extract_elements_bskyr
extract_many_posts_created_at_bskyr <- function(posts) {
  posts %>%
    pluck("record") %>%
    map("createdAt")
}


#' Extract langs from many posts
#'
#' @param posts A list of post objects
#' @return A list of langs
#' @importFrom purrr map pluck
#' @export
#' @rdname extract_elements_bskyr
extract_many_posts_langs_bskyr <- function(posts) {
  posts %>%
    pluck("record") %>%
    map("langs") %>%
    map(unlist) %>%
    map(paste, collapse = "|")
}


#' Extract hashtags from many posts
#'
#' @param posts A list of post objects
#' @return A list of lists of hashtags
#' @importFrom purrr map pluck list_flatten
#' @export
#' @rdname extract_elements_bskyr
extract_many_posts_hashtags_bskyr <- function(posts) {
  posts %>%
    pluck("record") %>%
    map("facets") %>%
    map(function(.x) map(.x, "features")) %>%
    map(function(.x) list_flatten(.x) %>% map("tag") %>% unlist())
}

#' Create post URL
#'
#' @param post_uri The URI of the post
#' @param author_did The DID of the author
#' @return The URL of the post
#' @export
#' @rdname extract_elements_bskyr
create_post_url_bskyr <- function(post_uri, author_did) {
  # Check if post has uri
  if (is.null(post_uri)) {
    return(NULL)
  }

  # Extract post rkey
  post_rkey = sub(".*/", "", post_uri)

  # Construct web URL
  web_url = paste0(
    "https://bsky.app/profile/",
    author_did,
    "/post/",
    post_rkey
  )

  return(web_url)
}

#' Create post URL from many posts
#'
#' @param posts A list of post objects
#' @return A list of URLs
#' @importFrom purrr map pluck map2
#' @export
#' @rdname extract_elements_bskyr
extract_many_posts_url_bskyr <- function(posts) {
  author_did <- posts %>%
    pluck("author") %>%
    map("did") %>%
    unlist()

  post_uri <- posts %>% pluck("uri") %>% unlist()

  map2(post_uri, author_did, create_post_url_bskyr)
}

#' Create post ID from many posts
#'
#' @param posts A list of post objects
#' @return A list of IDs
#' @importFrom purrr map
#' @export
#' @rdname extract_elements_bskyr
extract_many_posts_id_bskyr <- function(posts) {
  extract_many_posts_url_bskyr(posts) %>%
    map(digest, algo = "md5")
}
