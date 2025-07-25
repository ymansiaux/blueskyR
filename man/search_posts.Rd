% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_posts.R
\name{search_posts}
\alias{search_posts}
\title{Search posts for a given keyword}
\usage{
search_posts(
  keyword,
  access_jwt,
  cursor = NULL,
  sort = "latest",
  since = NULL,
  until = NULL,
  number_of_posts_per_request = 100,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts",
  max_retries = 20,
  delay_between_retries = NULL,
  errors_for_retries = c(420, 500, 503),
  verbose = TRUE
)
}
\arguments{
\item{keyword}{Keyword to search for}

\item{access_jwt}{Access token}

\item{cursor}{Optional cursor to resume from a specific point}

\item{sort}{Sort order for the search}

\item{since}{Start date for the search}

\item{until}{End date for the search}

\item{number_of_posts_per_request}{Number of posts to retrieve per request}

\item{search_url}{Search URL}

\item{max_retries}{Maximum number of retries}

\item{delay_between_retries}{Delay between retries}

\item{errors_for_retries}{Errors for retries}

\item{verbose}{Whether to print progress messages}
}
\value{
List with posts and next cursor for resumption
}
\description{
Search posts for a given keyword
}
\details{
You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/app-bsky-feed-search-posts}
Additionnaly more information about the "post" object is available here: \url{https://atproto.blue/en/latest/atproto/atproto_client.models.app.bsky.feed.defs.html#atproto_client.models.app.bsky.feed.defs.PostView}
}
