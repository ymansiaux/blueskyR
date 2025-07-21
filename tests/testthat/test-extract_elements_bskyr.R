example_posts_bskyr <- list(
  example_post,
  post_with_2_langs,
  post_without_hashtags,
  post_with_many_hashtags
)

bskyr_like_output <- tibble::tibble(
  "author" = purrr::map(example_posts_bskyr, "author"),
  "record" = purrr::map(example_posts_bskyr, "record"),
  "uri" = purrr::map_chr(example_posts_bskyr, "uri")
)
# Check many posts

test_that("bsky_extract_many_posts_author_infos_bskyr works", {
  author_infos <- bsky_extract_many_posts_author_infos_bskyr(bskyr_like_output)
  expect_equal(
    author_infos[[1]]$author_handle,
    example_posts_bskyr[[1]]$author$handle
  )
  expect_equal(
    author_infos[[1]]$author_did,
    example_posts_bskyr[[1]]$author$did
  )
  expect_equal(
    author_infos[[1]]$created_at,
    example_posts_bskyr[[1]]$author$createdAt
  )
  expect_equal(
    author_infos[[2]]$author_handle,
    example_posts_bskyr[[2]]$author$handle
  )
  expect_equal(
    author_infos[[2]]$author_did,
    example_posts_bskyr[[2]]$author$did
  )
  expect_equal(
    author_infos[[2]]$created_at,
    example_posts_bskyr[[2]]$author$createdAt
  )
  expect_equal(
    author_infos[[3]]$author_handle,
    example_posts_bskyr[[3]]$author$handle
  )
  expect_equal(
    author_infos[[3]]$author_did,
    example_posts_bskyr[[3]]$author$did
  )
  expect_equal(
    author_infos[[3]]$created_at,
    example_posts_bskyr[[3]]$author$createdAt
  )
  expect_equal(
    author_infos[[4]]$author_handle,
    example_posts_bskyr[[4]]$author$handle
  )
  expect_equal(
    author_infos[[4]]$author_did,
    example_posts_bskyr[[4]]$author$did
  )
  expect_equal(
    author_infos[[4]]$created_at,
    example_posts_bskyr[[4]]$author$createdAt
  )
})


test_that("bsky_extract_many_posts_text_bskyr works", {
  text <- bsky_extract_many_posts_text_bskyr(bskyr_like_output)
  expect_equal(text[[1]], example_posts_bskyr[[1]]$record$text)
  expect_equal(text[[2]], example_posts_bskyr[[2]]$record$text)
  expect_equal(text[[3]], example_posts_bskyr[[3]]$record$text)
  expect_equal(text[[4]], example_posts_bskyr[[4]]$record$text)
})

test_that("bsky_extract_many_posts_created_at_bskyr works", {
  created_at <- bsky_extract_many_posts_created_at_bskyr(bskyr_like_output)
  expect_equal(created_at[[1]], example_posts_bskyr[[1]]$record$createdAt)
  expect_equal(created_at[[2]], example_posts_bskyr[[2]]$record$createdAt)
  expect_equal(created_at[[3]], example_posts_bskyr[[3]]$record$createdAt)
  expect_equal(created_at[[4]], example_posts_bskyr[[4]]$record$createdAt)
})

test_that("bsky_extract_many_posts_langs_bskyr works", {
  langs <- bsky_extract_many_posts_langs_bskyr(bskyr_like_output)
  expect_equal(langs[[1]], "de")
  expect_equal(langs[[2]], "aa|de")
  expect_equal(langs[[3]], "")
  expect_equal(langs[[4]], "en")
})

test_that("bsky_extract_many_posts_hashtags_bskyr works", {
  hashtags <- bsky_extract_many_posts_hashtags_bskyr(bskyr_like_output)
  expect_equal(hashtags[[1]], c("Übertragung", "Influenza"))
  expect_equal(
    hashtags[[2]],
    c("Covidisnotover", "maskup", "cleanair", "Maskenpflicht", "Covid19")
  )
  expect_equal(hashtags[[3]], NULL)
  expect_equal(
    hashtags[[4]],
    c(
      "WearAMask",
      "Covid19",
      "CoronaVirus",
      "PandemicPanda",
      "WednesdayMorning"
    )
  )
})

test_that("bsky_extract_many_posts_elements_bskyr works", {
  elements <- bsky_extract_many_posts_elements_bskyr(bskyr_like_output)
  expect_equal(
    elements[[1]]$author_infos$author_handle,
    example_posts_bskyr[[1]]$author$handle
  )
  expect_equal(
    elements[[1]]$author_infos$author_did,
    example_posts_bskyr[[1]]$author$did
  )
  expect_equal(
    elements[[1]]$author_infos$created_at,
    example_posts_bskyr[[1]]$author$createdAt
  )
  expect_equal(elements[[1]]$text, example_posts_bskyr[[1]]$record$text)
  expect_equal(elements[[1]]$langs, "de")
  expect_equal(elements[[1]]$hashtags, c("Übertragung", "Influenza"))
  expect_equal(
    elements[[1]]$url,
    "https://bsky.app/profile/did:plc:k6vq7z2d5lyhhqnxs72tlchg/post/3ltlzhcvjqc2g"
  )
  expect_equal(
    elements[[1]]$id,
    digest::digest(elements[[1]]$url, algo = "md5")
  )

  expect_equal(
    names(elements),
    purrr::map(elements, "id") %>% purrr::reduce(c)
  )
})

test_that("bsky_extract_many_posts_url_bskyr works", {
  post_url <- bsky_extract_many_posts_url_bskyr(bskyr_like_output)
  expect_equal(
    post_url[[1]],
    "https://bsky.app/profile/did:plc:k6vq7z2d5lyhhqnxs72tlchg/post/3ltlzhcvjqc2g"
  )
})
