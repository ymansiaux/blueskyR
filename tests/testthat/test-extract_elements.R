example_post <- list(
  uri = "at://did:plc:k6vq7z2d5lyhhqnxs72tlchg/app.bsky.feed.post/3ltlzhcvjqc2g",
  cid = "bafyreia3ndujo2eoctbkhup3t2po2ucnidhgoctexu3hrghl4folrlkbqq",
  author = list(
    did = "did:plc:k6vq7z2d5lyhhqnxs72tlchg",
    handle = "egibertegoners.bsky.social",
    displayName = "Paying_attention_to_science",
    avatar = "https://cdn.bsky.app/img/avatar/plain/did:plc:k6vq7z2d5lyhhqnxs72tlchg/bafkreig6zutb4ursus7zc74dqwwpruzpcevosupggmfvctg3gxvor4jrim@jpeg",
    associated = list(
      chat = list(allowIncoming = "following"),
      activitySubscription = list(allowSubscriptions = "followers")
    ),
    viewer = list(muted = FALSE, blockedBy = FALSE),
    labels = list(
      list(
        src = "did:plc:k6vq7z2d5lyhhqnxs72tlchg",
        uri = "at://did:plc:k6vq7z2d5lyhhqnxs72tlchg/app.bsky.actor.profile/self",
        cid = "bafyreigamfrihqs462yaivhqh7fet4dra7gt2mt3gs23nu5qsqb4bdi2ve",
        val = "!no-unauthenticated",
        cts = "1970-01-01T00:00:00.000Z"
      )
    ),
    createdAt = "2023-09-06T21:52:30.540Z"
  ),
  record = list(
    `$type` = "app.bsky.feed.post",
    createdAt = "2025-07-10T09:31:10.092Z",
    embed = list(
      `$type` = "app.bsky.embed.external",
      external = list(
        description = "",
        title = " Übertragung » Influenza » Krankheiten » HNO-Ärzte-im-Netz » ",
        uri = "https://www.hno-aerzte-im-netz.de/krankheiten/influenza/uebertragung.html"
      )
    ),
    facets = list(
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Übertragung"
        )),
        index = list(byteEnd = 247L, byteStart = 234L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#tag",
          tag = "Influenza"
        )),
        index = list(byteEnd = 258L, byteStart = 248L)
      ),
      list(
        features = list(list(
          `$type` = "app.bsky.richtext.facet#link",
          uri = "https://www.hno-aerzte-im-netz.de/krankheiten/influenza/uebertragung.html"
        )),
        index = list(byteEnd = 301L, byteStart = 260L)
      )
    ),
    langs = list("de"),
    text = "Für alle, die derzeit akut respiratorisch erkrankt sind. Wenn Ärzt:innen besser informieren würden, wäre viel gewonnen. Maske tragen bei Arztbesuch sollte Standard sein. Bin seit einigen Tagen krank. Im Mai gegen Covid19 geimpft. #Übertragung #Influenza \nwww.hno-aerzte-im-netz.de/krankheiten/..."
  ),
  embed = list(
    `$type` = "app.bsky.embed.external#view",
    external = list(
      uri = "https://www.hno-aerzte-im-netz.de/krankheiten/influenza/uebertragung.html",
      title = " Übertragung » Influenza » Krankheiten » HNO-Ärzte-im-Netz » ",
      description = ""
    )
  ),
  replyCount = 0L,
  repostCount = 0L,
  likeCount = 0L,
  quoteCount = 0L,
  indexedAt = "2025-07-10T09:31:09.804Z",
  viewer = list(threadMuted = FALSE, embeddingDisabled = FALSE),
  labels = list()
)

test_that("extract_author_infos works", {
  author_infos <- extract_post_author_infos(example_post)
  expect_equal(author_infos$author_handle, example_post$author$handle)
  expect_equal(author_infos$author_did, example_post$author$did)
  expect_equal(author_infos$created_at, example_post$author$createdAt)
})


test_that("extract_text works", {
  text <- extract_post_text(example_post)
  expect_equal(text, example_post$record$text)
})

test_that("extract_created_at works", {
  created_at <- extract_post_created_at(example_post)
  expect_equal(created_at, example_post$record$createdAt)
})

test_that("extract_langs works", {
  langs <- extract_post_langs(example_post)
  expect_equal(langs, "de")
})

test_that("extract_hashtags works", {
  hashtags <- extract_post_hashtags(example_post)
  expect_equal(hashtags, c("Übertragung", "Influenza"))
})

test_that("extract_hashtags_from_text works", {
  text <- extract_post_text(example_post)
  hashtags <- extract_post_hashtags_from_text(text)
  expect_equal(hashtags, c("Übertragung", "Influenza"))
})

test_that("create_post_url works", {
  post_url <- create_post_url(example_post)
  expect_equal(
    post_url,
    "https://bsky.app/profile/did:plc:k6vq7z2d5lyhhqnxs72tlchg/post/3ltlzhcvjqc2g"
  )
})

# Check when posts have missing elements

test_that("extract_author_infos works", {
  post_missing_author <- example_post
  post_missing_author$author$handle <- NULL
  author_infos <- extract_post_author_infos(post_missing_author)
  expect_equal(author_infos$author_handle, NULL)
  expect_equal(author_infos$author_did, post_missing_author$author$did)
  expect_equal(author_infos$created_at, post_missing_author$author$createdAt)
})


test_that("extract_text works", {
  post_missing_text <- example_post
  post_missing_text$record$text <- NULL
  text <- extract_post_text(post_missing_text)
  expect_equal(text, NULL)
})

test_that("extract_created_at works", {
  post_missing_created_at <- example_post
  post_missing_created_at$record$createdAt <- NULL
  created_at <- extract_post_created_at(post_missing_created_at)
  expect_equal(created_at, NULL)
})

test_that("extract_langs works", {
  post_missing_langs <- example_post
  post_missing_langs$record$langs <- NULL
  langs <- extract_post_langs(post_missing_langs)
  expect_equal(langs, "")
})

test_that("extract_hashtags works", {
  post_missing_hashtags <- example_post
  post_missing_hashtags$record$facets <- NULL
  hashtags <- extract_post_hashtags(post_missing_hashtags)
  expect_equal(hashtags, NULL)
})

test_that("extract_hashtags_from_text works", {
  post_missing_hashtags_from_text <- example_post
  post_missing_hashtags_from_text$record$text <- NULL
  hashtags <- extract_post_hashtags_from_text(post_missing_hashtags_from_text)
  expect_equal(hashtags, character(0))
})

test_that("create_post_url works", {
  post_missing_url <- example_post
  post_missing_url$uri <- NULL
  post_url <- create_post_url(post_missing_url)
  expect_equal(
    post_url,
    NULL
  )
})
