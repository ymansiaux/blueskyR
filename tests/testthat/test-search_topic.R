conf <- new.env()
conf$data_dir <- tempdir()
unlink(list.files(conf$data_dir, full.names = TRUE, recursive = TRUE))


test_that("search_topic works", {
  skip_on_ci()
  plan <- list(
    research_max_date = "2025-07-15T00:00:00",
    research_min_date = "2025-07-10T00:00:00",
    requests = 0
  )

  has_more <- TRUE
  expect_no_error(
    while (has_more) {
      plan <- search_topic(plan, "covid19", "covid19", output_in_scala = FALSE)
      has_more <- plan$has_more
    }
  )
  expect_equal(plan$requests, 3)

  all_files <- list.files(
    conf$data_dir,
    full.names = TRUE,
    recursive = TRUE
  )
  json_files <- all_files[grepl("^\\d{4}.*\\.json$", basename(all_files))]

  expect_equal(length(json_files), 1)

  json_content <- jsonlite::read_json(json_files[1])
  expect_true(
    json_content[[1]]$created_from >=
      transform_date_to_utc(plan$boundaries_date_min)
  )
  expect_true(
    json_content[[1]]$created_from <=
      transform_date_to_utc(plan$boundaries_date_max)
  )

  expect_equal(json_content[[1]]$topic, "covid19")
})

unlink(list.files(conf$data_dir, full.names = TRUE, recursive = TRUE))
