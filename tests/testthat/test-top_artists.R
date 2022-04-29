# Test with `method = "streams"`
example_top_artists_s <- top_artists(test_path("StreamingHistory1.json"), method = "streams")
usethis::use_data(example_top_artists_s, overwrite = TRUE)

test_that("top artists works with streams", {

  res <- top_artists(test_path("StreamingHistory1.json"), method = "streams")

  expect_equal(example_top_artists_s, res)

})

# Test with `method = "minutes"`
example_top_artists_m <- top_artists(test_path("StreamingHistory1.json"), method = "minutes")
usethis::use_data(example_top_artists_m, overwrite = TRUE)

test_that("top artists works with minutes", {

  res <- top_artists(test_path("StreamingHistory1.json"), method = "minutes")

  expect_equal(example_top_artists_m, res)

})
