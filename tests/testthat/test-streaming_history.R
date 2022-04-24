example_top_artists <- top_artists(test_path("StreamingHistory1.json"))
usethis::use_data(example_top_artists, overwrite = TRUE)
test_that("top artists works", {

  res <- top_artists(test_path("StreamingHistory1.json"))

  expect_equal(example_top_artists, res)

})

example_top_songs <- top_songs(test_path("StreamingHistory1.json"))
usethis::use_data(example_top_songs, overwrite = TRUE)
test_that("top songs works", {

  res <- top_songs(test_path("StreamingHistory1.json"))

  expect_equal(example_top_songs, res)

})
