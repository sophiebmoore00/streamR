example_top_songs <- top_songs(test_path("StreamingHistory1.json"))
usethis::use_data(example_top_songs, overwrite = TRUE)
test_that("top songs works", {

  res <- top_songs(test_path("StreamingHistory1.json"))

  expect_equal(example_top_songs, res)

})
