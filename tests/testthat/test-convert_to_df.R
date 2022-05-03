example_tidy_df <- convert_to_df(test_path("StreamingHistory1.json"))
usethis::use_data(example_tidy_df, overwrite = TRUE)

test_that("convert_to_df works", {

  res <- convert_to_df(test_path("StreamingHistory1.json"))

  expect_equal(example_tidy_df, res)

})
