# Testing if month_wrap returns correct class of object
ex_month_wrap <- typeof(month_wrap(test_path("StreamingHistory1.json"), dmonth = "05"))
usethis::use_data(ex_month_wrap, overwrite = TRUE)

test_that("month wrap returns correct class of object", {

  ex <- typeof(month_wrap(test_path("StreamingHistory1.json"), dmonth = "05"))

  expect_equal(ex_month_wrap, ex)

})


# Testing if month_wrap returns correct error if no file path given
test_that("month wrap returns correct error", {

  expect_error(month_wrap(dmonth = "05"), 'Needs data file input.')

})

# Testing if year_wrap returns correct class of object
ex_year_wrap <- typeof(year_wrap(test_path("StreamingHistory1.json")))
usethis::use_data(ex_year_wrap, overwrite = TRUE)

test_that("year wrap returns correct class of object", {

  ex <- typeof(year_wrap(test_path("StreamingHistory1.json")))

  expect_equal(ex_month_wrap, ex)

})


# Testing if year_wrap returns correct if no file path given
test_that("year wrap returns correct error", {

  expect_error(year_wrap(), 'Needs data file input.')

})


# Testing if plot_top_artists returns correct class of object
ex_artists <- typeof(plot_top_artists(test_path("StreamingHistory1.json"), n = 10))
usethis::use_data(ex_artists, overwrite = TRUE)

test_that("plot of top artist returns correct class of object", {

  ex <- typeof(plot_top_artists(test_path("StreamingHistory1.json"), n = 10))

  expect_equal(ex_artists, ex)

})


# Testing if plot of top artists returns correct error
test_that("plot of top artist returns correct error", {

  expect_error(plot_top_artists(n = 10), 'Needs data file input.')

})
