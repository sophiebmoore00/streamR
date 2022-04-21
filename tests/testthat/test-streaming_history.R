test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("top artists works", {
  expect_equal(top_artists("musicmusicmusic/StreamingHistory1.json"))
})
