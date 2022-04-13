test_that("argument types are respected in code_chunk()", {
  chunk1 <- ymlthis::code_chunk(
    plot(1:5),
    chunk_args = list(fig.cap = "something")
  )

  chunk2 <- ymlthis::code_chunk(
    plot(1:5),
    chunk_args = list(fig.cap = label_object)
  )

  chunk3 <- ymlthis::code_chunk(
    plot(1:5),
    chunk_args = list(fig.height = 3)
  )

  expect_snapshot(cat(chunk1))
  expect_snapshot(cat(chunk2))
  expect_snapshot(cat(chunk3))
})
