
library(morphfoil)

test_that("correct arguments for read_mesh", {
  expect_error(read_mesh(1), " wrong 'name' argument")
  expect_error(read_mesh(c(1, "a")), " wrong 'name' argument")
  expect_error(read_mesh(c("a", "a")), " wrong 'name' argument")
})
