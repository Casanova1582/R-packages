test_that("log_summed_exps works", {
  expect_equal(log_summed_exps(1), 1)
  expect_true(is.finite(log_summed_exps(1:2000)))
})
