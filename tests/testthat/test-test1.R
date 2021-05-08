test_that("test for a single sumulation dataset", {
  data <- SimSAUC::SimData1_100
  y <- data[[2]]
  x <- as.data.frame(data[[1]])
  error <- SimSAUC(y, x, sim_size=10, parallel=T, n_core=1, ratio=2/3, seed=2021)$error_SAUC
  res <- 0 < error & error < 1
  expect_equal(res, TRUE)
})
