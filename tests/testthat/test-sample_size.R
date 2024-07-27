# tests/testthat/test-sample_size.R

library(testthat)
library(SampleSizeDiagnostics)

test_that("SampleSizeDiagnostics returns a data frame", {
  result <- SampleSizeDiagnostics(sn = 0.9, sp = 0.85, p = 0.2, w = 0.1, CI = 0.95)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 8)
  expect_equal(names(result), c("Precision", "Sensitivity", "Specificity", "Prevalence", "SS_Sensitivity", "SS_Specificity", "Total_Sample_Size", "CI"))
})
