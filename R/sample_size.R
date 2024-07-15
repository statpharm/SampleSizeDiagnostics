#' Calculate Sample Size for Evaluating a Diagnostic Test
#'
#' This function calculates the sample size needed for evaluating a diagnostic test based on sensitivity, specificity, prevalence, and desired precision.
#'
#' @param sn Sensitivity of the diagnostic test.
#' @param sp Specificity of the diagnostic test.
#' @param p Prevalence of the disease.
#' @param w Desired width of the confidence interval (default is 0.10).
#' @param CI Confidence interval level, either 0.95 or 0.9 (default is 0.95).
#' @return A data frame containing the calculated sample sizes and input parameters.
#' @examples
#' calculate_sample_size()
#' @export


calculate_sample_size <- function(sn , sp , p , w = 0.10, CI = 0.95) {
  # Calculate TP+FN
  a_c <- (1.96^2) * sn * (1 - sn) / (w^2) # 1.645 for 90% or 1.96 for 95% CI
  if (CI == 0.9) {
    a_c <- (1.645^2) * sn * (1 - sn) / (w^2)
  }
  
  # Calculate N1
  n1 <- a_c / p
  # Round up to the next whole integer
  n1_int <- floor(n1)
  if (n1 != n1_int) {
    n1 <- n1_int + 1
  }
  
  # Calculate FP+TN
  b_d <- (1.96^2) * sp * (1 - sp) / (w^2) # 1.645 for 90% or 1.96 for 95% CI
  if (CI == "90%") {
    b_d <- (1.645^2) * sp * (1 - sp) / (w^2)
  }
  
  # Calculate N2
  n2 <- b_d / (1 - p)
  # Round up to the next whole integer
  n2_int <- floor(n2)
  if (n2 != n2_int) {
    n2 <- n2_int + 1
  }
  
  # Get final sample size
  if (n1 > n2) {
    n <- n1
  } else if (n2 > n1) {
    n <- n2
  } else {
    n <- n1
  }
  

  # Create a data frame with the results
  mySS <- data.frame(Precision = w, Sensitivity = sn,
                     Specificity = sp, Prevalence = p, N1 = n1, N2 = n2, Total_Subjects = n, CI = CI)
  
  # Print the sample size
  print(mySS, row.names = FALSE)
}
