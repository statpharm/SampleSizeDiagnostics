calculate_sample_size <- function(sn = 0.90, sp = 0.85, p = 0.20, w = 0.10, Disease = "Disease Name", CI = "95%") {
  # Step 2: Calculate TP+FN
  a_c <- (1.96^2) * sn * (1 - sn) / (w^2) # 1.645 for 90% or 1.96 for 95% CI
  if (CI == "90%") {
    a_c <- (1.645^2) * sn * (1 - sn) / (w^2)
  }
  
  # Step 3: Calculate N1
  n1 <- a_c / p
  # Round up to the next whole integer
  n1_int <- floor(n1)
  if (n1 != n1_int) {
    n1 <- n1_int + 1
  }
  
  # Step 4: Calculate FP+TN
  b_d <- (1.96^2) * sp * (1 - sp) / (w^2) # 1.645 for 90% or 1.96 for 95% CI
  if (CI == "90%") {
    b_d <- (1.645^2) * sp * (1 - sp) / (w^2)
  }
  
  # Step 5: Calculate N2
  n2 <- b_d / (1 - p)
  # Round up to the next whole integer
  n2_int <- floor(n2)
  if (n2 != n2_int) {
    n2 <- n2_int + 1
  }
  
  # Step 6: Get final sample size
  if (n1 > n2) {
    n <- n1
  } else if (n2 > n1) {
    n <- n2
  } else {
    n <- n1
  }
  
  # Create a data frame with the results
  mySS <- data.frame(Disease = Disease, Precision = w, Sensitivity = sn,
                     Specificity = sp, Prevalence = p, N1 = n1, N2 = n2, Total_Subjects = n, CI = CI)
  
  # Print the sample size
  print(mySS[, c(2:8)], row.names = FALSE)
}
