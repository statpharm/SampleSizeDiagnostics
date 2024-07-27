#' Calculate Sample Size for Evaluating a Diagnostic Test
#'
#' This function calculates the sample size needed for evaluating a diagnostic test based on sensitivity, specificity, prevalence, and desired precision.
#'
#' @param sn Sensitivity of the diagnostic test.
#' @param sp Specificity of the diagnostic test.
#' @param p Prevalence of the disease.
#' @param w Desired width of the confidence interval (default is 0.10).
#' @param CI Confidence interval level, either 0.95 or 0.9 (default is 0.95). Only 0.95 and 0.9 are allowed.
#' @return A data frame containing the calculated sample sizes and input parameters:
#' \describe{
#'   \item{Precision}{Desired width of the confidence interval}
#'   \item{Sensitivity}{Sensitivity of the diagnostic test}
#'   \item{Specificity}{Specificity of the diagnostic test}
#'   \item{Prevalence}{Prevalence of the disease}
#'   \item{SS_sensitivity}{Sample size for sensitivity}
#'   \item{SS_specificity}{Sample size for specificity}
#'   \item{Total_Sample_Size}{Total sample size needed (maximum of ss_sensitivity and ss_specificity)}
#'   \item{CI}{Confidence interval level}
#' }
#'
#' @details
#' Abstract of Buderer (1996): Careful consideration of statistical issues related to the choice of a sample size is critical for achieving
#' meaningful results in research studies designed to evaluate diagnostic tests. When assessing the ability of a
#' diagnostic test to screen for disease, the parameters sensitivity, specificity, and predictive values are of interest.
#' Study sample size requirements can be calculated based on a clinically acceptable degree of precision. the
#' hypothesized values of sensitivity and specificity, and the estimated prevalence of disease in the target population. The simple methods and tables in this paper guide the researcher when deciding how many subjects
#' to sample in a study designed to estimate both the sensitivity and the specificity of a diagnostic test, given a
#' specified precision and estimated disease prevalence.
#'
#' @references Buderer, N. M. F. (1996). Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. Academic Emergency Medicine, 3(9), 895-900.
#'
#' @examples
#' SampleSizeDiagnostics(sn = 0.9, sp = 0.85, p = 0.2, w = 0.1, CI = 0.95)
#' SampleSizeDiagnostics(sn = 0.9, sp = 0.85, p = 0.2, w = 0.1, CI = 0.9)
#' @export
SampleSizeDiagnostics <- function(sn, sp, p, w = 0.10, CI = 0.95) {
  if (!CI %in% c(0.95, 0.9)) {
    stop("CI must be either 0.95 or 0.9")
  }
  
  z <- if (CI == 0.9) 1.645 else 1.96
  a_c <- (z^2) * sn * (1 - sn) / (w^2)
  n1 <- ceiling(a_c / p)
  
  b_d <- (z^2) * sp * (1 - sp) / (w^2)
  n2 <- ceiling(b_d / (1 - p))
  
  n <- max(n1, n2)
  
  mySS <- data.frame(
    Precision = w, 
    Sensitivity = sn,
    Specificity = sp, 
    Prevalence = p, 
    SS_Sensitivity = n1, 
    SS_Specificity = n2, 
    Total_Sample_Size = n, 
    CI = CI
  )
  
  return(mySS)
}
