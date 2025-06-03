#' Calculate Confidence Interval for Mean
#'
#' This function calculates the confidence interval for the population mean
#' using the t-distribution (for small samples) or normal distribution (for large samples).
#'
#' @param x A numeric vector of sample data
#' @param conf.level Confidence level (default = 0.95 for 95% confidence interval)
#' @param na.rm Logical. Should missing values be removed? Default is TRUE.
#'
#' @return A list containing:
#' \describe{
#'   \item{mean}{Sample mean}
#'   \item{lower}{Lower bound of confidence interval}
#'   \item{upper}{Upper bound of confidence interval}
#'   \item{conf.level}{Confidence level used}
#'   \item{n}{Sample size}
#'   \item{se}{Standard error of the mean}
#'   \item{margin_error}{Margin of error}
#' }
#'
#' @examples
#' # Generate sample data
#' sample_data <- rnorm(50, mean = 100, sd = 15)
#'
#' # Calculate 95% confidence interval
#' ci_95 <- confidence_interval(sample_data)
#' print(ci_95)
#'
#' # Calculate 99% confidence interval
#' ci_99 <- confidence_interval(sample_data, conf.level = 0.99)
#' print(ci_99)
#'
#' @export
confidence_interval <- function(x, conf.level = 0.95, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  if (conf.level <= 0 || conf.level >= 1) {
    stop("Confidence level must be between 0 and 1")
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) < 2) {
    stop("Need at least 2 observations to calculate confidence interval")
  }

  n <- length(x)
  mean_x <- mean(x)
  se <- sd(x) / sqrt(n)

  # Use t-distribution for degrees of freedom
  alpha <- 1 - conf.level
  t_value <- qt(1 - alpha/2, df = n - 1)

  margin_error <- t_value * se
  lower <- mean_x - margin_error
  upper <- mean_x + margin_error

  result <- list(
    mean = mean_x,
    lower = lower,
    upper = upper,
    conf.level = conf.level,
    n = n,
    se = se,
    margin_error = margin_error
  )

  class(result) <- "confidence_interval"
  return(result)
}
