#' Perform Simple T-Test with Clear Interpretation
#'
#' This function performs a one-sample or two-sample t-test and provides
#' easy-to-understand results and interpretation.
#'
#' @param x A numeric vector for the first group or single sample
#' @param y A numeric vector for the second group (optional, for two-sample test)
#' @param mu The hypothesized population mean for one-sample test (default = 0)
#' @param alternative Character string specifying the alternative hypothesis.
#'   Must be one of "two.sided" (default), "greater", or "less"
#' @param conf.level Confidence level (default = 0.95)
#' @param paired Logical indicating whether to perform a paired t-test (default = FALSE)
#'
#' @return A list containing:
#' \describe{
#'   \item{test_type}{Type of t-test performed}
#'   \item{statistic}{T-statistic value}
#'   \item{p.value}{P-value of the test}
#'   \item{conf.int}{Confidence interval}
#'   \item{estimate}{Sample mean(s)}
#'   \item{interpretation}{Plain English interpretation of results}
#'   \item{significant}{Logical indicating if result is statistically significant}
#' }
#'
#' @examples
#' # One-sample t-test
#' sample1 <- rnorm(30, mean = 105, sd = 10)
#' result1 <- simple_ttest(sample1, mu = 100)
#' print(result1$interpretation)
#'
#' # Two-sample t-test
#' group1 <- rnorm(25, mean = 50, sd = 8)
#' group2 <- rnorm(25, mean = 55, sd = 8)
#' result2 <- simple_ttest(group1, group2)
#' print(result2$interpretation)
#'
#' @export
simple_ttest <- function(x, y = NULL, mu = 0, alternative = "two.sided",
                         conf.level = 0.95, paired = FALSE) {

  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  if (!is.null(y) && !is.numeric(y)) {
    stop("y must be a numeric vector if provided")
  }

  # Remove missing values
  x <- x[!is.na(x)]
  if (!is.null(y)) {
    y <- y[!is.na(y)]
  }

  # Perform appropriate t-test
  if (is.null(y)) {
    # One-sample t-test
    test_result <- t.test(x, mu = mu, alternative = alternative, conf.level = conf.level)
    test_type <- "One-sample t-test"
    estimate <- mean(x)

    # Interpretation
    if (test_result$p.value < (1 - conf.level)) {
      significance <- "statistically significant"
      conclusion <- paste("reject the null hypothesis that the mean equals", mu)
    } else {
      significance <- "not statistically significant"
      conclusion <- paste("fail to reject the null hypothesis that the mean equals", mu)
    }

    interpretation <- paste(
      "Based on the", test_type, "with", length(x), "observations:",
      "\n- Sample mean:", round(estimate, 3),
      "\n- T-statistic:", round(test_result$statistic, 3),
      "\n- P-value:", round(test_result$p.value, 4),
      "\n- Result is", significance, "at", conf.level * 100, "% confidence level",
      "\n- Conclusion: We", conclusion
    )

  } else {
    # Two-sample t-test
    test_result <- t.test(x, y, alternative = alternative, conf.level = conf.level, paired = paired)
    test_type <- ifelse(paired, "Paired t-test", "Two-sample t-test")
    estimate <- c(mean(x), mean(y))
    names(estimate) <- c("Group 1 mean", "Group 2 mean")

    # Interpretation
    if (test_result$p.value < (1 - conf.level)) {
      significance <- "statistically significant"
      conclusion <- "reject the null hypothesis of equal means"
    } else {
      significance <- "not statistically significant"
      conclusion <- "fail to reject the null hypothesis of equal means"
    }

    interpretation <- paste(
      "Based on the", test_type, "comparing", length(x), "vs", length(y), "observations:",
      "\n- Group 1 mean:", round(mean(x), 3),
      "\n- Group 2 mean:", round(mean(y), 3),
      "\n- Mean difference:", round(mean(x) - mean(y), 3),
      "\n- T-statistic:", round(test_result$statistic, 3),
      "\n- P-value:", round(test_result$p.value, 4),
      "\n- Result is", significance, "at", conf.level * 100, "% confidence level",
      "\n- Conclusion: We", conclusion
    )
  }

  result <- list(
    test_type = test_type,
    statistic = test_result$statistic,
    p.value = test_result$p.value,
    conf.int = test_result$conf.int,
    estimate = estimate,
    interpretation = interpretation,
    significant = test_result$p.value < (1 - conf.level)
  )

  class(result) <- "simple_ttest"
  return(result)
}
