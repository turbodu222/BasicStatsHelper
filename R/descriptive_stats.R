#' Calculate Descriptive Statistics
#'
#' This function calculates common descriptive statistics for a numeric vector,
#' including mean, median, standard deviation, quartiles, and more.
#'
#' @param x A numeric vector
#' @param na.rm Logical. Should missing values be removed? Default is TRUE.
#'
#' @return A list containing descriptive statistics:
#' \describe{
#'   \item{mean}{The arithmetic mean}
#'   \item{median}{The median value}
#'   \item{sd}{Standard deviation}
#'   \item{var}{Variance}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#'   \item{q25}{First quartile (25th percentile)}
#'   \item{q75}{Third quartile (75th percentile)}
#'   \item{n}{Number of observations}
#'   \item{missing}{Number of missing values}
#' }
#'
#' @examples
#' # Generate some sample data
#' data <- rnorm(100, mean = 50, sd = 10)
#'
#' # Calculate descriptive statistics
#' stats <- descriptive_stats(data)
#' print(stats)
#'
#' @export
descriptive_stats <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  missing_count <- sum(is.na(x))

  if (na.rm) {
    x_clean <- x[!is.na(x)]
  } else {
    x_clean <- x
  }

  if (length(x_clean) == 0) {
    stop("No non-missing values to analyze")
  }

  stats <- list(
    mean = mean(x_clean, na.rm = na.rm),
    median = median(x_clean, na.rm = na.rm),
    sd = sd(x_clean, na.rm = na.rm),
    var = var(x_clean, na.rm = na.rm),
    min = min(x_clean, na.rm = na.rm),
    max = max(x_clean, na.rm = na.rm),
    q25 = quantile(x_clean, 0.25, na.rm = na.rm),
    q75 = quantile(x_clean, 0.75, na.rm = na.rm),
    n = length(x_clean),
    missing = missing_count
  )

  class(stats) <- "descriptive_stats"
  return(stats)
}
