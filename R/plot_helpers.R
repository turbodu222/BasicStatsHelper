#' Create Publication-Ready Histogram
#'
#' This function creates a clean, publication-ready histogram with
#' customizable options for statistical analysis presentations.
#'
#' @param x A numeric vector to plot
#' @param title Character string for plot title (default = "Histogram")
#' @param xlab Character string for x-axis label (default = "Value")
#' @param ylab Character string for y-axis label (default = "Frequency")
#' @param bins Number of bins for histogram (default = 30)
#' @param add_normal Logical. Add normal curve overlay? (default = FALSE)
#' @param add_stats Logical. Add descriptive statistics text? (default = TRUE)
#' @param color Fill color for bars (default = "lightblue")
#'
#' @return A ggplot2 object
#'
#' @examples
#' # Basic histogram
#' data <- rnorm(500, mean = 100, sd = 15)
#' plot_histogram(data, title = "Sample Distribution")
#'
#' # Histogram with normal overlay and statistics
#' plot_histogram(data,
#'                title = "Distribution with Normal Overlay",
#'                add_normal = TRUE,
#'                add_stats = TRUE)
#'
#' @export
plot_histogram <- function(x, title = "Histogram", xlab = "Value", ylab = "Frequency",
                           bins = 30, add_normal = FALSE, add_stats = TRUE, color = "lightblue") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is required for this function")
  }

  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  # Remove missing values
  x_clean <- x[!is.na(x)]

  if (length(x_clean) == 0) {
    stop("No non-missing values to plot")
  }

  # Create base plot
  p <- ggplot2::ggplot(data.frame(x = x_clean), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = bins, fill = color, color = "white", alpha = 0.7) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  # Add normal curve if requested
  if (add_normal) {
    x_mean <- mean(x_clean)
    x_sd <- sd(x_clean)
    x_range <- seq(min(x_clean), max(x_clean), length.out = 100)
    normal_curve <- data.frame(
      x = x_range,
      y = dnorm(x_range, mean = x_mean, sd = x_sd) * length(x_clean) * diff(range(x_clean)) / bins
    )

    p <- p + ggplot2::geom_line(data = normal_curve, ggplot2::aes(x = x, y = y),
                                color = "red", size = 1, linetype = "dashed")
  }

  # Add statistics text if requested
  if (add_stats) {
    stats_text <- paste(
      "n =", length(x_clean),
      "\nMean =", round(mean(x_clean), 2),
      "\nSD =", round(sd(x_clean), 2)
    )

    p <- p + ggplot2::annotate("text", x = Inf, y = Inf,
                               label = stats_text,
                               hjust = 1.1, vjust = 1.1,
                               size = 3.5,
                               bbox = list(boxstyle = "round,pad=0.3", facecolor = "white", alpha = 0.8))
  }

  return(p)
}

#' Generate Synthetic Dataset for Testing
#'
#' This function generates synthetic datasets with various distributions
#' for testing statistical functions and creating examples.
#'
#' @param n Sample size (default = 100)
#' @param type Type of distribution: "normal", "uniform", "skewed", or "bimodal" (default = "normal")
#' @param seed Random seed for reproducibility (default = NULL)
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{The generated data vector}
#'   \item{true_mean}{True population mean (if applicable)}
#'   \item{true_sd}{True population standard deviation (if applicable)}
#'   \item{distribution}{Type of distribution generated}
#' }
#'
#' @examples
#' # Generate normal data
#' normal_data <- generate_synthetic_data(n = 200, type = "normal", seed = 123)
#' hist(normal_data$data)
#'
#' # Generate skewed data
#' skewed_data <- generate_synthetic_data(n = 200, type = "skewed", seed = 123)
#' hist(skewed_data$data)
#'
#' @export
generate_synthetic_data <- function(n = 100, type = "normal", seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (n <= 0) {
    stop("Sample size n must be positive")
  }

  type <- match.arg(type, c("normal", "uniform", "skewed", "bimodal"))

  result <- switch(type,
                   "normal" = {
                     true_mean <- 50
                     true_sd <- 10
                     data <- rnorm(n, mean = true_mean, sd = true_sd)
                     list(data = data, true_mean = true_mean, true_sd = true_sd, distribution = "Normal")
                   },

                   "uniform" = {
                     min_val <- 0
                     max_val <- 100
                     data <- runif(n, min = min_val, max = max_val)
                     true_mean <- (min_val + max_val) / 2
                     true_sd <- sqrt((max_val - min_val)^2 / 12)
                     list(data = data, true_mean = true_mean, true_sd = true_sd, distribution = "Uniform")
                   },

                   "skewed" = {
                     # Right-skewed using gamma distribution
                     shape <- 2
                     rate <- 0.1
                     data <- rgamma(n, shape = shape, rate = rate)
                     true_mean <- shape / rate
                     true_sd <- sqrt(shape / rate^2)
                     list(data = data, true_mean = true_mean, true_sd = true_sd, distribution = "Right-skewed (Gamma)")
                   },

                   "bimodal" = {
                     # Mixture of two normal distributions
                     n1 <- floor(n / 2)
                     n2 <- n - n1
                     data1 <- rnorm(n1, mean = 40, sd = 5)
                     data2 <- rnorm(n2, mean = 60, sd = 5)
                     data <- c(data1, data2)
                     data <- sample(data)  # Shuffle to mix
                     list(data = data, true_mean = 50, true_sd = sqrt(125), distribution = "Bimodal")
                   }
  )

  return(result)
}
