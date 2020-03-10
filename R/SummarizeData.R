#' Summarize Data Vector
#'
#' \code{SummarizeData} returns summary statistics for a numeric vector.
#'
#' @param x a numeric vector
#' @param na.rm option to remove NAs. Default is FALSE.
#' @return A numeric vector with length of x, percent NA, percent 0, mean,
#'   standard deviation, standard error, variance, and quantile values.
#' @examples
#' SummarizeData(1:10)
#' SummarizeData(c(1:10, NA))
#' SummarizeData(c(1:10, NA), na.rm = TRUE)
#' @export

SummarizeData <- function(x, na.rm = FALSE){
  N <- ifelse(na.rm, length(x[which(!is.na(x))]), length(x))
  percent_NA <- length(x[which(is.na(x))])/length(x)
  percent_0 <- length(x[which(x == 0 & !is.na(x))])/N
  mean <- mean(x, na.rm = na.rm)
  sd <- sd(x, na.rm = na.rm)
  se <- sd(x, na.rm = na.rm)/sqrt(N)
  var <- var(x, na.rm = na.rm)
  quantiles <- quantile(x, na.rm = TRUE)
  summary_num <- c(N, percent_NA, percent_0, mean, sd, se, var, quantiles)
  names(summary_num) <- c("N", "percent_NA", "percent_0",
                          "mean", "sd", "se", "var", "min",
                          "quartile_1", "median", "quartile_3", "max")
  summary_num
}
