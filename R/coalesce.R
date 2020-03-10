#' Coalesce vectors with NAs
#'
#' \code{coalesce} returns value which is not NA across columns of a data.frame
#' or a list of vectors
#' @param ... data.frame or list of vectors of equal length
#' @return A vector of coalesced values
#' @examples
#' dat <- list(c(1, NA, NA), c(NA, 2, NA), c(NA, NA, 3))
#' coalesce(dat)
#' dat <- data.frame(c(1, NA, NA), c(NA, 2, NA), c(NA, NA, 3))
#' coalesce(dat)
#' @export

coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x
  },
  ...)
}
