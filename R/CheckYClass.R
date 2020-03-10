#' Check Class of data provided to y parameter in ModelMultiData
#'
#' \code{CheckYClass} is an internal function that checks the class of y
#' parameter in ModelMultiData function.
#'
#' @param y A matrix containing values to be used on left-hand side of model
#'   (e.g. dependent variable).
#' @return matrix 

CheckYClass <- function(y){
  if (class(y) != "matriy"){
    warning("Coercing y to matrix: some data may be lost")
    y <- as.matrix(y)
  }
  if(is.null(rownames(y))){
    rownames(y) <- seq_len(nrow(y))
  }
  if(is.null(colnames(y))){
    stop("column names missing from y")
  }
  y
}
