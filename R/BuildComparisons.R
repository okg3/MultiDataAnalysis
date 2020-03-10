#' Construct comparisons matrix
#'
#' \code{BuildComparisons} is an internal function that construcs the
#' comparisons matrix for ModelMultiData if comparisons not defined.
#'
#' This function constructs the comparisons matrix if not provided. By default,
#' if all matrices in x have the same rownames, ows with the same name will be
#' grouped. Else, comparisons includes all possible combinations of rownames in
#' x and y. Column names match values in yName and xName. Rows contain
#' respective rownames or row numbers from each dataset in x and y that should
#' be tested together.
#'
#' @param x A matrix or list of matrices containing values for model.
#' @param y A matrix containing values to be used on left-hand side of model
#'   (e.g. dependent variable). At least one of y or groups must be provided.
#' @param xName Names of matrices provided in x. If x is a list with named
#'   elements, defaults to list names. Else, defaults to x1, x2, ..., xn where n
#'   is the number of matrices provided in x.
#' @param yName Name of matrix or name of column in groups to be used on
#'   left-hand of model. (see formula details for default values)
#' @return A matrix with all combinations of rows in x and y to be tested. 

BuildComparisons <- function(x, y, xName, yName){
  if (length(x) > 1){
    rowMatch <- unlist(lapply(x[-1], function(m){
      all(rownames(x[[1]]) %in% rownames(m),
          rownames(m) %in% rownames(x[[1]]))
    }))
    allRowMatch <- all(rowMatch)
    if (allRowMatch){
      comparisons <- matrix(
        rownames(x[[1]]), length(rownames(x[[1]])), length(x))
      colnames(comparisons) <- xName
    } else {
      comparisons <- lapply(x, rownames)
      comparisons <- as.matrix(expand.grid(
        comparisons, stringsAsFactors = FALSE))
    }
  } else {
    comparisons <- as.matrix(rownames(x[[1]]))
    colnames(comparisons) <- xName
    allRowMatch <- TRUE
  }
  
  if (!is.null(y)){
    if (all(allRowMatch, comparisons[, 1] %in% rownames(y),
            rownames(y) %in% comparisons[, 1])){
      comparisons <- cbind(comparisons[, 1], comparisons)
    } else {
      comparisons <- lapply(rownames(y), function(n){
        cbind(n, comparisons)
      })
      comparisons <- Reduce(rbind, comparisons)
    }
    colnames(comparisons)[1] <- yName
  }
  return(comparisons)
}
