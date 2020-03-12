#' Check Class of data provided to x parameter in ModelMultiData
#'
#' \code{CheckXClass} is an internal function that checks the class of x
#' parameter in ModelMultiData function.
#'
#' @param x matrix or list of matrices
#' @return list of matrices

CheckXClass <- function(x){
  if (class(x) == "list"){
    for (i in 1:length(x)){
      if (class(x[[i]]) != "matrix"){
        x[[i]] <- as.matrix(x[[i]])
        warning(paste0("Warning: Coercing list item ", i,
                       " of x to matrix: some data may be lost\n"))
      }
      if(is.null(rownames(x[[i]]))){
        rownames(x[[i]]) <- seq_len(nrow(x[[i]]))
      }
      if(is.null(colnames(x[[i]]))){
        stop(paste0("Error: column names missing from list element ",
                    i, "of x\n"))
      }
    }
  } else {
    if (class(x) != "matrix"){
      warning("Warning: Coercing x to matrix: some data may be lost\n")
      x <- as.matrix(x)
    }
    if(is.null(rownames(x))){
      rownames(x) <- seq_len(nrow(x))
    }
    if(is.null(colnames(x))){
      stop("Error: column names missing from x\n")
    }
    x <- list(x)
  }
  x
}
