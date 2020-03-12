#' Find Shared Sample Names
#'
#' \code{GetSharedSamples} is an internal function that checks identifies names
#' that are shared in x, y, and the first column of groups as relevant within
#' ModelMultiData function.
#'
#' @param x matrix or list of matrices
#' @param y matrix
#' @param groups data.frame or data.table with groups data
#' @return vector of shared sample names

GetSharedSamples <- function(x, y = NULL, groups = NULL){
  sampleNames <- colnames(x[[1]])
  if (length(x) > 1){
    for (i in 1:length(x[-1])){
      sampleNames <- intersect(sampleNames, colnames(x[[i]]))
    }
  }
  if (!is.null(y)){
    sampleNames <- intersect(sampleNames, colnames(y))
  }
  if (!is.null(groups)){
    sampleNames <- intersect(sampleNames, groups[[1]])
  }
  if(length(sampleNames) == 0){
    stop("Error: length of shared sample names is 0\n")
  }
  sampleNames
}
