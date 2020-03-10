#' Build data.table for use in model 
#'
#' \code{BuildModelData} is an internal function that constructs a data.table
#' from groups, x, and y to be used in regression model.
#' 
#' This function binds together desired rows in x and y with groups object to
#' create model data.
#'
#' @param comparisons Matrix containing rownames or numbers from each dataset
#'   for all desired comparisons. If not provided, comparisons matrix will be
#'   build (see details for defaults).
#' @param x A matrix or list of matrices containing values for model.
#' @param y A matrix containing values to be used on left-hand side of model
#'   (e.g. dependent variable). At least one of y or groups must be provided.
#' @param groups A data.frame containing sample group data. At least one of y or
#'   groups must be provided. Column one of groups data.frame must contain
#'   sample names that correspond to column names of x and y.
#' @param by Character vector specifying column names in groups that will be
#'   used to split data if stratified analysis desired. Ignored if groups not
#'   provided.
#' @param xName Names of matrices provided in x. If x is a list with named
#'   elements, defaults to list names. Else, defaults to x1, x2, ..., xn where n
#'   is the number of matrices provided in x.
#' @param yName Name of matrix or name of column in groups to be used on
#'   left-hand of model. (see formula details for default values)
#' @return A data.table with all data to be used in model. 

BuildModelData <- function(comparison, x, y = NULL, groups = NULL,
                           by = NULL, xName, yName){
  modelDat <- data.table(groups)
  for (j in 1:length(x)){
    modelDat <- data.table(modelDat, x[[j]][comparison[xName[j]], ])
    colnames(modelDat)[ncol(modelDat)] <- xName[j]
  }
  
  if (!is.null(y)){
    modelDat <- data.table(modelDat, y[comparison[yName], ])
    colnames(modelDat)[ncol(modelDat)] <- yName
  }
  col_classes <- vapply(modelDat[, -1], class, FUN.VALUE = character(1))
  for (col in names(col_classes[col_classes == "character"])) {
    set(modelDat, j=col, value=as.factor(modelDat[[col]]))
  }
  
  if (!is.null(by)){
    modelDat <- split(modelDat, by = by)
  } else {
    modelDat <- list(modelDat)
  }
  return(modelDat)
}
