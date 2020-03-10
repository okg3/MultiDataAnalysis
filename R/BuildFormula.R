#' Build formula with provided data
#'
#' \code{BuildFormula} is an internal function that builds a formula from data
#' provided by user to ModelMultiData.
#'
#' Builds model formula from provided data. By default, the left-hand
#' (dependent) variable will be "y" if y is provided, else if yName is defined
#' the left-hand variable will be defined as the column of groups that matches
#' yName, else the left-hand variable will be defined as the second column of
#' groups after applying includeVars and excludeVars. The right-hand of formula
#' will include all in xName and all remaining columns in groups after applying
#' includeVars and excludeVars.
#' 
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
#'   left-hand of model. (see details for default values)
#' @param excludeVars Character vector of column names in groups to exclude from
#'   formula. Ignored if formula provided or if groups not provided.
#' @param includeVars Character vector of column names in groups to include in
#'   formula. Ignored if formula provided or if groups not provided.
#' @return formula

BuildFormula <- function(x, y = NULL, groups = NULL, by = NULL,
                         xName = "auto", yName = "auto",
                         excludeVars = NULL, includeVars = NULL){
  if (is.null(groups) & (!is.null(excludeVars) |
                         !is.null(includeVars))){
    warning(paste0("Arguments excludeVars and includeVars",
                   " if groups is null."))
  }
  if (length(yName) > 1){
    yName <- yName[1]
    warning(paste0(length(yName), " arguments passed to yName",
                   "which requires 1. Only the first will be used."))
  }
  if (!is.null(groups)) {
    include <- colnames(groups)[-1]
    if (!is.null(excludeVars)){
      include <- setdiff(include, excludeVars)
    }
    if (!is.null(includeVars)){
      include <- include[which(include %in% includeVars)]
    }
    include <- setdiff(include, by)
    if (is.null(y)){
      if (yName != "auto"){
        if (!(yName %in% colnames(groups))){
          stop(paste0(yName, " not found in colnames(groups)"))
        }
        include <- setdiff(include, yName)
      } else {
        yName <- include[1]
        include <- include[-1]
        warning(paste0("y missing. Default value is second column of  ",
                       "groups, post inclusion/exclusion criteria: ",
                       yName))
      }
    }
  } else {
    include <- NULL
  }
  if (!is.null(y) & yName == "auto"){yName <- "y"}
  formula.vars <- unique(c(yName, include, xName))
  formula <- as.formula(
    paste(formula.vars[1], "~", paste(formula.vars[-1], collapse = "+")))
  warning(paste0("argument \"formula\" undefined. ",
                 "Using formula constructed from provided data: ",
                 deparse(formula)))
  return(as.formula(formula))
}
