#' Apply regression model using multiple datasets as input
#'
#' \code{ModelMultiData} loops through one or more datasets and returns
#' coefficients of regression model.
#'
#' This function applies a regression model across all rows of one or more
#' matrices with inputs from multiple datasets.
#'
#' For all matrices in x and y, the function will loop by row. Rownames
#' represent individual events or observations and should match names provided
#' in the comparisons matrix, if user-defined. Columns represent individual
#' samples. Columns of x and y will be dropped if their column names are not
#' shared across all matrices in x and y and in the first column of groups, if
#' defined.
#'
#' Users may provide multiple datasets to be used in the same model by providing
#' a list of matrices to x. For example, a user may wish to test for
#' associations between an outcome of interest and all measurements in x[[1]]
#' while correcting for measurements in x[[2]].
#'
#' If formula is user-defined, the formula should include all values in yName
#' and xName (e.g. names of provided matrices). Else, formula will be built from
#' provided data. By default, the left-hand (dependent) variable will be "y" if
#' y is provided, else if yName is defined the left-hand variable will be
#' defined as the column of groups that matches yName, else the left-hand
#' variable will be defined as the second column of groups after applying
#' includeVars and excludeVars. The right-hand of formula will include all in
#' xName and all remaining columns in groups after applying includeVars and
#' excludeVars.
#'
#' The comparisons parameter allows users to pre-define which combinations of
#' rows in x and y should be tested.  Column names should match values in yName
#' and xName. Rows contain respective rownames or row numbers from each dataset
#' in x and y that should be tested together. If not provided, comparisons
#' matrix will be constructed. By default, if all matrices in x have the same
#' rownames, ows with the same name will be grouped. Else, comparisons includes
#' all possible combinations of rownames in x and y.
#'
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
#' @param formula Formula to be used in model. If provided by user, formula
#'   should include all values in yName and xName. Else, formula will be built
#'   using provided data (see details for more information).
#' @param FUN Function to be used for regression analysis. Defaults to lm().
#' @param pAdjust Adjust p-values using one of several methods contained in
#'   p.adjust.methods. Skipped if pAjust = NULL.
#' @param xName Names of matrices provided in x. If x is a list with named
#'   elements, defaults to list names. Else, defaults to x1, x2, ..., xn where n
#'   is the number of matrices provided in x.
#' @param yName Name of matrix or name of column in groups to be used on
#'   left-hand of model. (see formula details for default values)
#' @param returnVars Character vector of coefficient variable names to return.
#'   By default, returns all coefficients for variables in xName. To return all
#'   coefficients (including intercept), use "*".
#' @param comparisons Matrix containing rownames or numbers from each dataset
#'   for all desired comparisons. If not provided, comparisons matrix will be
#'   build (see details for defaults).
#' @param excludeVars Character vector of column names in groups to exclude from
#'   formula. Ignored if formula provided or if groups not provided.
#' @param includeVars Character vector of column names in groups to include in
#'   formula. Ignored if formula provided or if groups not provided.
#' @param ... Additional parameters to be passed to FUN.
#' @return A data.table with model coefficients for all variables in returnVars
#'   for all unique tests in comparisons. Includes comparison, variable name,
#'   Estimate, Std. Error, t- or z-value, pValue, and adjusted pValue.
#' @example ./inst/extdata/ModelMultiDataRunExample.R
#' @export

ModelMultiData <- function(x, y = NULL, groups = NULL, by = NULL,
                           formula = "auto", FUN = lm, pAdjust = "fdr",
                           xName = "auto", yName = "auto",
                           returnVars = "auto", comparisons = NULL,
                           excludeVars = NULL, includeVars = NULL, ...){
  if (missing(x)){
    stop(paste0("argument \"x\" is missing, with no default"))
  } else {
    x <- CheckXClass(x)
  }
  if (xName == "auto"){
    if (is.null(names(x))){
      xName <- paste0("x", 1:length(x))
      names(x) <- xName
    } else {
      xName <- names(x)
    }
  } else {
    if (length(xName) != length(x)){
      stop("xName does not have the same length as x")
    } else {
      names(x) <- xName
    }
  }
  if (is.null(y) & is.null(groups)){
    stop("At least one of \"groups\" or \"y\" must be defined")
  }
  if (!is.null(groups)){
    groups <- CheckGroups(groups)
  }
  if (!is.null(y)){
    y <- CheckYClass(y)
  }
  sampleNames <- GetSharedSamples(x = x, y = y, groups = groups)
  for (i in 1:length(x)){
    if (length(sampleNames) != ncol(x[[i]])){
      warning(
        paste0("Names not identical. Columns dropped from list element ",
               i, " of x: ", setdiff(sampleNames, colnames(x[[i]]))))
    }
    x[[i]] <- x[[i]][, sampleNames]
  }
  if (!is.null(y)){
    if (length(sampleNames) != ncol(y)){
      warning(paste0("Names not identical. Columns dropped from y: ",
                     setdiff(sampleNames, colnames(y))))
    }
    y <- y[, sampleNames]
  }
  if (!is.null(groups)){
    groups <- groups[samples %in% sampleNames][match(samples, sampleNames)]
  }
  if(formula == "auto"){
    formula <- BuildFormula(x = x, y = y, groups = groups, by = by,
                            xName = xName, yName = yName,
                            excludeVars = excludeVars,
                            includeVars = includeVars)
  } else {
    formula <- CheckFormula(formula = formula, y = y, groups = groups,
                            xName = xName, yName = yName)
  }

  formula.vars <- all.vars(formula)
  yName <- formula.vars[1]
  if (returnVars == "auto"){
    returnVars <- xName
  } else {
    CheckReturnVars(returnVars, formula)
  }
  if (is.null(comparisons)){
    comparisons <- BuildComparisons(x = x, y = y, xName = xName, yName = yName)
  } else {
    if (class(comparisons) != "matrix"){
      warning("Coercing comparisons to matrix: some data may be lost")
      comparisons <- as.matrix(comparisons)
    }
    if (ncol(comparisons) != (length(x) + ifelse(is.null(y), 0, 1))){
      stop(paste0("Number of columns in comparisons does not match ",
                  "length(x) + ifelse(is.null(y), 0, 1))"))
    }
  }
  if (!all(colnames(comparisons) %in% formula.vars)){
    stop(paste0(
      "The following variables from comparisons are missing in formula: ",
      colnames(comparisons)[!(colnames(comparisons) %in% formula.vars)]))
  }
  FUN <- match.fun(FUN)
  if (is.null(groups) & !is.null(by)){
    warning(paste0("When groups object not provided, by is ignored."))
    by <- NULL
  } else if (!is.null(groups) & !is.null(by)){
    if (!(all(by %in% colnames(groups)))){
      stop(paste0(by[!(by %in% colnames(groups))],
                  " not found in colnames(groups)"))
    }
  }
  if (!is.null(by)){
    groups_ls <- split(groups, by = by)
  } else {
    groups_ls <- list(groups)
  }
  result <- lapply(groups_ls, function(dt) data.table(NULL))
  formulas <- lapply(groups_ls, function(dt) CheckFormulaLevels(formula, dt))
  for (i in 1:nrow(comparisons)){
    comparison <- comparisons[i, ]
    modelDat <- BuildModelData(comparison = comparison, x = x,
                               y = y, groups = groups, by = by,
                               xName = xName, yName = yName)
    for (g in 1:length(groups_ls)){
      tryCatch({
        coef_dt <- RunModel(
          formula = formulas[[g]], modelData = modelDat[[g]],
          comparison = comparison, returnVars = returnVars, FUN = FUN, ...)
        result[[g]] <- rbind(result[[g]], coef_dt)
      }, error = function(e) {})
    }
  }
  if (!is.null(pAdjust)){
    result <- lapply(result, function(r){
      r <- r[, pAdjust := p.adjust(pValue, method = pAdjust)]
      colnames(r)[grep("pAdjust", colnames(r))]<- pAdjust
      r
    })
  }
  return(result)
}
