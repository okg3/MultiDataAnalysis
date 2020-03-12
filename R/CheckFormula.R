#' Check formula with provided data
#'
#' \code{CheckFormula} is an internal function that checks formula if formula is
#' defined in ModelMultiData.
#'
#' Checks that user defined formula include all values in yName and xName (e.g.
#' names of provided matrices).
#'
#' @param formula Formula to be used in model. If provided by user, formula
#'   should include all values in yName and xName. Else, formula will be built
#'   using provided data (see details for more information).
#' @param y A matrix containing values to be used on left-hand side of model
#'   (e.g. dependent variable). At least one of y or groups must be provided.
#' @param groups A data.frame containing sample group data. At least one of y or
#'   groups must be provided. Column one of groups data.frame must contain
#'   sample names that correspond to column names of x and y.
#' @param xName Names of matrices provided in x. If x is a list with named
#'   elements, defaults to list names. Else, defaults to x1, x2, ..., xn where n
#'   is the number of matrices provided in x.
#' @param yName Name of matrix or name of column in groups to be used on
#'   left-hand of model. (see formula details for default values)
#' @return formula

CheckFormula <- function(formula, y = NULL, groups = NULL,
                         xName = "auto", yName = "auto"){
  formula <- as.formula(formula)
  formula.vars <- all.vars(formula)

  for (i in 1:length(xName)){
    if (!(xName[i] %in% formula.vars)) {
      warning(paste0("Warning: variable", xName[i],
                     "missing from formula.\n"))
    }
  }

  if (yName == "auto"){
    yName <- formula.vars[1]
  } else {
    if (yName != formula.vars[1]){
      stop(paste0("Error: yName, ", yName,
                  ", does not match formula\n"))
    }
  }
  if (is.null(y) & !(yName %in% colnames(groups))){
    stop(paste0("Error: ", yName, " not found in colnames(groups)\n"))
  }
  modelCov <- formula.vars[!(formula.vars %in% c(xName, yName))]
  varsAvail <- NULL
  if (!is.null(groups)){
    varsAvail <- c(varsAvail, colnames(groups))
  }
  if (length(modelCov[!(modelCov %in% varsAvail)]) > 0){
    stop(
      paste0("Error: Unable to find the following Vars: ",
             paste0(c(modelCov[!(modelCov %in% varsAvail)]), collapse = ", "),
             "\n"))
  }
  return(as.formula(formula))
}
