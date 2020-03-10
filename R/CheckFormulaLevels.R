#' Check formula for singular values 
#'
#' \code{CheckFormulaLevels} is an internal function that tests that all
#' variables in formula have more than one value.
#' 
#' This function checks all variables in formula for singular values, which
#' usually cause errors when modeling, and drops singular variables.
#'
#' @param formula Formula to be used in model. 
#' @param dt data.table with model data. 
#' @return formula with singular values dropped. 

CheckFormulaLevels <- function(formula, dt){
  formula.vars <- all.vars(formula)
  varCheck <- formula.vars[formula.vars %in% colnames(dt)]
  dropVars <- NULL
  for (v in varCheck){
    if (length(unique(dt[[v]])) <= 1){
      dropVars <- c(dropVars, v)
    }
  }
  if (!is.null(dropVars)){
    warning(paste0("The following variables have singular value: ",
                   dropVars))
    formula <- as.formula(
      paste(formula.vars[1], "~",
            paste(setdiff(formula.vars[-1], dropVars), collapse = "+")))
  }
  formula
}
