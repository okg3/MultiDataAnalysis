#' Check that all variables in returnVars are included in formula
#'
#' \code{returnVars} is an internal function that checks that all variables in
#' returnVars are included in formula in ModelMultiData.
#'
#'
#' @param returnVars Character vector of coefficient variable names to return.
#'   By default, returns all coefficients for variables in xName. To return all
#'   coefficients (including intercept), use "*".
#' @param formula Formula to be used in model. If provided by user, formula
#'   should include all values in yName and xName. Else, formula will be built
#'   using provided data (see details for more information).

CheckReturnVars <- function(returnVars, formula){
  formula.vars <- all.vars(formula)
  for (v in returnVars){
    if (!any(grepl(v, formula.vars[-1]))){
      stop(paste0("Pattern ", v, "did not match any variable in formula"))
    }
  }
}
