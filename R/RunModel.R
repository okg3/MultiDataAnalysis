#' Run regression model
#'
#' \code{RunModel} is an internal function that applies a regression model to
#' constructed model data and regurns model coefficients.
#'
#' @param formula Formula to be used in model.
#' @param modelData data.table with all data to be used in model. Constructed
#'   with BuildModelData function.
#' @param FUN Function to be used for regression analysis. Defaults to lm().
#' @param returnVars Character vector of coefficient variable names to return.
#'   By default, returns all coefficients for variables in xName. To return all
#'   coefficients (including intercept), use "*".
#' @param comparison Vector containing rownames or numbers of x and y included
#'   in test.
#' @param ... Additional parameters to be passed to FUN.
#' @return A data.table with model coefficients for all variables in returnVars.
#'   Includes comparison, variable name, Estimate, Std. Error, t- or z-value,
#'   and pValue.

RunModel <- function(formula, modelData, comparison = NULL,
                     returnVars = "*", FUN = lm, ...){
  coef_num <- coefficients(
    summary(FUN(formula, data = modelData, ...)))
  if (class(coef_num) == "list"){
    rnCoef <- NULL
    for (j in 1:length(coef_num)){
      rnCoef <- c(
        rnCoef, paste0(names(coef_num)[j], ".", rownames(coef_num[[j]])))
    }
    coef_num <- Reduce(rbind, coef_num)
    rownames(coef_num) <- rnCoef
  }
  coef_mat <- NULL
  for (v in returnVars){
    coef_mat <- unique(
      rbind(coef_mat, coef_num[grep(v, rownames(coef_num)), ]))
  }
  if (class(coef_mat) == "numeric"){
    coef_mat <- t(as.matrix(coef_mat))
  }
  if (ncol(coef_mat) == 3){
    n <- nrow(modelData)
    coef_mat <- cbind(coef_mat,
                      pValue = 2*pt(-abs(coef_mat[, 3]), df=n-1))
  } else {
    colnames(coef_mat)[4] <- "pValue"
  }
  if (is.null(rownames(coef_mat))){
    variables <- returnVars
  } else {
    variables <- rownames(coef_mat)
  }
  if (!is.null(comparison)){
    data.table(t(comparison), variable = variables, coef_mat)
  } else {
    data.table(variable = variables, coef_mat)
  }
}
