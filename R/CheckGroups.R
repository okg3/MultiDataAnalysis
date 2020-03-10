#' Check Class of data provided to x parameter in ModelMultiData
#'
#' \code{CheckGroups} is an internal function that checks the class of groups
#' parameter in ModelMultiData function.
#'
#' @param groups data.frame or data.table with groups data 
#' @return data.table with groups data


CheckGroups <- function(groups){
  if (!("data.table" %in% class(groups))) {
    groups <- as.data.table(groups)
  }
  colnames(groups)[1] <- "samples"
  groups <- groups[, samples := as.character(samples)]
  col_classes <- vapply(groups[, -1], class, FUN.VALUE = character(1))
  for (col in names(col_classes[col_classes == "character"])) {
    set(groups, j=col, value=as.factor(groups[[col]]))
  }
  groups
}
