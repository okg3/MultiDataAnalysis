#' RNA Editing Data Object post Quality Control
#'
#' A list with 1 Annotation data.frame and 7 matrices containing RNA editing
#' data.
#'
#' @format 1 list with 1 data.frame and 7 matrices.
#' \describe{
#'   \item{RNAEdData[["Annotation"]]}{A data.frame with 1000 observations and
#'   17 variables}
#'   \item{RNAEdData[["AllSubs"]]}{A matrix with 1000 observations and 8
#'   variables}
#'   \item{RNAEdData[["Coverage-q25"]]}{A matrix with 1000 observations and 8
#'   variables}
#'   \item{RNAEdData[["MeanQ"]]}{A matrix with 1000 observations and 8
#'   variables}
#'   \item{RNAEdData[["BaseCount[A,C,G,T]"]]}{A matrix with 1000 observations
#'   and 8 variables}
#'   \item{RNAEdData[["Frequency"]]}{A matrix with 1000 observations and 8
#'   variables}
#'   \item{RNAEdData[["Pvalue"]]}{A matrix with 1000 observations and 8
#'   variables}
#'   \item{RNAEdData[["EditedReads"]]}{A matrix with 1000 observations and 8
#'   variables}
#' }
"RNAEdDataQCed"
