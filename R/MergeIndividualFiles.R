#' Merge Data from Multiple Files
#'
#' \code{MergeIndividualFiles} merges data from individual sample data and
#' returns list of matrices.
#'
#' This function reads in files containing data from individual samples and
#' merges individual data into one dataset by variable or measurement.
#'
#' @param fileDirectory a character vector of full path names to directory
#'   containing individual files.
#' @param filePattern an optional regular expression. Only file names which
#'   match the regular expression will be returned. Strings matching regular
#'   expression will be removed from column names in returned dataset.
#' @param filePrefix an optional regular expression. Strings matching regular
#'   expression will be removed from column names in returned dataset.
#' @param indVars character vector of column names in individual files that
#'   correspond with individual measures (e.g. not annotation). Default value is
#'   all columns except those in IDcol.
#' @param IDcol character or numeric vector of columns in individual files on
#'   which to merge
#' @param ... additional parameters to be passed to data.table::fread()
#' @return A list of merged data. The first list element contains annotations
#'   (e.g. columns that are not included in indVars). All other list elements
#'   are matrices with rownames matching values in IDcol and columns matching
#'   file names.
#' @examples
#' \dontrun{
#' out <- MergeIndividualFiles(
#' fileDirectory = paste0(path.package("MultiDataAnalysis"), "/extdata/"),
#' filePattern = ".txt.gz", indVars = c("AllSubs", "Coverage-q25", "MeanQ",
#' "BaseCount[A,C,G,T]", "Frequency", "Pvalue"),
#' IDcol = c("Region", "Position", "Reference", "Strand"))
#' }
#' @export

MergeIndividualFiles <- function(fileDirectory, filePattern = NULL,
                                 filePrefix = NULL, indVars = "auto",
                                 IDcol = 1, ...){
  if (missing(fileDirectory)){
    stop("fileDirectory undefined with no default")
  }
  fileList <- list.files(path = fileDirectory, pattern = filePattern)
  file <- fileList[1]
  temp_dataset <- fread(paste0(fileDirectory, file), ...)
  stopifnot(length(IDcol) >= 1)
  if (is.character(IDcol)){
    IDcol <- match(IDcol, colnames(temp_dataset))
  }
  if (length(IDcol) > 1){
    id_mat <- as.matrix(
      temp_dataset[, lapply(.SD, as.character), .SDcols = IDcol])
    id <- apply(id_mat, 1, paste0, collapse = "_")
    rm(id_mat)
  } else {
    id <- temp_dataset[[IDcol]]
  }
  temp_dataset <- data.table(id = id, temp_dataset)
  IDcolShift <- c(1, IDcol + 1)
  if (length(indVars) > 1 || indVars != "auto"){
    stopifnot(length(indVars) >= 1)
    if (is.numeric(indVars)){
      indCols <- indVars + 1
      indVars <- colnames(temp_dataset)[indCols]
    } else if (is.character(indVars)){
      indCols <- match(indVars, colnames(temp_dataset))
    } else {
      stop("indVars must be of class numeric or character")
    }
  } else {
    indVars = colnames(temp_dataset)[-IDcolShift]
    indCols <- match(indVars, colnames(temp_dataset))
  }
  dataset <- vector("list", length(indVars) + 1)
  names(dataset) <- c("Annotation", indVars)
  dataset[["Annotation"]] <- temp_dataset[, -(..indCols)]
  datName <- file
  if (!is.null(filePattern)){
    datName <- gsub(filePattern, "", datName)
  }
  if (!is.null(filePrefix)){
    datName <- gsub(filePrefix, "", datName)
  }
  for (Var in 1:length(indVars)) {
    varCols <- c(1, indCols[Var])
    tempVarData <- temp_dataset[, ..varCols]
    colnames(tempVarData)[2] <- datName
    dataset[[indVars[Var]]] <- tempVarData
  }
  for (file in fileList[-1]){
    temp_dataset <- fread(paste0(fileDirectory, file), ...)
    if (is.character(IDcol)){
      IDcol <- match(IDcol, colnames(temp_dataset))
    }
    if (length(IDcol) > 1){
      id_mat <- as.matrix(
        temp_dataset[, lapply(.SD, as.character), .SDcols = IDcol])
      id <- apply(id_mat, 1, paste0, collapse = "_")
      rm(id_mat)
    } else {
      id <- temp_dataset[[IDcol]]
    }
    temp_dataset <- data.table(id = id, temp_dataset)
    temp_annotation <- temp_dataset[, -(..indCols)]
    dataset[["Annotation"]] <- unique(rbindlist(list(
      dataset[["Annotation"]], temp_annotation)))
    datName <- file
    if (!is.null(filePattern)){
      datName <- gsub(filePattern, "", datName)
    }
    if (!is.null(filePrefix)){
      datName <- gsub(filePrefix, "", datName)
    }
    for (Var in 1:length(indVars)) {
      varCols <- c(1, indCols[Var])
      tempVarData <- temp_dataset[, ..varCols]
      colnames(tempVarData)[2] <- datName
      dataset[[indVars[Var]]] <- merge(
        dataset[[indVars[Var]]], tempVarData,
        by = "id", all = TRUE, allow.cartesian = TRUE)
    }
  }
  dataset <- lapply(dataset, function(x){x[order(id)]})
  for (Var in 1:length(indVars)) {
    id <- dataset[[indVars[Var]]][[1]]
    varMat <- as.matrix(dataset[[indVars[Var]]][, -1])
    rownames(varMat) <- id
    dataset[[indVars[Var]]] <- varMat
  }
  return(dataset)
}
