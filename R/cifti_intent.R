#' @include cifti_io.R
NULL

# ============================================================================
# Intent-Specific Constructors
# ============================================================================

#' Create a Dense Timeseries CIFTI Image
#'
#' @param data Numeric matrix (time x grayordinates).
#' @param brain_model A \code{\linkS4class{BrainModelAxis}} for columns.
#' @param start Numeric start time (default 0).
#' @param step Numeric TR / time step.
#' @param unit Character unit (default "SECOND").
#' @return A \code{\linkS4class{CiftiImage}} with intent 3002.
#'
#' @export
dtseries <- function(data, brain_model, start = 0, step = 1, unit = "SECOND") {
  stopifnot(is.matrix(data))
  stopifnot(is(brain_model, "BrainModelAxis"))

  series <- new("SeriesAxis",
    start = as.numeric(start),
    step = as.numeric(step),
    size = as.integer(nrow(data)),
    unit = unit
  )

  CiftiImage(
    data = data,
    row_axis = series,
    col_axis = brain_model,
    intent = CIFTI_INTENTS[["ConnDenseTimeSeries"]]
  )
}


#' Create a Dense Scalar CIFTI Image
#'
#' @param data Numeric matrix (maps x grayordinates).
#' @param brain_model A \code{\linkS4class{BrainModelAxis}} for columns.
#' @param names Character vector of map names.
#' @return A \code{\linkS4class{CiftiImage}} with intent 3006.
#'
#' @export
dscalar <- function(data, brain_model, names = NULL) {
  stopifnot(is.matrix(data))
  stopifnot(is(brain_model, "BrainModelAxis"))

  if (is.null(names)) {
    names <- paste0("map_", seq_len(nrow(data)))
  }

  scalar <- new("ScalarAxis",
    name = names,
    meta = rep(list(list()), length(names))
  )

  CiftiImage(
    data = data,
    row_axis = scalar,
    col_axis = brain_model,
    intent = CIFTI_INTENTS[["ConnDenseScalar"]]
  )
}


#' Create a Dense Label CIFTI Image
#'
#' @param data Integer matrix (maps x grayordinates) of label keys.
#' @param brain_model A \code{\linkS4class{BrainModelAxis}} for columns.
#' @param names Character vector of map names.
#' @param label_tables List of \code{\linkS4class{CiftiLabelTable}} objects.
#' @return A \code{\linkS4class{CiftiImage}} with intent 3007.
#'
#' @export
dlabel <- function(data, brain_model, names = NULL, label_tables = NULL) {
  stopifnot(is.matrix(data))
  stopifnot(is(brain_model, "BrainModelAxis"))

  nmaps <- nrow(data)

  if (is.null(names)) {
    names <- paste0("label_", seq_len(nmaps))
  }
  if (is.null(label_tables)) {
    label_tables <- rep(list(new("CiftiLabelTable")), nmaps)
  }

  label_axis <- new("LabelAxis",
    name = names,
    label = label_tables,
    meta = rep(list(list()), nmaps)
  )

  CiftiImage(
    data = data,
    row_axis = label_axis,
    col_axis = brain_model,
    intent = CIFTI_INTENTS[["ConnDenseLabel"]]
  )
}
