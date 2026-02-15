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


# ============================================================================
# Parcel-Based Intent Constructors
# ============================================================================

#' Create a Parcel Timeseries CIFTI Image
#'
#' @param data Numeric matrix (time x parcels).
#' @param parcels A \code{\linkS4class{ParcelsAxis}} for columns.
#' @param start Numeric start time (default 0).
#' @param step Numeric TR / time step.
#' @param unit Character unit (default "SECOND").
#' @return A \code{\linkS4class{CiftiImage}} with intent 3004.
#'
#' @export
ptseries <- function(data, parcels, start = 0, step = 1, unit = "SECOND") {
  stopifnot(is.matrix(data))
  stopifnot(is(parcels, "ParcelsAxis"))

  series <- new("SeriesAxis",
    start = as.numeric(start),
    step = as.numeric(step),
    size = as.integer(nrow(data)),
    unit = unit
  )

  CiftiImage(
    data = data,
    row_axis = series,
    col_axis = parcels,
    intent = CIFTI_INTENTS[["ConnParcelTimeSeries"]]
  )
}


#' Create a Parcel Scalar CIFTI Image
#'
#' @param data Numeric matrix (maps x parcels).
#' @param parcels A \code{\linkS4class{ParcelsAxis}} for columns.
#' @param names Character vector of map names.
#' @return A \code{\linkS4class{CiftiImage}} with intent 3008.
#'
#' @export
pscalar <- function(data, parcels, names = NULL) {
  stopifnot(is.matrix(data))
  stopifnot(is(parcels, "ParcelsAxis"))

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
    col_axis = parcels,
    intent = CIFTI_INTENTS[["ConnParcelScalar"]]
  )
}


#' Create a Parcel Label CIFTI Image
#'
#' @param data Integer matrix (maps x parcels) of label keys.
#' @param parcels A \code{\linkS4class{ParcelsAxis}} for columns.
#' @param names Character vector of map names.
#' @param label_tables List of \code{\linkS4class{CiftiLabelTable}} objects.
#' @return A \code{\linkS4class{CiftiImage}} with intent 3012.
#'
#' @export
plabel <- function(data, parcels, names = NULL, label_tables = NULL) {
  stopifnot(is.matrix(data))
  stopifnot(is(parcels, "ParcelsAxis"))

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
    col_axis = parcels,
    intent = CIFTI_INTENTS[["ConnParcelLabel"]]
  )
}


# ============================================================================
# Connectivity Intent Constructors
# ============================================================================

#' Create a Dense Connectivity CIFTI Image
#'
#' Connectivity matrix between grayordinates (BrainModelAxis x BrainModelAxis).
#'
#' @param data Numeric matrix (grayordinates x grayordinates).
#' @param row_brain_model A \code{\linkS4class{BrainModelAxis}} for rows.
#' @param col_brain_model A \code{\linkS4class{BrainModelAxis}} for columns
#'   (defaults to \code{row_brain_model}).
#' @return A \code{\linkS4class{CiftiImage}} with intent 3001.
#'
#' @export
dconn <- function(data, row_brain_model, col_brain_model = row_brain_model) {
  stopifnot(is.matrix(data))
  stopifnot(is(row_brain_model, "BrainModelAxis"))
  stopifnot(is(col_brain_model, "BrainModelAxis"))

  CiftiImage(
    data = data,
    row_axis = row_brain_model,
    col_axis = col_brain_model,
    intent = CIFTI_INTENTS[["ConnDense"]]
  )
}


#' Create a Parcel Connectivity CIFTI Image
#'
#' Connectivity matrix between parcels (ParcelsAxis x ParcelsAxis).
#'
#' @param data Numeric matrix (parcels x parcels).
#' @param row_parcels A \code{\linkS4class{ParcelsAxis}} for rows.
#' @param col_parcels A \code{\linkS4class{ParcelsAxis}} for columns
#'   (defaults to \code{row_parcels}).
#' @return A \code{\linkS4class{CiftiImage}} with intent 3003.
#'
#' @export
pconn <- function(data, row_parcels, col_parcels = row_parcels) {
  stopifnot(is.matrix(data))
  stopifnot(is(row_parcels, "ParcelsAxis"))
  stopifnot(is(col_parcels, "ParcelsAxis"))

  CiftiImage(
    data = data,
    row_axis = row_parcels,
    col_axis = col_parcels,
    intent = CIFTI_INTENTS[["ConnParcels"]]
  )
}


#' Create a Dense-to-Parcel Connectivity CIFTI Image
#'
#' Connectivity from grayordinates (rows) to parcels (columns).
#'
#' @param data Numeric matrix (grayordinates x parcels).
#' @param brain_model A \code{\linkS4class{BrainModelAxis}} for rows.
#' @param parcels A \code{\linkS4class{ParcelsAxis}} for columns.
#' @return A \code{\linkS4class{CiftiImage}} with intent 3010.
#'
#' @export
dpconn <- function(data, brain_model, parcels) {
  stopifnot(is.matrix(data))
  stopifnot(is(brain_model, "BrainModelAxis"))
  stopifnot(is(parcels, "ParcelsAxis"))

  CiftiImage(
    data = data,
    row_axis = brain_model,
    col_axis = parcels,
    intent = CIFTI_INTENTS[["ConnDenseParcel"]]
  )
}


#' Create a Parcel-to-Dense Connectivity CIFTI Image
#'
#' Connectivity from parcels (rows) to grayordinates (columns).
#'
#' @param data Numeric matrix (parcels x grayordinates).
#' @param parcels A \code{\linkS4class{ParcelsAxis}} for rows.
#' @param brain_model A \code{\linkS4class{BrainModelAxis}} for columns.
#' @return A \code{\linkS4class{CiftiImage}} with intent 3011.
#'
#' @export
pdconn <- function(data, parcels, brain_model) {
  stopifnot(is.matrix(data))
  stopifnot(is(parcels, "ParcelsAxis"))
  stopifnot(is(brain_model, "BrainModelAxis"))

  CiftiImage(
    data = data,
    row_axis = parcels,
    col_axis = brain_model,
    intent = CIFTI_INTENTS[["ConnParcelDense"]]
  )
}


#' Create a Parcel Series CIFTI Image
#'
#' Parcel data with a series axis on rows (e.g., parcellated time series
#' with parcels on both axes is ConnParcelSeries, intent 3009).
#'
#' @param data Numeric matrix (series x parcels).
#' @param parcels A \code{\linkS4class{ParcelsAxis}} for columns.
#' @param start Numeric start value (default 0).
#' @param step Numeric step value.
#' @param unit Character unit (default "SECOND").
#' @return A \code{\linkS4class{CiftiImage}} with intent 3009.
#'
#' @export
pcseries <- function(data, parcels, start = 0, step = 1, unit = "SECOND") {
  stopifnot(is.matrix(data))
  stopifnot(is(parcels, "ParcelsAxis"))

  series <- new("SeriesAxis",
    start = as.numeric(start),
    step = as.numeric(step),
    size = as.integer(nrow(data)),
    unit = unit
  )

  CiftiImage(
    data = data,
    row_axis = series,
    col_axis = parcels,
    intent = CIFTI_INTENTS[["ConnParcelSeries"]]
  )
}
