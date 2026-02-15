#' @include cifti_axis_methods.R
NULL

# ============================================================================
# CiftiImage Constructor
# ============================================================================

#' Create a CiftiImage
#'
#' @param data A numeric matrix (rows x cols), or NULL.
#' @param header A \code{\linkS4class{CiftiHeader}} object (optional if axes given).
#' @param row_axis A \code{\linkS4class{CiftiAxis}} for rows.
#' @param col_axis A \code{\linkS4class{CiftiAxis}} for columns.
#' @param intent Integer NIfTI intent code.
#' @param file_name Character source file path.
#' @return A \code{\linkS4class{CiftiImage}} object.
#'
#' @export
CiftiImage <- function(data = NULL, header = NULL, row_axis = NULL,
                        col_axis = NULL, intent = 0L, file_name = "") {
  # If header provided but no axes, derive axes from header
  if (!is.null(header) && (is.null(row_axis) || is.null(col_axis))) {
    axes <- header_to_axes(header)
    if (is.null(row_axis)) row_axis <- axes$row_axis
    if (is.null(col_axis)) col_axis <- axes$col_axis
  }

  # If axes provided but no header, build header from axes
  if (is.null(header) && !is.null(row_axis) && !is.null(col_axis)) {
    header <- axes_to_header(row_axis, col_axis)
  }

  if (is.null(header)) {
    header <- new("CiftiHeader")
  }
  if (is.null(row_axis)) row_axis <- new("SeriesAxis")
  if (is.null(col_axis)) col_axis <- new("SeriesAxis")

  new("CiftiImage",
    data = data,
    header = header,
    row_axis = row_axis,
    col_axis = col_axis,
    intent = as.integer(intent),
    file_name = as.character(file_name)
  )
}


# ============================================================================
# show()
# ============================================================================

#' @rdname CiftiImage-class
#' @param object A \code{CiftiImage} object.
#' @param x A \code{CiftiImage} object.
#' @param i Row indices.
#' @param j Column indices.
#' @param ... Additional arguments (ignored).
#' @param drop Logical (ignored).
#' @export
setMethod("show", "CiftiImage", function(object) {
  intent_name <- names(CIFTI_INTENTS)[match(object@intent, CIFTI_INTENTS)]
  if (is.na(intent_name)) intent_name <- "Unknown"

  cat("CiftiImage\n")
  cat(sprintf("  Intent: %s (%d)\n", intent_name, object@intent))

  if (!is.null(object@data)) {
    cat(sprintf("  Data:   %d x %d matrix\n", nrow(object@data), ncol(object@data)))
  } else {
    cat("  Data:   (not loaded)\n")
  }

  if (nchar(object@file_name) > 0) {
    cat(sprintf("  File:   %s\n", basename(object@file_name)))
  }

  cat("  Row axis:  ")
  show(object@row_axis)
  cat("  Col axis:  ")
  show(object@col_axis)

  invisible(object)
})


# ============================================================================
# Accessors
# ============================================================================

#' @rdname CiftiImage-class
#' @export
setMethod("row_axis", "CiftiImage", function(x, ...) x@row_axis)

#' @rdname CiftiImage-class
#' @export
setMethod("col_axis", "CiftiImage", function(x, ...) x@col_axis)

#' @rdname CiftiImage-class
#' @export
setMethod("cifti_header", "CiftiImage", function(x, ...) x@header)

#' @rdname CiftiImage-class
#' @export
setMethod("cifti_intent", "CiftiImage", function(x, ...) x@intent)

#' @rdname CiftiImage-class
#' @export
setMethod("cifti_data", "CiftiImage", function(x, ...) x@data)

#' @rdname CiftiImage-class
#' @export
setMethod("dim", "CiftiImage", function(x) {
  if (is.null(x@data)) {
    c(length(x@row_axis), length(x@col_axis))
  } else {
    base::dim(x@data)
  }
})


# ============================================================================
# brain_models(), named_maps(), volume_info()
# ============================================================================

#' @rdname CiftiImage-class
#' @param structure Character string naming a brain structure (optional filter).
#' @export
setMethod("brain_models", "CiftiImage", function(x, ...) {
  bms <- list()
  for (mim in x@header@matrix@maps) {
    if (mim@index_type == "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
      bms <- c(bms, mim@brain_models)
    }
  }
  bms
})

#' @rdname CiftiImage-class
#' @export
setMethod("named_maps", "CiftiImage", function(x, ...) {
  nms <- list()
  for (mim in x@header@matrix@maps) {
    if (mim@index_type %in% c("CIFTI_INDEX_TYPE_SCALARS",
                               "CIFTI_INDEX_TYPE_LABELS")) {
      nms <- c(nms, mim@named_maps)
    }
  }
  nms
})

#' @rdname CiftiImage-class
#' @export
setMethod("volume_info", "CiftiImage", function(x, ...) {
  for (mim in x@header@matrix@maps) {
    if (!is.null(mim@volume)) return(mim@volume)
  }
  NULL
})


# ============================================================================
# extract_structure()
# ============================================================================

#' @rdname CiftiImage-class
#' @export
setMethod("extract_structure", signature(x = "CiftiImage", structure = "character"),
  function(x, structure, ...) {
    structure <- to_cifti_structure_name(structure)
    ca <- x@col_axis

    if (is(ca, "BrainModelAxis")) {
      idx <- which(ca@name == structure)
      if (length(idx) == 0L) {
        stop(sprintf("Structure '%s' not found in column axis", structure))
      }
      sub_axis <- ca[idx]
      sub_data <- if (!is.null(x@data)) x@data[, idx, drop = FALSE] else NULL
      list(data = sub_data, axis = sub_axis, indices = idx)
    } else if (is(ca, "ParcelsAxis")) {
      # For parcels, find parcels that contain the requested structure
      hits <- integer(0)
      for (i in seq_along(ca@vertices)) {
        if (structure %in% names(ca@vertices[[i]])) {
          hits <- c(hits, i)
        }
      }
      # Also check voxel-based parcels by structure naming convention
      if (length(hits) == 0L) {
        stop(sprintf("Structure '%s' not found in parcels axis", structure))
      }
      sub_axis <- ca[hits]
      sub_data <- if (!is.null(x@data)) x@data[, hits, drop = FALSE] else NULL
      list(data = sub_data, axis = sub_axis, indices = hits)
    } else {
      stop("extract_structure requires a BrainModelAxis or ParcelsAxis column axis")
    }
  }
)


# ============================================================================
# [ Subsetting
# ============================================================================

#' @rdname CiftiImage-class
#' @export
setMethod("[", signature(x = "CiftiImage", i = "ANY", j = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    has_i <- !missing(i)
    has_j <- !missing(j)

    new_data <- x@data
    new_row <- x@row_axis
    new_col <- x@col_axis

    if (has_i && has_j) {
      if (!is.null(new_data)) new_data <- new_data[i, j, drop = FALSE]
      new_row <- new_row[i]
      new_col <- new_col[j]
    } else if (has_i) {
      if (!is.null(new_data)) new_data <- new_data[i, , drop = FALSE]
      new_row <- new_row[i]
    } else if (has_j) {
      if (!is.null(new_data)) new_data <- new_data[, j, drop = FALSE]
      new_col <- new_col[j]
    }

    CiftiImage(
      data = new_data,
      row_axis = new_row,
      col_axis = new_col,
      intent = x@intent,
      file_name = x@file_name
    )
  }
)
