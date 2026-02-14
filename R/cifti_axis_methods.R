#' @include cifti_axis.R
NULL

# ============================================================================
# show() Methods
# ============================================================================

#' @rdname BrainModelAxis-class
#' @param object A \code{BrainModelAxis} object.
#' @param x A \code{BrainModelAxis} object.
#' @param i Numeric or logical index vector.
#' @param j Not used.
#' @param ... Additional arguments (ignored).
#' @param drop Logical (ignored).
#' @param e1 First \code{BrainModelAxis}.
#' @param e2 Second \code{BrainModelAxis}.
#' @export
setMethod("show", "BrainModelAxis", function(object) {
  n <- length(object@name)
  n_surf <- sum(object@surface_mask)
  n_vol <- n - n_surf
  cat(sprintf("BrainModelAxis [%d grayordinates]\n", n))
  if (n_surf > 0) {
    structs <- unique(object@name[object@surface_mask])
    for (s in structs) {
      cnt <- sum(object@name == s & object@surface_mask)
      nv <- object@nvertices[[s]] %||% "?"
      cat(sprintf("  %s: %d / %s vertices\n", s, cnt, nv))
    }
  }
  if (n_vol > 0) {
    structs <- unique(object@name[!object@surface_mask])
    for (s in structs) {
      cnt <- sum(object@name == s & !object@surface_mask)
      cat(sprintf("  %s: %d voxels\n", s, cnt))
    }
    if (!is.null(object@volume_shape)) {
      cat(sprintf("  Volume: %s\n", paste(object@volume_shape, collapse = " x ")))
    }
  }
  invisible(object)
})

#' @rdname ParcelsAxis-class
#' @param object A \code{ParcelsAxis} object.
#' @param x A \code{ParcelsAxis} object.
#' @param i Numeric index vector.
#' @param j Not used.
#' @param ... Additional arguments (ignored).
#' @param drop Logical (ignored).
#' @export
setMethod("show", "ParcelsAxis", function(object) {
  n <- length(object@parcel_name)
  cat(sprintf("ParcelsAxis [%d parcels]\n", n))
  show_n <- min(n, 5L)
  if (show_n > 0) {
    for (i in seq_len(show_n)) {
      cat(sprintf("  %s\n", object@parcel_name[i]))
    }
    if (n > show_n) cat(sprintf("  ... and %d more\n", n - show_n))
  }
  invisible(object)
})

#' @rdname SeriesAxis-class
#' @param object A \code{SeriesAxis} object.
#' @param x A \code{SeriesAxis} object.
#' @param i Numeric index vector.
#' @param j Not used.
#' @param ... Additional arguments (ignored).
#' @param drop Logical (ignored).
#' @param e1 First \code{SeriesAxis}.
#' @param e2 Second \code{SeriesAxis}.
#' @export
setMethod("show", "SeriesAxis", function(object) {
  cat(sprintf("SeriesAxis [%d points, start=%.4g, step=%.4g, unit=%s]\n",
    object@size, object@start, object@step, object@unit))
  invisible(object)
})

#' @rdname ScalarAxis-class
#' @param object A \code{ScalarAxis} object.
#' @param x A \code{ScalarAxis} object.
#' @param i Numeric index vector.
#' @param j Not used.
#' @param ... Additional arguments (ignored).
#' @param drop Logical (ignored).
#' @param e1 First \code{ScalarAxis}.
#' @param e2 Second \code{ScalarAxis}.
#' @export
setMethod("show", "ScalarAxis", function(object) {
  n <- length(object@name)
  cat(sprintf("ScalarAxis [%d maps]\n", n))
  show_n <- min(n, 5L)
  if (show_n > 0) {
    for (i in seq_len(show_n)) cat(sprintf("  %s\n", object@name[i]))
    if (n > show_n) cat(sprintf("  ... and %d more\n", n - show_n))
  }
  invisible(object)
})

#' @rdname LabelAxis-class
#' @param object A \code{LabelAxis} object.
#' @param x A \code{LabelAxis} object.
#' @param i Numeric index vector.
#' @param j Not used.
#' @param ... Additional arguments (ignored).
#' @param drop Logical (ignored).
#' @export
setMethod("show", "LabelAxis", function(object) {
  n <- length(object@name)
  cat(sprintf("LabelAxis [%d maps]\n", n))
  show_n <- min(n, 5L)
  if (show_n > 0) {
    for (i in seq_len(show_n)) {
      nlab <- length(object@label[[i]]@labels)
      cat(sprintf("  %s (%d labels)\n", object@name[i], nlab))
    }
    if (n > show_n) cat(sprintf("  ... and %d more\n", n - show_n))
  }
  invisible(object)
})


# ============================================================================
# length() Methods
# ============================================================================

#' @rdname BrainModelAxis-class
#' @export
setMethod("length", "BrainModelAxis", function(x) length(x@name))

#' @rdname ParcelsAxis-class
#' @export
setMethod("length", "ParcelsAxis", function(x) length(x@parcel_name))

#' @rdname SeriesAxis-class
#' @export
setMethod("length", "SeriesAxis", function(x) x@size)

#' @rdname ScalarAxis-class
#' @export
setMethod("length", "ScalarAxis", function(x) length(x@name))

#' @rdname LabelAxis-class
#' @export
setMethod("length", "LabelAxis", function(x) length(x@name))


# ============================================================================
# [ Subsetting Methods
# ============================================================================

#' @rdname BrainModelAxis-class
#' @export
setMethod("[", signature(x = "BrainModelAxis", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    new("BrainModelAxis",
      name = x@name[i],
      vertex = x@vertex[i],
      voxel = x@voxel[i, , drop = FALSE],
      surface_mask = x@surface_mask[i],
      nvertices = x@nvertices,
      affine = x@affine,
      volume_shape = x@volume_shape
    )
  }
)

#' @rdname BrainModelAxis-class
#' @export
setMethod("[", signature(x = "BrainModelAxis", i = "logical", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    x[which(i)]
  }
)

#' @rdname ParcelsAxis-class
#' @export
setMethod("[", signature(x = "ParcelsAxis", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    new("ParcelsAxis",
      parcel_name = x@parcel_name[i],
      voxels = x@voxels[i],
      vertices = x@vertices[i],
      nvertices = x@nvertices,
      affine = x@affine,
      volume_shape = x@volume_shape
    )
  }
)

#' @rdname SeriesAxis-class
#' @export
setMethod("[", signature(x = "SeriesAxis", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    # For contiguous ranges, adjust start; otherwise return ScalarAxis
    if (length(i) > 1 && all(diff(i) == 1L)) {
      new("SeriesAxis",
        start = x@start + (i[1] - 1L) * x@step,
        step = x@step,
        size = length(i),
        unit = x@unit
      )
    } else {
      # Non-contiguous: degrade to ScalarAxis with time labels
      times <- x@start + (i - 1L) * x@step
      new("ScalarAxis",
        name = sprintf("%.4g %s", times, x@unit),
        meta = rep(list(list()), length(i))
      )
    }
  }
)

#' @rdname ScalarAxis-class
#' @export
setMethod("[", signature(x = "ScalarAxis", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    new("ScalarAxis",
      name = x@name[i],
      meta = x@meta[i]
    )
  }
)

#' @rdname LabelAxis-class
#' @export
setMethod("[", signature(x = "LabelAxis", i = "numeric", j = "missing"),
  function(x, i, j, ..., drop = TRUE) {
    new("LabelAxis",
      name = x@name[i],
      label = x@label[i],
      meta = x@meta[i]
    )
  }
)


# ============================================================================
# c() Concatenation Methods
# ============================================================================

#' @rdname BrainModelAxis-class
#' @export
setMethod("c", "BrainModelAxis", function(x, ...) {
  others <- list(...)
  if (length(others) == 0) return(x)

  all_axes <- c(list(x), others)
  stopifnot(all(vapply(all_axes, is, logical(1), "BrainModelAxis")))

  nvert_merged <- x@nvertices
  affine <- x@affine
  vshape <- x@volume_shape

  for (ax in others) {
    for (nm in names(ax@nvertices)) {
      nvert_merged[[nm]] <- ax@nvertices[[nm]]
    }
    if (is.null(affine) && !is.null(ax@affine)) affine <- ax@affine
    if (is.null(vshape) && !is.null(ax@volume_shape)) vshape <- ax@volume_shape
  }

  new("BrainModelAxis",
    name = unlist(lapply(all_axes, function(a) a@name)),
    vertex = unlist(lapply(all_axes, function(a) a@vertex)),
    voxel = do.call(rbind, lapply(all_axes, function(a) a@voxel)),
    surface_mask = unlist(lapply(all_axes, function(a) a@surface_mask)),
    nvertices = nvert_merged,
    affine = affine,
    volume_shape = vshape
  )
})

#' @rdname SeriesAxis-class
#' @export
setMethod("c", "SeriesAxis", function(x, ...) {
  others <- list(...)
  if (length(others) == 0) return(x)

  # Concatenating series: sum sizes, keep start/step from first
  total <- x@size
  for (ax in others) {
    stopifnot(is(ax, "SeriesAxis"))
    total <- total + ax@size
  }

  new("SeriesAxis",
    start = x@start,
    step = x@step,
    size = total,
    unit = x@unit
  )
})

#' @rdname ScalarAxis-class
#' @export
setMethod("c", "ScalarAxis", function(x, ...) {
  others <- list(...)
  if (length(others) == 0) return(x)

  all_axes <- c(list(x), others)
  new("ScalarAxis",
    name = unlist(lapply(all_axes, function(a) a@name)),
    meta = unlist(lapply(all_axes, function(a) a@meta), recursive = FALSE)
  )
})


# ============================================================================
# == Equality
# ============================================================================

#' @rdname SeriesAxis-class
#' @export
setMethod("==", signature(e1 = "SeriesAxis", e2 = "SeriesAxis"), function(e1, e2) {
  e1@start == e2@start && e1@step == e2@step &&
    e1@size == e2@size && e1@unit == e2@unit
})

#' @rdname ScalarAxis-class
#' @export
setMethod("==", signature(e1 = "ScalarAxis", e2 = "ScalarAxis"), function(e1, e2) {
  identical(e1@name, e2@name)
})

#' @rdname BrainModelAxis-class
#' @export
setMethod("==", signature(e1 = "BrainModelAxis", e2 = "BrainModelAxis"), function(e1, e2) {
  identical(e1@name, e2@name) && identical(e1@vertex, e2@vertex) &&
    identical(e1@voxel, e2@voxel)
})


# ============================================================================
# iter_structures()
# ============================================================================

#' @rdname BrainModelAxis-class
#' @export
setMethod("iter_structures", "BrainModelAxis", function(x, ...) {
  structs <- unique(x@name)
  result <- list()
  for (s in structs) {
    idx <- which(x@name == s)
    result[[s]] <- idx
  }
  result
})

#' @rdname ParcelsAxis-class
#' @export
setMethod("iter_structures", "ParcelsAxis", function(x, ...) {
  # For parcels, each parcel is its own "structure"
  stats::setNames(as.list(seq_along(x@parcel_name)), x@parcel_name)
})
