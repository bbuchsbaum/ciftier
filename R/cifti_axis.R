#' @include cifti_xml.R
NULL

# ============================================================================
# Axis Constructors
# ============================================================================

#' Create a BrainModelAxis from a Mask
#'
#' @param mask For surfaces: a logical vector (length = nvertex).
#'   For volumes: a 3D logical array.
#' @param name Canonical CIFTI structure string.
#' @param affine 4x4 affine matrix (required for volume masks).
#' @return A \code{\linkS4class{BrainModelAxis}} object.
#'
#' @export
brain_model_from_mask <- function(mask, name, affine = NULL) {
  name <- to_cifti_structure_name(name)

  if (is.logical(mask) && is.null(dim(mask))) {
    # 1D logical mask → surface model
    nvert <- length(mask)
    vertices <- which(mask) - 1L  # 0-based
    n <- length(vertices)

    new("BrainModelAxis",
      name = rep(name, n),
      vertex = vertices,
      voxel = matrix(NA_integer_, nrow = n, ncol = 3),
      surface_mask = rep(TRUE, n),
      nvertices = stats::setNames(list(nvert), name),
      affine = affine,
      volume_shape = NULL
    )
  } else if (is.array(mask) && length(dim(mask)) == 3) {
    # 3D array → volume model
    if (is.null(affine)) stop("affine required for volume masks")
    vol_shape <- dim(mask)
    ijk <- which(mask, arr.ind = TRUE) - 1L  # 0-based IJK
    storage.mode(ijk) <- "integer"
    colnames(ijk) <- c("i", "j", "k")
    n <- nrow(ijk)

    new("BrainModelAxis",
      name = rep(name, n),
      vertex = rep(NA_integer_, n),
      voxel = ijk,
      surface_mask = rep(FALSE, n),
      nvertices = list(),
      affine = affine,
      volume_shape = as.integer(vol_shape)
    )
  } else {
    stop("mask must be a logical vector (surface) or 3D logical array (volume)")
  }
}


#' Create a BrainModelAxis from Surface Vertex Indices
#'
#' @param vertices Integer vector of 0-based vertex indices.
#' @param nvertex Total number of vertices on the surface.
#' @param name Canonical CIFTI structure string.
#' @return A \code{\linkS4class{BrainModelAxis}} object.
#'
#' @export
brain_model_from_surface <- function(vertices, nvertex, name) {
  name <- to_cifti_structure_name(name)
  vertices <- as.integer(vertices)
  n <- length(vertices)

  new("BrainModelAxis",
    name = rep(name, n),
    vertex = vertices,
    voxel = matrix(NA_integer_, nrow = n, ncol = 3),
    surface_mask = rep(TRUE, n),
    nvertices = stats::setNames(list(as.integer(nvertex)), name),
    affine = NULL,
    volume_shape = NULL
  )
}


#' Create a ParcelsAxis from Brain Models
#'
#' @param named_brain_models A named list where each element is a
#'   \code{\linkS4class{BrainModelAxis}} and names are parcel names.
#' @return A \code{\linkS4class{ParcelsAxis}} object.
#'
#' @export
parcels_from_brain_models <- function(named_brain_models) {
  parcel_names <- names(named_brain_models)
  if (is.null(parcel_names)) stop("named_brain_models must be a named list")

  all_nvertices <- list()
  all_voxels <- list()
  all_vertices <- list()
  affine <- NULL
  volume_shape <- NULL

  for (i in seq_along(named_brain_models)) {
    bm <- named_brain_models[[i]]
    stopifnot(is(bm, "BrainModelAxis"))

    # Collect nvertices
    for (nm in names(bm@nvertices)) {
      all_nvertices[[nm]] <- bm@nvertices[[nm]]
    }

    # Separate surface and volume entries
    surf_idx <- which(bm@surface_mask)
    vol_idx <- which(!bm@surface_mask)

    # Vertices by structure
    pverts <- list()
    if (length(surf_idx) > 0) {
      structs <- unique(bm@name[surf_idx])
      for (s in structs) {
        mask <- bm@name[surf_idx] == s
        pverts[[s]] <- bm@vertex[surf_idx[mask]]
      }
    }
    all_vertices[[i]] <- pverts

    # Voxels
    if (length(vol_idx) > 0) {
      all_voxels[[i]] <- bm@voxel[vol_idx, , drop = FALSE]
      if (is.null(affine)) affine <- bm@affine
      if (is.null(volume_shape)) volume_shape <- bm@volume_shape
    } else {
      all_voxels[[i]] <- matrix(integer(0), ncol = 3)
    }
  }

  new("ParcelsAxis",
    parcel_name = parcel_names,
    voxels = all_voxels,
    vertices = all_vertices,
    nvertices = all_nvertices,
    affine = affine,
    volume_shape = volume_shape
  )
}


# ============================================================================
# Header ↔ Axis Conversion
# ============================================================================

#' Convert CiftiHeader to Axis Objects
#'
#' @param header A \code{\linkS4class{CiftiHeader}} object.
#' @return A list with elements \code{row_axis} and \code{col_axis}.
#'
#' @export
header_to_axes <- function(header) {
  stopifnot(is(header, "CiftiHeader"))

  axes <- list(row_axis = NULL, col_axis = NULL)

  for (mim in header@matrix@maps) {
    axis <- .mim_to_axis(mim)
    for (d in mim@applies_to_dimension) {
      if (d == 0L) {
        axes$row_axis <- axis
      } else if (d == 1L) {
        axes$col_axis <- axis
      }
    }
  }

  axes
}


#' Convert Axis Objects Back to CiftiHeader
#'
#' @param row_axis A \code{\linkS4class{CiftiAxis}} for rows (dimension 0).
#' @param col_axis A \code{\linkS4class{CiftiAxis}} for columns (dimension 1).
#' @param version Character version string (default "2").
#' @return A \code{\linkS4class{CiftiHeader}} object.
#'
#' @export
axes_to_header <- function(row_axis, col_axis, version = "2") {
  mim_row <- .axis_to_mim(row_axis, 0L)
  mim_col <- .axis_to_mim(col_axis, 1L)

  new("CiftiHeader",
    version = version,
    matrix = new("CiftiMatrix", maps = list(mim_row, mim_col))
  )
}


# ============================================================================
# Internal: MatrixIndicesMap ↔ Axis
# ============================================================================

#' Convert a MatrixIndicesMap to an Axis
#' @keywords internal
.mim_to_axis <- function(mim) {
  switch(mim@index_type,
    CIFTI_INDEX_TYPE_BRAIN_MODELS = .mim_to_brain_model_axis(mim),
    CIFTI_INDEX_TYPE_PARCELS      = .mim_to_parcels_axis(mim),
    CIFTI_INDEX_TYPE_SERIES       = .mim_to_series_axis(mim),
    CIFTI_INDEX_TYPE_SCALARS      = .mim_to_scalar_axis(mim),
    CIFTI_INDEX_TYPE_LABELS       = .mim_to_label_axis(mim),
    stop(sprintf("Unknown index type: %s", mim@index_type))
  )
}


#' @keywords internal
.mim_to_brain_model_axis <- function(mim) {
  if (length(mim@brain_models) == 0) {
    return(new("BrainModelAxis"))
  }

  # Collect all grayordinates
  names_all <- character(0)
  vertex_all <- integer(0)
  voxel_rows <- list()
  smask_all <- logical(0)
  nvertices <- list()
  affine <- NULL
  vol_shape <- NULL

  if (!is.null(mim@volume)) {
    affine <- mim@volume@transform
    vol_shape <- mim@volume@dimensions
  }

  for (bm in mim@brain_models) {
    n <- bm@index_count

    if (bm@model_type == "CIFTI_MODEL_TYPE_SURFACE") {
      names_all <- c(names_all, rep(bm@brain_structure, n))
      vertex_all <- c(vertex_all, bm@vertex_indices)
      voxel_rows[[length(voxel_rows) + 1]] <- matrix(NA_integer_, nrow = n, ncol = 3)
      smask_all <- c(smask_all, rep(TRUE, n))
      nvertices[[bm@brain_structure]] <- bm@surface_number_of_vertices
    } else {
      names_all <- c(names_all, rep(bm@brain_structure, n))
      vertex_all <- c(vertex_all, rep(NA_integer_, n))
      voxel_rows[[length(voxel_rows) + 1]] <- bm@voxel_indices_ijk
      smask_all <- c(smask_all, rep(FALSE, n))
    }
  }

  voxel_mat <- do.call(rbind, voxel_rows)
  if (is.null(voxel_mat) || length(voxel_mat) == 0) {
    voxel_mat <- matrix(NA_integer_, nrow = length(names_all), ncol = 3)
  }
  colnames(voxel_mat) <- c("i", "j", "k")

  new("BrainModelAxis",
    name = names_all,
    vertex = vertex_all,
    voxel = voxel_mat,
    surface_mask = smask_all,
    nvertices = nvertices,
    affine = affine,
    volume_shape = vol_shape
  )
}


#' @keywords internal
.mim_to_parcels_axis <- function(mim) {
  pnames <- character(0)
  voxels <- list()
  vertices <- list()
  nvert <- list()

  for (surf in mim@surfaces) {
    nvert[[surf@brain_structure]] <- surf@number_of_vertices
  }

  for (p in mim@parcels) {
    pnames <- c(pnames, p@name)
    voxels[[length(voxels) + 1]] <- p@voxel_indices_ijk
    vertices[[length(vertices) + 1]] <- p@surface_vertices
  }

  affine <- NULL
  vol_shape <- NULL
  if (!is.null(mim@volume)) {
    affine <- mim@volume@transform
    vol_shape <- mim@volume@dimensions
  }

  new("ParcelsAxis",
    parcel_name = pnames,
    voxels = voxels,
    vertices = vertices,
    nvertices = nvert,
    affine = affine,
    volume_shape = vol_shape
  )
}


#' @keywords internal
.mim_to_series_axis <- function(mim) {
  new("SeriesAxis",
    start = mim@series_start,
    step  = mim@series_step,
    size  = mim@number_of_series_points,
    unit  = mim@series_unit
  )
}


#' @keywords internal
.mim_to_scalar_axis <- function(mim) {
  nms <- vapply(mim@named_maps, function(nm) nm@map_name, character(1))
  metas <- lapply(mim@named_maps, function(nm) nm@metadata)
  new("ScalarAxis", name = nms, meta = metas)
}


#' @keywords internal
.mim_to_label_axis <- function(mim) {
  nms <- vapply(mim@named_maps, function(nm) nm@map_name, character(1))
  labs <- lapply(mim@named_maps, function(nm) nm@label_table)
  metas <- lapply(mim@named_maps, function(nm) nm@metadata)
  new("LabelAxis", name = nms, label = labs, meta = metas)
}


#' Convert an Axis back to a MatrixIndicesMap
#' @keywords internal
.axis_to_mim <- function(axis, dimension) {
  if (is(axis, "BrainModelAxis")) {
    .brain_model_axis_to_mim(axis, dimension)
  } else if (is(axis, "ParcelsAxis")) {
    .parcels_axis_to_mim(axis, dimension)
  } else if (is(axis, "SeriesAxis")) {
    .series_axis_to_mim(axis, dimension)
  } else if (is(axis, "ScalarAxis")) {
    .scalar_axis_to_mim(axis, dimension)
  } else if (is(axis, "LabelAxis")) {
    .label_axis_to_mim(axis, dimension)
  } else {
    stop("Unknown axis type")
  }
}


#' @keywords internal
.brain_model_axis_to_mim <- function(axis, dimension) {
  structs <- unique(axis@name)
  brain_models <- list()
  offset <- 0L

  for (s in structs) {
    idx <- which(axis@name == s)
    n <- length(idx)
    is_surf <- axis@surface_mask[idx[1]]

    if (is_surf) {
      bm <- new("CiftiBrainModel",
        index_offset = as.integer(offset),
        index_count = as.integer(n),
        model_type = "CIFTI_MODEL_TYPE_SURFACE",
        brain_structure = s,
        surface_number_of_vertices = as.integer(axis@nvertices[[s]] %||% 0L),
        vertex_indices = axis@vertex[idx],
        voxel_indices_ijk = matrix(integer(0), ncol = 3)
      )
    } else {
      bm <- new("CiftiBrainModel",
        index_offset = as.integer(offset),
        index_count = as.integer(n),
        model_type = "CIFTI_MODEL_TYPE_VOXELS",
        brain_structure = s,
        surface_number_of_vertices = 0L,
        vertex_indices = integer(0),
        voxel_indices_ijk = axis@voxel[idx, , drop = FALSE]
      )
    }

    brain_models <- c(brain_models, list(bm))
    offset <- offset + n
  }

  vol <- NULL
  if (!is.null(axis@affine) && !is.null(axis@volume_shape)) {
    vol <- new("CiftiVolume",
      dimensions = axis@volume_shape,
      transform = axis@affine
    )
  }

  new("CiftiMatrixIndicesMap",
    applies_to_dimension = as.integer(dimension),
    index_type = "CIFTI_INDEX_TYPE_BRAIN_MODELS",
    brain_models = brain_models,
    volume = vol
  )
}


#' @keywords internal
.parcels_axis_to_mim <- function(axis, dimension) {
  parcels <- lapply(seq_along(axis@parcel_name), function(i) {
    new("CiftiParcel",
      name = axis@parcel_name[i],
      surface_vertices = axis@vertices[[i]],
      voxel_indices_ijk = axis@voxels[[i]]
    )
  })

  surfaces <- lapply(names(axis@nvertices), function(nm) {
    new("CiftiSurface",
      brain_structure = nm,
      number_of_vertices = as.integer(axis@nvertices[[nm]])
    )
  })

  vol <- NULL
  if (!is.null(axis@affine) && !is.null(axis@volume_shape)) {
    vol <- new("CiftiVolume",
      dimensions = axis@volume_shape,
      transform = axis@affine
    )
  }

  new("CiftiMatrixIndicesMap",
    applies_to_dimension = as.integer(dimension),
    index_type = "CIFTI_INDEX_TYPE_PARCELS",
    parcels = parcels,
    surfaces = surfaces,
    volume = vol
  )
}


#' @keywords internal
.series_axis_to_mim <- function(axis, dimension) {
  new("CiftiMatrixIndicesMap",
    applies_to_dimension = as.integer(dimension),
    index_type = "CIFTI_INDEX_TYPE_SERIES",
    series_start = axis@start,
    series_step = axis@step,
    series_unit = axis@unit,
    number_of_series_points = axis@size
  )
}


#' @keywords internal
.scalar_axis_to_mim <- function(axis, dimension) {
  named_maps <- lapply(seq_along(axis@name), function(i) {
    meta <- if (i <= length(axis@meta)) axis@meta[[i]] else list()
    new("CiftiNamedMap",
      map_name = axis@name[i],
      metadata = meta
    )
  })

  new("CiftiMatrixIndicesMap",
    applies_to_dimension = as.integer(dimension),
    index_type = "CIFTI_INDEX_TYPE_SCALARS",
    named_maps = named_maps
  )
}


#' @keywords internal
.label_axis_to_mim <- function(axis, dimension) {
  named_maps <- lapply(seq_along(axis@name), function(i) {
    lt <- if (i <= length(axis@label)) axis@label[[i]] else new("CiftiLabelTable")
    meta <- if (i <= length(axis@meta)) axis@meta[[i]] else list()
    new("CiftiNamedMap",
      map_name = axis@name[i],
      label_table = lt,
      metadata = meta
    )
  })

  new("CiftiMatrixIndicesMap",
    applies_to_dimension = as.integer(dimension),
    index_type = "CIFTI_INDEX_TYPE_LABELS",
    named_maps = named_maps
  )
}
