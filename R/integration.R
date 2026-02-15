#' @include cifti_intent.R
NULL

# ============================================================================
# Integration Layer: neuroim2 / neurosurf Conversion
# ============================================================================
# All functions in this file require optional packages (neuroim2, neurosurf).
# They are in Suggests, not Imports. We check availability at runtime.

.check_neuroim2 <- function() {
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for this function. ",
         "Install it with: install.packages('neuroim2')")
  }
}

.check_neurosurf <- function() {
  if (!requireNamespace("neurosurf", quietly = TRUE)) {
    stop("Package 'neurosurf' is required for this function. ",
         "Install it with: install.packages('neurosurf')")
  }
}


# ============================================================================
# Helper: Build NeuroSpace from CIFTI Volume Info
# ============================================================================

#' Build a NeuroSpace from CIFTI volume parameters
#'
#' @param vol_shape Integer vector of length 3 (volume dimensions).
#' @param affine 4x4 numeric affine matrix.
#' @param ndim Integer: 3 for single map, 4 for time series (adds 4th dim).
#' @param n4 Integer: size of 4th dimension (ignored if ndim=3).
#' @return A \code{neuroim2::NeuroSpace} object.
#' @keywords internal
.cifti_to_neurospace <- function(vol_shape, affine, ndim = 3L, n4 = 1L) {
  .check_neuroim2()

  spacing <- sqrt(colSums(affine[1:3, 1:3]^2))
  origin <- affine[1:3, 4]

  if (ndim == 3L) {
    neuroim2::NeuroSpace(dim = as.integer(vol_shape),
                         spacing = spacing,
                         origin = origin,
                         trans = affine)
  } else {
    neuroim2::NeuroSpace(dim = c(as.integer(vol_shape), as.integer(n4)),
                         spacing = c(spacing, 1),
                         origin = c(origin, 0),
                         trans = affine)
  }
}


# ============================================================================
# Volume Extraction: as_neuro_vol / as_neuro_vec
# ============================================================================

#' Extract Volume Data as a neuroim2 Object
#'
#' Extracts data for a subcortical (volume-based) brain structure from a
#' \code{\linkS4class{CiftiImage}} and returns it as a neuroim2 sparse volume.
#'
#' For a single-map image (e.g., one scalar map), returns a
#' \code{neuroim2::SparseNeuroVol}. For multi-map or time series data,
#' returns a \code{neuroim2::SparseNeuroVec}.
#'
#' @param x A \code{\linkS4class{CiftiImage}} with loaded data.
#' @param structure Character string naming the subcortical structure
#'   (e.g., "thalamus_left", "CIFTI_STRUCTURE_PUTAMEN_RIGHT").
#' @return A \code{SparseNeuroVol} (single map) or \code{SparseNeuroVec}
#'   (multiple maps / time series).
#'
#' @export
as_neuro_vol <- function(x, structure) {
  .check_neuroim2()
  stopifnot(is(x, "CiftiImage"))
  stopifnot(!is.null(x@data))

  es <- extract_structure(x, structure)
  ax <- es$axis

  if (!is(ax, "BrainModelAxis")) {
    stop("as_neuro_vol requires a BrainModelAxis for the extracted structure")
  }
  if (any(ax@surface_mask)) {
    stop("as_neuro_vol is for volume (voxel) structures, not surfaces")
  }
  if (is.null(ax@affine) || is.null(ax@volume_shape)) {
    stop("Volume structure lacks affine or volume_shape information")
  }

  # Convert 0-based IJK to 1-based linear indices
  linear_idx <- ijk_to_linear(ax@voxel, ax@volume_shape)
  nrows <- nrow(es$data)

  if (nrows == 1L) {
    # Single map → SparseNeuroVol
    space3 <- .cifti_to_neurospace(ax@volume_shape, ax@affine, ndim = 3L)
    neuroim2::SparseNeuroVol(data = as.numeric(es$data[1, ]),
                             space = space3,
                             indices = linear_idx)
  } else {
    # Multiple maps → SparseNeuroVec
    space4 <- .cifti_to_neurospace(ax@volume_shape, ax@affine,
                                   ndim = 4L, n4 = nrows)
    # Build a 3D logical mask from the linear indices
    mask_arr <- array(FALSE, dim = as.integer(ax@volume_shape))
    mask_arr[linear_idx] <- TRUE
    mask_vol <- neuroim2::LogicalNeuroVol(
      mask_arr,
      neuroim2::NeuroSpace(dim = as.integer(ax@volume_shape),
                           spacing = sqrt(colSums(ax@affine[1:3, 1:3]^2)),
                           origin = ax@affine[1:3, 4],
                           trans = ax@affine)
    )
    # data is rows x cols → t() gives [voxels x time], we need [time x voxels]
    # es$data is already [time x voxels] so just pass it
    neuroim2::SparseNeuroVec(data = es$data,
                             space = space4,
                             mask = mask_vol)
  }
}


#' Extract All Subcortical Volume Data as a neuroim2 Object
#'
#' Combines all volume-based brain structures into a single
#' \code{SparseNeuroVol} or \code{SparseNeuroVec}.
#'
#' @param x A \code{\linkS4class{CiftiImage}} with loaded data.
#' @return A \code{SparseNeuroVol} (single map) or \code{SparseNeuroVec}
#'   (multiple maps / time series).
#'
#' @export
as_neuro_vec <- function(x) {
  .check_neuroim2()
  stopifnot(is(x, "CiftiImage"))
  stopifnot(!is.null(x@data))

  ca <- x@col_axis
  if (!is(ca, "BrainModelAxis")) {
    stop("as_neuro_vec requires a BrainModelAxis column axis")
  }

  vol_idx <- which(!ca@surface_mask)
  if (length(vol_idx) == 0L) {
    stop("No volume (voxel) entries in this CiftiImage")
  }
  if (is.null(ca@affine) || is.null(ca@volume_shape)) {
    stop("BrainModelAxis lacks affine or volume_shape information")
  }

  voxel_ijk <- ca@voxel[vol_idx, , drop = FALSE]
  linear_idx <- ijk_to_linear(voxel_ijk, ca@volume_shape)
  sub_data <- x@data[, vol_idx, drop = FALSE]
  nrows <- nrow(sub_data)

  if (nrows == 1L) {
    space3 <- .cifti_to_neurospace(ca@volume_shape, ca@affine, ndim = 3L)
    neuroim2::SparseNeuroVol(data = as.numeric(sub_data[1, ]),
                             space = space3,
                             indices = linear_idx)
  } else {
    space4 <- .cifti_to_neurospace(ca@volume_shape, ca@affine,
                                   ndim = 4L, n4 = nrows)
    mask_arr <- array(FALSE, dim = as.integer(ca@volume_shape))
    mask_arr[linear_idx] <- TRUE
    space3 <- .cifti_to_neurospace(ca@volume_shape, ca@affine, ndim = 3L)
    mask_vol <- neuroim2::LogicalNeuroVol(mask_arr, space3)
    neuroim2::SparseNeuroVec(data = sub_data, space = space4, mask = mask_vol)
  }
}


# ============================================================================
# Surface Extraction: as_neuro_surface / as_neuro_surface_vector
# ============================================================================

#' Extract Surface Data as a neurosurf Object
#'
#' Extracts data for a cortical surface structure from a
#' \code{\linkS4class{CiftiImage}} and returns it as a neurosurf object.
#'
#' For a single-map image, returns a \code{neurosurf::NeuroSurface}.
#' For multi-map or time series data, returns a
#' \code{neurosurf::NeuroSurfaceVector}.
#'
#' @param x A \code{\linkS4class{CiftiImage}} with loaded data.
#' @param structure Character string naming the surface structure
#'   (e.g., "cortex_left", "cortex_right").
#' @param geometry A \code{neurosurf::SurfaceGeometry} object providing the
#'   mesh for this hemisphere. Required.
#' @return A \code{NeuroSurface} (single map) or \code{NeuroSurfaceVector}
#'   (multiple maps / time series).
#'
#' @export
as_neuro_surface <- function(x, structure, geometry) {
  .check_neurosurf()
  stopifnot(is(x, "CiftiImage"))
  stopifnot(!is.null(x@data))

  es <- extract_structure(x, structure)
  ax <- es$axis

  if (!is(ax, "BrainModelAxis")) {
    stop("as_neuro_surface requires a BrainModelAxis for the extracted structure")
  }
  if (!all(ax@surface_mask)) {
    stop("as_neuro_surface is for surface structures, not volumes")
  }

  # Convert 0-based vertex indices to 1-based
  r_indices <- to_r_index(ax@vertex)
  nrows <- nrow(es$data)

  if (nrows == 1L) {
    neurosurf::NeuroSurface(geometry = geometry,
                            indices = r_indices,
                            data = as.numeric(es$data[1, ]))
  } else {
    # NeuroSurfaceVector: mat = vertices x measures (columns)
    # es$data is [time x vertices], so transpose to [vertices x time]
    neurosurf::NeuroSurfaceVector(geometry = geometry,
                                  indices = r_indices,
                                  mat = t(es$data))
  }
}


#' Extract Bilateral Surface Data as a neurosurf Object
#'
#' Extracts left and right cortex data and combines them into a
#' \code{neurosurf::BilatNeuroSurfaceVector}.
#'
#' @param x A \code{\linkS4class{CiftiImage}} with loaded data.
#' @param left_geometry A \code{neurosurf::SurfaceGeometry} for the left hemisphere.
#' @param right_geometry A \code{neurosurf::SurfaceGeometry} for the right hemisphere.
#' @return A \code{BilatNeuroSurfaceVector}.
#'
#' @export
as_bilat_surface_vector <- function(x, left_geometry, right_geometry) {
  .check_neurosurf()
  stopifnot(is(x, "CiftiImage"))
  stopifnot(!is.null(x@data))

  left_nsv <- as_neuro_surface(x, "cortex_left", left_geometry)
  right_nsv <- as_neuro_surface(x, "cortex_right", right_geometry)

  # Ensure both are NeuroSurfaceVector (promote single-map if needed)
  if (is(left_nsv, "NeuroSurface") && !is(left_nsv, "NeuroSurfaceVector")) {
    left_nsv <- neurosurf::NeuroSurfaceVector(
      geometry = left_geometry,
      indices = left_nsv@indices,
      mat = matrix(left_nsv@data, ncol = 1)
    )
  }
  if (is(right_nsv, "NeuroSurface") && !is(right_nsv, "NeuroSurfaceVector")) {
    right_nsv <- neurosurf::NeuroSurfaceVector(
      geometry = right_geometry,
      indices = right_nsv@indices,
      mat = matrix(right_nsv@data, ncol = 1)
    )
  }

  new("BilatNeuroSurfaceVector", left = left_nsv, right = right_nsv)
}


# ============================================================================
# Build CiftiImage from neuroim2/neurosurf Objects
# ============================================================================

#' Build a CiftiImage from Spatial Data Objects
#'
#' Constructs a CiftiImage from neuroim2 volume objects and/or neurosurf
#' surface objects.
#'
#' @param surfaces A named list of neurosurf objects, where names are CIFTI
#'   structure identifiers (e.g., "cortex_left", "cortex_right"). Each element
#'   should be a \code{NeuroSurface} or \code{NeuroSurfaceVector}.
#' @param volumes A named list of neuroim2 volume objects, where names are CIFTI
#'   structure identifiers (e.g., "thalamus_left"). Each element should be a
#'   \code{SparseNeuroVol} or \code{SparseNeuroVec}.
#' @param row_axis A \code{\linkS4class{CiftiAxis}} for the row dimension
#'   (e.g., a \code{SeriesAxis} for time series data). If NULL, a default
#'   \code{SeriesAxis} is created.
#' @param intent Integer NIfTI intent code (default: 3002 for dtseries).
#' @return A \code{\linkS4class{CiftiImage}}.
#'
#' @export
cifti_from_data <- function(surfaces = list(), volumes = list(),
                             row_axis = NULL, intent = 3002L) {
  bm_list <- list()
  data_cols <- list()

  # --- Process surfaces ---
  for (nm in names(surfaces)) {
    obj <- surfaces[[nm]]
    struct_name <- to_cifti_structure_name(nm)

    if (is(obj, "NeuroSurfaceVector")) {
      .check_neurosurf()
      # indices are 1-based in neurosurf, convert to 0-based for CIFTI
      verts_0 <- to_cifti_index(obj@indices)
      nvertex <- length(neurosurf::nodes(obj@geometry))
      bm <- brain_model_from_surface(verts_0, nvertex = nvertex, name = struct_name)
      # NeuroSurfaceVector mat is [vertices x time], transpose to [time x vertices]
      data_cols[[length(data_cols) + 1]] <- t(as.matrix(obj@data))
    } else if (is(obj, "NeuroSurface")) {
      .check_neurosurf()
      verts_0 <- to_cifti_index(obj@indices)
      nvertex <- length(neurosurf::nodes(obj@geometry))
      bm <- brain_model_from_surface(verts_0, nvertex = nvertex, name = struct_name)
      data_cols[[length(data_cols) + 1]] <- matrix(obj@data, nrow = 1)
    } else {
      stop(sprintf("Surface '%s' must be a NeuroSurface or NeuroSurfaceVector", nm))
    }

    bm_list[[length(bm_list) + 1]] <- bm
  }

  # --- Process volumes ---
  for (nm in names(volumes)) {
    obj <- volumes[[nm]]
    struct_name <- to_cifti_structure_name(nm)

    if (is(obj, "SparseNeuroVec")) {
      .check_neuroim2()
      mask_arr <- as.array(obj@mask)
      sp <- obj@mask@space
      affine <- sp@trans
      bm <- brain_model_from_mask(mask_arr, name = struct_name, affine = affine)
      # SparseNeuroVec data is [time x voxels]
      data_cols[[length(data_cols) + 1]] <- obj@data
    } else if (is(obj, "SparseNeuroVol")) {
      .check_neuroim2()
      sv <- obj@data  # sparseVector
      idx <- sv@i     # 1-based indices
      vals <- sv@x
      vol_dims <- dim(obj@space)
      ijk_0 <- linear_to_ijk(idx, vol_dims)
      sp <- obj@space
      affine <- sp@trans

      # Build mask array
      mask_arr <- array(FALSE, dim = vol_dims)
      mask_arr[idx] <- TRUE
      bm <- brain_model_from_mask(mask_arr, name = struct_name, affine = affine)
      data_cols[[length(data_cols) + 1]] <- matrix(vals, nrow = 1)
    } else {
      stop(sprintf("Volume '%s' must be a SparseNeuroVol or SparseNeuroVec", nm))
    }

    bm_list[[length(bm_list) + 1]] <- bm
  }

  if (length(bm_list) == 0) {
    stop("At least one surface or volume must be provided")
  }

  # Combine brain models
  combined_bm <- bm_list[[1]]
  if (length(bm_list) > 1) {
    combined_bm <- do.call(c, bm_list)
  }

  # Combine data columns
  combined_data <- do.call(cbind, data_cols)

  # Default row axis if not provided
  if (is.null(row_axis)) {
    nrows <- nrow(combined_data)
    row_axis <- new("SeriesAxis",
      start = 0, step = 1,
      size = as.integer(nrows),
      unit = "SECOND"
    )
  }

  CiftiImage(
    data = combined_data,
    row_axis = row_axis,
    col_axis = combined_bm,
    intent = as.integer(intent)
  )
}
