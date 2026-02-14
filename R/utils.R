#' @include all_class.R
NULL

# ============================================================================
# Index Conversion Helpers
# ============================================================================

#' Convert 0-based CIFTI Index to 1-based R Index
#'
#' @param x An integer vector of 0-based indices.
#' @return An integer vector of 1-based indices.
#' @export
to_r_index <- function(x) {
  as.integer(x) + 1L
}

#' Convert 1-based R Index to 0-based CIFTI Index
#'
#' @param x An integer vector of 1-based indices.
#' @return An integer vector of 0-based indices.
#' @export
to_cifti_index <- function(x) {
  as.integer(x) - 1L
}

# ============================================================================
# Structure Name Normalization
# ============================================================================

#' Normalize a Brain Structure Name to CIFTI Convention
#'
#' Converts short names, common aliases, and partial matches to the
#' canonical \code{CIFTI_STRUCTURE_*} string.
#'
#' @param name A character string (e.g., "cortex_left", "CORTEX_LEFT",
#'   "CortexLeft", "CIFTI_STRUCTURE_CORTEX_LEFT").
#' @return The canonical CIFTI structure string.
#'
#' @examples
#' to_cifti_structure_name("cortex_left")
#' to_cifti_structure_name("CORTEX_LEFT")
#' to_cifti_structure_name("CortexLeft")
#'
#' @export
to_cifti_structure_name <- function(name) {
  stopifnot(is.character(name), length(name) == 1L)

  # Already canonical?
  if (name %in% CIFTI_STRUCTURES) return(name)

  # Try matching as a short alias (case-insensitive)
  upper <- toupper(name)
  idx <- match(upper, toupper(names(CIFTI_STRUCTURES)))
  if (!is.na(idx)) return(unname(CIFTI_STRUCTURES[idx]))

  # Try with CIFTI_STRUCTURE_ prefix removed
  stripped <- sub("^CIFTI_STRUCTURE_", "", upper)
  idx2 <- match(stripped, toupper(names(CIFTI_STRUCTURES)))
  if (!is.na(idx2)) return(unname(CIFTI_STRUCTURES[idx2]))

  # CamelCase → UPPER_SNAKE
  snake <- gsub("([a-z])([A-Z])", "\\1_\\2", name)
  snake_upper <- toupper(snake)
  idx3 <- match(snake_upper, toupper(names(CIFTI_STRUCTURES)))
  if (!is.na(idx3)) return(unname(CIFTI_STRUCTURES[idx3]))

  stop(sprintf("Unknown CIFTI brain structure: '%s'", name))
}


#' Check if a Structure is a Surface Type
#'
#' @param structure A canonical CIFTI structure string.
#' @return Logical.
#' @keywords internal
is_surface_structure <- function(structure) {
  structure %in% c(
    "CIFTI_STRUCTURE_CORTEX_LEFT",
    "CIFTI_STRUCTURE_CORTEX_RIGHT",
    "CIFTI_STRUCTURE_CORTEX",
    "CIFTI_STRUCTURE_CEREBELLUM",
    "CIFTI_STRUCTURE_CEREBELLUM_LEFT",
    "CIFTI_STRUCTURE_CEREBELLUM_RIGHT"
  )
}

#' IJK Voxel Indices to Linear Index
#'
#' Converts 0-based IJK voxel coordinates to a 1-based linear index
#' given volume dimensions.
#'
#' @param ijk An integer matrix (N x 3) of 0-based IJK coordinates.
#' @param vol_dims An integer vector of length 3 (volume dimensions).
#' @return An integer vector of 1-based linear indices.
#' @keywords internal
ijk_to_linear <- function(ijk, vol_dims) {
  if (is.null(ijk) || nrow(ijk) == 0L) return(integer(0))
  # R uses column-major (Fortran) order: i + j*ni + k*ni*nj + 1
  as.integer(ijk[, 1] + ijk[, 2] * vol_dims[1] +
               ijk[, 3] * vol_dims[1] * vol_dims[2]) + 1L
}

#' Linear Index to IJK Voxel Indices
#'
#' Converts 1-based linear index to 0-based IJK voxel coordinates.
#'
#' @param idx An integer vector of 1-based linear indices.
#' @param vol_dims An integer vector of length 3.
#' @return An integer matrix (N x 3) of 0-based IJK coordinates.
#' @keywords internal
linear_to_ijk <- function(idx, vol_dims) {
  idx0 <- idx - 1L
  i <- idx0 %% vol_dims[1]
  j <- (idx0 %/% vol_dims[1]) %% vol_dims[2]
  k <- idx0 %/% (vol_dims[1] * vol_dims[2])
  cbind(i = as.integer(i), j = as.integer(j), k = as.integer(k))
}

#' Pad Bytes to 16-byte Boundary
#'
#' @param n Number of bytes.
#' @return The next multiple of 16 >= n.
#' @keywords internal
pad_to_16 <- function(n) {
  remainder <- n %% 16L
  if (remainder == 0L) n else n + (16L - remainder)
}
