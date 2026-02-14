#' @include constants.R
NULL

# ============================================================================
# Generic Function Declarations
# ============================================================================

#' Get the Row Axis of a CIFTI Object
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return A \code{\linkS4class{CiftiAxis}} object.
#' @export
setGeneric("row_axis", function(x, ...) standardGeneric("row_axis"))

#' Get the Column Axis of a CIFTI Object
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return A \code{\linkS4class{CiftiAxis}} object.
#' @export
setGeneric("col_axis", function(x, ...) standardGeneric("col_axis"))

#' Get the CIFTI Header
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return A \code{\linkS4class{CiftiHeader}} object.
#' @export
setGeneric("cifti_header", function(x, ...) standardGeneric("cifti_header"))

#' Get the NIfTI Intent Code
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return An integer intent code.
#' @export
setGeneric("cifti_intent", function(x, ...) standardGeneric("cifti_intent"))

#' Get the Data Matrix
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return A numeric matrix.
#' @export
setGeneric("cifti_data", function(x, ...) standardGeneric("cifti_data"))

#' Iterate Over Brain Structures
#'
#' Returns a list of (structure_name, slice) pairs for each brain structure
#' in an axis.
#'
#' @param x An axis object.
#' @param ... Additional arguments.
#' @return A named list of index ranges.
#' @export
setGeneric("iter_structures", function(x, ...) standardGeneric("iter_structures"))

#' Get Brain Models from an Object
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return A list of \code{\linkS4class{CiftiBrainModel}} objects.
#' @export
setGeneric("brain_models", function(x, ...) standardGeneric("brain_models"))

#' Get Named Maps from an Object
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return A list of \code{\linkS4class{CiftiNamedMap}} objects.
#' @export
setGeneric("named_maps", function(x, ...) standardGeneric("named_maps"))

#' Get Volume Information
#'
#' @param x A CIFTI object.
#' @param ... Additional arguments.
#' @return A \code{\linkS4class{CiftiVolume}} object or NULL.
#' @export
setGeneric("volume_info", function(x, ...) standardGeneric("volume_info"))
