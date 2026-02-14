#' @include all_generic.R
#' @import methods
NULL

# ============================================================================
# Layer 0: XML Data Classes
# ============================================================================

#' CiftiLabel Class
#'
#' A single label entry in a CIFTI label table.
#'
#' @slot key Integer key value.
#' @slot label Character label string.
#' @slot red Numeric red channel (0-1).
#' @slot green Numeric green channel (0-1).
#' @slot blue Numeric blue channel (0-1).
#' @slot alpha Numeric alpha channel (0-1).
#'
#' @export
setClass("CiftiLabel",
  slots = list(
    key   = "integer",
    label = "character",
    red   = "numeric",
    green = "numeric",
    blue  = "numeric",
    alpha = "numeric"
  ),
  prototype = list(
    key = 0L, label = "", red = 1, green = 1, blue = 1, alpha = 1
  ),
  validity = function(object) {
    errors <- character()
    for (ch in c("red", "green", "blue", "alpha")) {
      val <- slot(object, ch)
      if (length(val) != 1 || is.na(val) || val < 0 || val > 1) {
        errors <- c(errors, sprintf("'%s' must be a single number in [0, 1]", ch))
      }
    }
    if (length(errors)) errors else TRUE
  }
)


#' CiftiLabelTable Class
#'
#' A table of label entries for a named map.
#'
#' @slot labels A list of \code{\linkS4class{CiftiLabel}} objects.
#'
#' @export
setClass("CiftiLabelTable",
  slots = list(labels = "list"),
  prototype = list(labels = list()),
  validity = function(object) {
    if (length(object@labels) > 0) {
      ok <- vapply(object@labels, is, logical(1), "CiftiLabel")
      if (!all(ok)) return("All elements of 'labels' must be CiftiLabel objects")
    }
    TRUE
  }
)


#' CiftiNamedMap Class
#'
#' One named map entry (used in SCALARS and LABELS index types).
#'
#' @slot map_name Character string naming the map.
#' @slot label_table A \code{\linkS4class{CiftiLabelTable}} or NULL.
#' @slot metadata A named list of metadata key-value pairs.
#'
#' @export
setClass("CiftiNamedMap",
  slots = list(
    map_name    = "character",
    label_table = "CiftiLabelTable",
    metadata    = "list"
  ),
  prototype = list(
    map_name = "",
    label_table = new("CiftiLabelTable"),
    metadata = list()
  )
)


#' CiftiVolume Class
#'
#' Volume geometry reference (dimensions and affine transform).
#'
#' @slot dimensions Integer vector of length 3 (I, J, K).
#' @slot transform A 4x4 numeric matrix (voxel-to-world affine).
#'
#' @export
setClass("CiftiVolume",
  slots = list(
    dimensions = "integer",
    transform  = "matrix"
  ),
  prototype = list(
    dimensions = integer(3),
    transform  = diag(4)
  ),
  validity = function(object) {
    errors <- character()
    if (length(object@dimensions) != 3L) {
      errors <- c(errors, "'dimensions' must have length 3")
    }
    d <- dim(object@transform)
    if (!identical(d, c(4L, 4L))) {
      errors <- c(errors, "'transform' must be a 4x4 matrix")
    }
    if (length(errors)) errors else TRUE
  }
)


#' CiftiBrainModel Class
#'
#' Describes one brain structure's mapping in a BrainModels index map.
#' Indices stored here follow CIFTI conventions (0-based).
#'
#' @slot index_offset Integer offset into the matrix dimension.
#' @slot index_count Integer number of indices for this model.
#' @slot model_type Character: "CIFTI_MODEL_TYPE_SURFACE" or "CIFTI_MODEL_TYPE_VOXELS".
#' @slot brain_structure Character: canonical CIFTI structure string.
#' @slot surface_number_of_vertices Integer: total vertices on the surface (0 for voxel models).
#' @slot vertex_indices Integer vector of 0-based vertex indices (surface models).
#' @slot voxel_indices_ijk Integer matrix (N x 3) of 0-based IJK voxel indices (voxel models).
#'
#' @export
setClass("CiftiBrainModel",
  slots = list(
    index_offset               = "integer",
    index_count                = "integer",
    model_type                 = "character",
    brain_structure            = "character",
    surface_number_of_vertices = "integer",
    vertex_indices             = "integer",
    voxel_indices_ijk          = "matrix"
  ),
  prototype = list(
    index_offset = 0L,
    index_count  = 0L,
    model_type   = "CIFTI_MODEL_TYPE_SURFACE",
    brain_structure = "",
    surface_number_of_vertices = 0L,
    vertex_indices = integer(0),
    voxel_indices_ijk = matrix(integer(0), ncol = 3)
  ),
  validity = function(object) {
    errors <- character()
    if (!(object@model_type %in% CIFTI_MODEL_TYPES)) {
      errors <- c(errors, sprintf("Invalid model_type: '%s'", object@model_type))
    }
    if (object@model_type == "CIFTI_MODEL_TYPE_SURFACE") {
      if (length(object@vertex_indices) != object@index_count) {
        errors <- c(errors, "Length of vertex_indices must equal index_count for surface models")
      }
    } else {
      if (nrow(object@voxel_indices_ijk) != object@index_count) {
        errors <- c(errors, "Rows of voxel_indices_ijk must equal index_count for voxel models")
      }
    }
    if (length(errors)) errors else TRUE
  }
)


#' CiftiParcel Class
#'
#' One parcel in a PARCELS index map.
#'
#' @slot name Character parcel name.
#' @slot surface_vertices A named list mapping structure names to integer vectors
#'   of 0-based vertex indices.
#' @slot voxel_indices_ijk Integer matrix (N x 3) of 0-based IJK voxel indices.
#'
#' @export
setClass("CiftiParcel",
  slots = list(
    name              = "character",
    surface_vertices  = "list",
    voxel_indices_ijk = "matrix"
  ),
  prototype = list(
    name = "",
    surface_vertices = list(),
    voxel_indices_ijk = matrix(integer(0), ncol = 3)
  )
)


#' CiftiSurface Class
#'
#' Surface metadata for a PARCELS index map.
#'
#' @slot brain_structure Character: canonical CIFTI structure string.
#' @slot number_of_vertices Integer: total vertices on the surface.
#'
#' @export
setClass("CiftiSurface",
  slots = list(
    brain_structure     = "character",
    number_of_vertices  = "integer"
  ),
  prototype = list(
    brain_structure = "",
    number_of_vertices = 0L
  )
)


#' CiftiMatrixIndicesMap Class
#'
#' Describes one dimension of the CIFTI data matrix.
#'
#' @slot applies_to_dimension Integer vector (0-based dimension indices this map applies to).
#' @slot index_type Character: one of the CIFTI_INDEX_TYPES values.
#' @slot brain_models List of \code{\linkS4class{CiftiBrainModel}} objects.
#' @slot named_maps List of \code{\linkS4class{CiftiNamedMap}} objects.
#' @slot parcels List of \code{\linkS4class{CiftiParcel}} objects.
#' @slot surfaces List of \code{\linkS4class{CiftiSurface}} objects.
#' @slot volume A \code{\linkS4class{CiftiVolume}} or NULL.
#' @slot series_start Numeric start value for SERIES type.
#' @slot series_step Numeric step value for SERIES type.
#' @slot series_unit Character unit for SERIES type.
#' @slot number_of_series_points Integer count for SERIES type.
#'
#' @export
setClass("CiftiMatrixIndicesMap",
  slots = list(
    applies_to_dimension   = "integer",
    index_type             = "character",
    brain_models           = "list",
    named_maps             = "list",
    parcels                = "list",
    surfaces               = "list",
    volume                 = "ANY",
    series_start           = "numeric",
    series_step            = "numeric",
    series_unit            = "character",
    number_of_series_points = "integer"
  ),
  prototype = list(
    applies_to_dimension = integer(0),
    index_type = "",
    brain_models = list(),
    named_maps = list(),
    parcels = list(),
    surfaces = list(),
    volume = NULL,
    series_start = 0,
    series_step = 1,
    series_unit = "SECOND",
    number_of_series_points = 0L
  ),
  validity = function(object) {
    if (nchar(object@index_type) > 0 && !(object@index_type %in% CIFTI_INDEX_TYPES)) {
      return(sprintf("Invalid index_type: '%s'", object@index_type))
    }
    TRUE
  }
)


#' CiftiMatrix Class
#'
#' Complete matrix metadata — contains all dimension descriptors.
#'
#' @slot maps A list of \code{\linkS4class{CiftiMatrixIndicesMap}} objects.
#' @slot metadata A named list of metadata key-value pairs.
#'
#' @export
setClass("CiftiMatrix",
  slots = list(
    maps     = "list",
    metadata = "list"
  ),
  prototype = list(
    maps = list(),
    metadata = list()
  )
)


#' CiftiHeader Class
#'
#' Top-level parsed CIFTI-2 XML header.
#'
#' @slot version Character string (e.g., "2").
#' @slot matrix A \code{\linkS4class{CiftiMatrix}} object.
#'
#' @export
setClass("CiftiHeader",
  slots = list(
    version = "character",
    matrix  = "CiftiMatrix"
  ),
  prototype = list(
    version = "2",
    matrix = new("CiftiMatrix")
  )
)


# ============================================================================
# Layer 1: Axis Classes
# ============================================================================

#' CiftiAxis Virtual Base Class
#'
#' All CIFTI axis types inherit from this virtual class.
#'
#' @export
setClass("CiftiAxis", contains = "VIRTUAL")


#' BrainModelAxis Class
#'
#' Describes a BRAIN_MODELS dimension — each row/col maps to a surface vertex
#' or volume voxel.
#'
#' @slot name Character vector of structure names (one per grayordinate).
#' @slot vertex Integer vector of 0-based vertex indices (NA for voxel entries).
#' @slot voxel Integer matrix (N x 3) of 0-based IJK indices (NA rows for surface entries).
#' @slot surface_mask Logical vector: TRUE for surface entries, FALSE for volume.
#' @slot nvertices Named list mapping structure names to total vertex counts.
#' @slot affine 4x4 numeric matrix (volume affine) or NULL.
#' @slot volume_shape Integer vector of length 3 or NULL.
#'
#' @export
setClass("BrainModelAxis",
  contains = "CiftiAxis",
  slots = list(
    name         = "character",
    vertex       = "integer",
    voxel        = "matrix",
    surface_mask = "logical",
    nvertices    = "list",
    affine       = "ANY",
    volume_shape = "ANY"
  ),
  prototype = list(
    name = character(0),
    vertex = integer(0),
    voxel = matrix(integer(0), ncol = 3),
    surface_mask = logical(0),
    nvertices = list(),
    affine = NULL,
    volume_shape = NULL
  )
)


#' ParcelsAxis Class
#'
#' Describes a PARCELS dimension — each row/col maps to a named parcel.
#'
#' @slot parcel_name Character vector of parcel names.
#' @slot voxels List of integer matrices (N x 3) of 0-based IJK indices per parcel.
#' @slot vertices List of named lists mapping structure names to integer vectors per parcel.
#' @slot nvertices Named list mapping structure names to total vertex counts.
#' @slot affine 4x4 numeric matrix or NULL.
#' @slot volume_shape Integer vector of length 3 or NULL.
#'
#' @export
setClass("ParcelsAxis",
  contains = "CiftiAxis",
  slots = list(
    parcel_name  = "character",
    voxels       = "list",
    vertices     = "list",
    nvertices    = "list",
    affine       = "ANY",
    volume_shape = "ANY"
  ),
  prototype = list(
    parcel_name = character(0),
    voxels = list(),
    vertices = list(),
    nvertices = list(),
    affine = NULL,
    volume_shape = NULL
  )
)


#' SeriesAxis Class
#'
#' Describes a SERIES dimension (e.g., time points).
#'
#' @slot start Numeric start value.
#' @slot step Numeric step size.
#' @slot size Integer number of points.
#' @slot unit Character unit string (one of CIFTI_SERIES_UNITS).
#'
#' @export
setClass("SeriesAxis",
  contains = "CiftiAxis",
  slots = list(
    start = "numeric",
    step  = "numeric",
    size  = "integer",
    unit  = "character"
  ),
  prototype = list(
    start = 0, step = 1, size = 0L, unit = "SECOND"
  ),
  validity = function(object) {
    if (object@size < 0L) return("'size' must be non-negative")
    TRUE
  }
)


#' ScalarAxis Class
#'
#' Describes a SCALARS dimension — each entry is a named scalar map.
#'
#' @slot name Character vector of map names.
#' @slot meta List of named lists (metadata per map).
#'
#' @export
setClass("ScalarAxis",
  contains = "CiftiAxis",
  slots = list(
    name = "character",
    meta = "list"
  ),
  prototype = list(
    name = character(0),
    meta = list()
  )
)


#' LabelAxis Class
#'
#' Describes a LABELS dimension — each entry is a named label map
#' with an associated label table.
#'
#' @slot name Character vector of map names.
#' @slot label List of \code{\linkS4class{CiftiLabelTable}} objects (one per map).
#' @slot meta List of named lists (metadata per map).
#'
#' @export
setClass("LabelAxis",
  contains = "CiftiAxis",
  slots = list(
    name  = "character",
    label = "list",
    meta  = "list"
  ),
  prototype = list(
    name = character(0),
    label = list(),
    meta = list()
  )
)


# ============================================================================
# Layer 2: CiftiImage
# ============================================================================

#' CiftiImage Class
#'
#' A complete CIFTI-2 image: data matrix + header + axis descriptors.
#'
#' @slot data A numeric matrix (or NULL for header-only reads).
#' @slot header A \code{\linkS4class{CiftiHeader}} object.
#' @slot row_axis A \code{\linkS4class{CiftiAxis}} describing rows.
#' @slot col_axis A \code{\linkS4class{CiftiAxis}} describing columns.
#' @slot intent Integer NIfTI intent code.
#' @slot file_name Character source file path (may be empty).
#'
#' @export
setClass("CiftiImage",
  slots = list(
    data      = "ANY",
    header    = "CiftiHeader",
    row_axis  = "CiftiAxis",
    col_axis  = "CiftiAxis",
    intent    = "integer",
    file_name = "character"
  ),
  prototype = list(
    data = NULL,
    header = new("CiftiHeader"),
    row_axis = new("SeriesAxis"),
    col_axis = new("SeriesAxis"),
    intent = 0L,
    file_name = ""
  )
)
