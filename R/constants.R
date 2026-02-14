#' @import methods
NULL

# ============================================================================
# CIFTI-2 Enumerations
# ============================================================================

#' CIFTI-2 Brain Structure Names
#'
#' Named character vector of valid CIFTI-2 brain structure identifiers.
#' Names are short aliases; values are the full CIFTI XML strings.
#'
#' @export
CIFTI_STRUCTURES <- c(
  CORTEX_LEFT              = "CIFTI_STRUCTURE_CORTEX_LEFT",
  CORTEX_RIGHT             = "CIFTI_STRUCTURE_CORTEX_RIGHT",
  CEREBELLUM               = "CIFTI_STRUCTURE_CEREBELLUM",
  ACCUMBENS_LEFT           = "CIFTI_STRUCTURE_ACCUMBENS_LEFT",
  ACCUMBENS_RIGHT          = "CIFTI_STRUCTURE_ACCUMBENS_RIGHT",
  ALL                      = "CIFTI_STRUCTURE_ALL",
  ALL_WHITE_MATTER         = "CIFTI_STRUCTURE_ALL_WHITE_MATTER",
  ALL_GREY_MATTER          = "CIFTI_STRUCTURE_ALL_GREY_MATTER",
  AMYGDALA_LEFT            = "CIFTI_STRUCTURE_AMYGDALA_LEFT",
  AMYGDALA_RIGHT           = "CIFTI_STRUCTURE_AMYGDALA_RIGHT",
  BRAIN_STEM               = "CIFTI_STRUCTURE_BRAIN_STEM",
  CAUDATE_LEFT             = "CIFTI_STRUCTURE_CAUDATE_LEFT",
  CAUDATE_RIGHT            = "CIFTI_STRUCTURE_CAUDATE_RIGHT",
  CEREBELLAR_WHITE_MATTER_LEFT  = "CIFTI_STRUCTURE_CEREBELLAR_WHITE_MATTER_LEFT",
  CEREBELLAR_WHITE_MATTER_RIGHT = "CIFTI_STRUCTURE_CEREBELLAR_WHITE_MATTER_RIGHT",
  CEREBELLUM_LEFT          = "CIFTI_STRUCTURE_CEREBELLUM_LEFT",
  CEREBELLUM_RIGHT         = "CIFTI_STRUCTURE_CEREBELLUM_RIGHT",
  CEREBRAL_WHITE_MATTER_LEFT    = "CIFTI_STRUCTURE_CEREBRAL_WHITE_MATTER_LEFT",
  CEREBRAL_WHITE_MATTER_RIGHT   = "CIFTI_STRUCTURE_CEREBRAL_WHITE_MATTER_RIGHT",
  CORTEX                   = "CIFTI_STRUCTURE_CORTEX",
  DIENCEPHALON_VENTRAL_LEFT     = "CIFTI_STRUCTURE_DIENCEPHALON_VENTRAL_LEFT",
  DIENCEPHALON_VENTRAL_RIGHT    = "CIFTI_STRUCTURE_DIENCEPHALON_VENTRAL_RIGHT",
  HIPPOCAMPUS_LEFT         = "CIFTI_STRUCTURE_HIPPOCAMPUS_LEFT",
  HIPPOCAMPUS_RIGHT        = "CIFTI_STRUCTURE_HIPPOCAMPUS_RIGHT",
  OTHER                    = "CIFTI_STRUCTURE_OTHER",
  OTHER_GREY_MATTER        = "CIFTI_STRUCTURE_OTHER_GREY_MATTER",
  OTHER_WHITE_MATTER       = "CIFTI_STRUCTURE_OTHER_WHITE_MATTER",
  PALLIDUM_LEFT            = "CIFTI_STRUCTURE_PALLIDUM_LEFT",
  PALLIDUM_RIGHT           = "CIFTI_STRUCTURE_PALLIDUM_RIGHT",
  PUTAMEN_LEFT             = "CIFTI_STRUCTURE_PUTAMEN_LEFT",
  PUTAMEN_RIGHT            = "CIFTI_STRUCTURE_PUTAMEN_RIGHT",
  THALAMUS_LEFT            = "CIFTI_STRUCTURE_THALAMUS_LEFT",
  THALAMUS_RIGHT           = "CIFTI_STRUCTURE_THALAMUS_RIGHT"
)

#' CIFTI-2 Brain Model Types
#'
#' @export
CIFTI_MODEL_TYPES <- c(
  SURFACE = "CIFTI_MODEL_TYPE_SURFACE",
  VOXELS  = "CIFTI_MODEL_TYPE_VOXELS"
)

#' CIFTI-2 Index Types
#'
#' Identifies what kind of data a dimension contains.
#'
#' @export
CIFTI_INDEX_TYPES <- c(
  BRAIN_MODELS = "CIFTI_INDEX_TYPE_BRAIN_MODELS",
  PARCELS      = "CIFTI_INDEX_TYPE_PARCELS",
  SERIES       = "CIFTI_INDEX_TYPE_SERIES",
  SCALARS      = "CIFTI_INDEX_TYPE_SCALARS",
  LABELS       = "CIFTI_INDEX_TYPE_LABELS"
)

#' CIFTI-2 NIfTI Intent Codes
#'
#' NIfTI intent codes used by CIFTI-2 files.
#'
#' @export
CIFTI_INTENTS <- c(
  ConnDenseTimeSeries  = 3002L,
  ConnDense            = 3001L,
  ConnDenseScalar      = 3006L,
  ConnDenseLabel       = 3007L,
  ConnDenseParcel      = 3010L,
  ConnParcels          = 3003L,
  ConnParcelTimeSeries = 3004L,
  ConnParcelScalar     = 3008L,
  ConnParcelSeries     = 3009L,
  ConnParcelLabel      = 3012L,
  ConnParcelDense      = 3011L
)

#' CIFTI-2 Series Unit Types
#'
#' @export
CIFTI_SERIES_UNITS <- c(
  SECOND      = "SECOND",
  HERTZ       = "HERTZ",
  METER       = "METER",
  RADIAN      = "RADIAN"
)

#' NIfTI-2 Data Type Codes
#'
#' Numeric type codes for NIfTI-2 binary data.
#'
#' @keywords internal
NIFTI2_DTYPES <- c(
  FLOAT32 = 16L,
  FLOAT64 = 64L,
  INT32   = 8L,
  INT8    = 256L,
  INT16   = 4L
)

#' Size in bytes for NIfTI-2 data types
#'
#' @keywords internal
NIFTI2_DTYPE_SIZES <- c(
  "16"  = 4L,
  "64"  = 8L,
  "8"   = 4L,
  "256" = 1L,
  "4"   = 2L
)

#' NIfTI-2 header size in bytes
#'
#' @keywords internal
NIFTI2_HEADER_SIZE <- 540L
