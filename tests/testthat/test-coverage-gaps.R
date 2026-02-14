# Tests targeting coverage gaps in axis conversion, XML round-trips,
# and untested generics/branches.

# ============================================================================
# Parcels axis ↔ header round-trip (covers .mim_to_parcels_axis,
# .parcels_axis_to_mim, .build_parcel, etc.)
# ============================================================================

test_that("ParcelsAxis round-trips through header conversion", {
  bm1 <- brain_model_from_surface(c(10L, 20L), nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(c(5L, 15L), nvertex = 80, name = "cortex_right")
  pa <- parcels_from_brain_models(list(Region1 = bm1, Region2 = bm2))

  sa <- new("SeriesAxis", start = 0, step = 1, size = 5L, unit = "SECOND")

  # Build header from parcels + series axes

  hdr <- axes_to_header(sa, pa)
  expect_s4_class(hdr, "CiftiHeader")

  # Convert back to axes
  axes <- header_to_axes(hdr)
  expect_s4_class(axes$col_axis, "ParcelsAxis")
  expect_equal(axes$col_axis@parcel_name, c("Region1", "Region2"))
  expect_equal(length(axes$col_axis), 2L)
})

test_that("ParcelsAxis with volume voxels round-trips through header", {
  # Create a volume-based brain model for parcels
  mask <- array(FALSE, dim = c(5, 5, 5))
  mask[1, 2, 3] <- TRUE
  mask[3, 4, 5] <- TRUE
  bm_vol <- brain_model_from_mask(mask, "thalamus_left", affine = diag(4))

  bm_surf <- brain_model_from_surface(c(0L, 1L), nvertex = 100, name = "cortex_left")

  pa <- parcels_from_brain_models(list(ThalParcel = bm_vol, CortexParcel = bm_surf))

  sa <- new("SeriesAxis", start = 0, step = 0.5, size = 3L, unit = "SECOND")
  hdr <- axes_to_header(sa, pa)
  axes <- header_to_axes(hdr)

  expect_s4_class(axes$col_axis, "ParcelsAxis")
  expect_equal(axes$col_axis@parcel_name, c("ThalParcel", "CortexParcel"))
})

test_that("ParcelsAxis XML round-trip preserves structure", {
  # Build parcels header, convert to XML, re-parse
  bm1 <- brain_model_from_surface(c(10L, 20L), nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(MyParcel = bm1))
  sa <- new("ScalarAxis", name = "stat1", meta = list(list()))

  hdr <- axes_to_header(sa, pa)
  xml_str <- build_cifti_xml(hdr)
  hdr2 <- parse_cifti_xml(xml_str)
  axes <- header_to_axes(hdr2)

  expect_s4_class(axes$col_axis, "ParcelsAxis")
  expect_equal(axes$col_axis@parcel_name, "MyParcel")
})

# ============================================================================
# Volume BrainModel through axis conversion
# ============================================================================

test_that("BrainModelAxis with voxels round-trips through header", {
  mask <- array(FALSE, dim = c(4, 4, 4))
  mask[1, 1, 1] <- TRUE
  mask[2, 3, 4] <- TRUE
  mask[4, 4, 4] <- TRUE
  aff <- diag(4)
  aff[1, 4] <- -10  # Add some offset

  bm <- brain_model_from_mask(mask, "thalamus_left", affine = aff)
  sa <- new("SeriesAxis", start = 0, step = 1, size = 5L, unit = "SECOND")

  hdr <- axes_to_header(sa, bm)
  axes <- header_to_axes(hdr)

  expect_s4_class(axes$col_axis, "BrainModelAxis")
  expect_equal(length(axes$col_axis), 3L)
  expect_false(any(axes$col_axis@surface_mask))
  expect_equal(axes$col_axis@volume_shape, c(4L, 4L, 4L))
  expect_equal(axes$col_axis@affine[1, 4], -10)
})

test_that("Mixed surface + volume BrainModelAxis round-trips", {
  bm_surf <- brain_model_from_surface(0:2, nvertex = 100, name = "cortex_left")

  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  bm_vol <- brain_model_from_mask(mask, "thalamus_left", affine = diag(4))

  combined <- c(bm_surf, bm_vol)
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")

  hdr <- axes_to_header(sa, combined)
  xml_str <- build_cifti_xml(hdr)
  hdr2 <- parse_cifti_xml(xml_str)
  axes <- header_to_axes(hdr2)

  bm2 <- axes$col_axis
  expect_equal(length(bm2), 4L)
  expect_equal(sum(bm2@surface_mask), 3L)
  expect_equal(sum(!bm2@surface_mask), 1L)
})

# ============================================================================
# XML metadata parsing/building
# ============================================================================

test_that("XML metadata round-trips through build/parse", {
  # Build a header with matrix-level metadata
  bm <- brain_model_from_surface(0:2, nvertex = 50, name = "cortex_left")
  sa <- new("ScalarAxis", name = "mymap",
            meta = list(list(Description = "test value")))
  hdr <- axes_to_header(sa, bm)
  hdr@matrix@metadata <- list(Provenance = "ciftier test", Version = "1.0")

  xml_str <- build_cifti_xml(hdr)
  hdr2 <- parse_cifti_xml(xml_str)
  expect_equal(hdr2@matrix@metadata$Provenance, "ciftier test")
  expect_equal(hdr2@matrix@metadata$Version, "1.0")
})

test_that("NamedMap metadata round-trips", {
  nm <- new("CiftiNamedMap",
    map_name = "test",
    metadata = list(key1 = "val1", key2 = "val2")
  )

  mim <- new("CiftiMatrixIndicesMap",
    applies_to_dimension = 0L,
    index_type = "CIFTI_INDEX_TYPE_SCALARS",
    named_maps = list(nm)
  )

  hdr <- new("CiftiHeader",
    version = "2",
    matrix = new("CiftiMatrix", maps = list(
      mim,
      new("CiftiMatrixIndicesMap",
        applies_to_dimension = 1L,
        index_type = "CIFTI_INDEX_TYPE_BRAIN_MODELS",
        brain_models = list(
          new("CiftiBrainModel",
            index_offset = 0L, index_count = 2L,
            model_type = "CIFTI_MODEL_TYPE_SURFACE",
            brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT",
            surface_number_of_vertices = 50L,
            vertex_indices = c(0L, 1L),
            voxel_indices_ijk = matrix(integer(0), ncol = 3)
          )
        )
      )
    ))
  )

  xml_str <- build_cifti_xml(hdr)
  hdr2 <- parse_cifti_xml(xml_str)

  nm2 <- hdr2@matrix@maps[[1]]@named_maps[[1]]
  expect_equal(nm2@metadata$key1, "val1")
  expect_equal(nm2@metadata$key2, "val2")
})

# ============================================================================
# Untested generics: brain_models, named_maps, volume_info
# (these need methods — currently only generics exist)
# ============================================================================

test_that("brain_models generic is defined", {
  expect_true(isGeneric("brain_models"))
})

test_that("named_maps generic is defined", {
  expect_true(isGeneric("named_maps"))
})

test_that("volume_info generic is defined", {
  expect_true(isGeneric("volume_info"))
})

# ============================================================================
# Error paths
# ============================================================================

test_that("parse_cifti_xml errors on bad input type", {
  expect_error(parse_cifti_xml(42), "must be raw, character, or xml2 document")
})

test_that("parse_cifti_xml errors on wrong root element", {
  bad_xml <- '<?xml version="1.0"?><NotCIFTI/>'
  expect_error(parse_cifti_xml(bad_xml), "Expected root element")
})

test_that("parse_cifti_xml errors on missing Matrix", {
  bad_xml <- '<?xml version="1.0"?><CIFTI Version="2"></CIFTI>'
  expect_error(parse_cifti_xml(bad_xml), "No <Matrix> element")
})

test_that("brain_model_from_mask errors without affine for volume", {
  mask <- array(TRUE, dim = c(2, 2, 2))
  expect_error(brain_model_from_mask(mask, "thalamus_left"), "affine required")
})

test_that("brain_model_from_mask errors on bad input", {
  expect_error(brain_model_from_mask("bad", "cortex_left"),
               "must be a logical vector")
})

test_that("to_cifti_structure_name errors on bad input", {
  expect_error(to_cifti_structure_name("totally_bogus_name"))
})

test_that("write_cifti errors on NULL data", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  img <- CiftiImage(data = NULL, row_axis = sa, col_axis = bm, intent = 3002L)
  expect_error(write_cifti(img, tempfile()), "is not TRUE")
})

# ============================================================================
# parse_cifti_xml from raw bytes
# ============================================================================

test_that("parse_cifti_xml handles raw input with trailing nulls", {
  xml_str <- '<?xml version="1.0"?>
<CIFTI Version="2">
  <Matrix>
    <MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES"
                      NumberOfSeriesPoints="10" SeriesStart="0" SeriesStep="1" SeriesUnit="SECOND"/>
  </Matrix>
</CIFTI>'
  raw_bytes <- c(charToRaw(xml_str), raw(20))  # pad with nulls
  hdr <- parse_cifti_xml(raw_bytes)
  expect_s4_class(hdr, "CiftiHeader")
  expect_equal(hdr@matrix@maps[[1]]@number_of_series_points, 10L)
})

# ============================================================================
# Show methods with many items (> 5 truncation)
# ============================================================================

test_that("ParcelsAxis show truncates beyond 5", {
  bms <- lapply(1:8, function(i) {
    brain_model_from_surface(as.integer(i), nvertex = 100, name = "cortex_left")
  })
  names(bms) <- paste0("Parcel", 1:8)
  pa <- parcels_from_brain_models(bms)
  expect_output(show(pa), "8 parcels")
  expect_output(show(pa), "and 3 more")
})

test_that("ScalarAxis show truncates beyond 5", {
  sc <- new("ScalarAxis",
    name = paste0("map", 1:10),
    meta = rep(list(list()), 10)
  )
  expect_output(show(sc), "10 maps")
  expect_output(show(sc), "and 5 more")
})

test_that("LabelAxis show truncates beyond 5", {
  lts <- lapply(1:7, function(i) {
    new("CiftiLabelTable", labels = list(
      new("CiftiLabel", key = 0L, label = "bg", red = 1, green = 1, blue = 1, alpha = 0)
    ))
  })
  la <- new("LabelAxis",
    name = paste0("label", 1:7),
    label = lts,
    meta = rep(list(list()), 7)
  )
  expect_output(show(la), "7 maps")
  expect_output(show(la), "and 2 more")
})

# ============================================================================
# LabelAxis round-trip through header
# ============================================================================

test_that("LabelAxis round-trips through axes_to_header/header_to_axes", {
  lt <- new("CiftiLabelTable", labels = list(
    new("CiftiLabel", key = 0L, label = "bg", red = 1, green = 1, blue = 1, alpha = 0),
    new("CiftiLabel", key = 1L, label = "area1", red = 1, green = 0, blue = 0, alpha = 1)
  ))
  la <- new("LabelAxis", name = "regions", label = list(lt), meta = list(list()))
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")

  hdr <- axes_to_header(la, bm)
  axes <- header_to_axes(hdr)

  expect_s4_class(axes$row_axis, "LabelAxis")
  expect_equal(axes$row_axis@name, "regions")
  expect_equal(axes$row_axis@label[[1]]@labels[[2]]@label, "area1")
})
