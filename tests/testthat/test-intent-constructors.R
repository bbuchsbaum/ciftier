# Tests for Phase 5: Parcel-based and connectivity intent constructors,
# brain_models(), named_maps(), volume_info(), extract_structure()

# ============================================================================
# Parcel-Based Intent Constructors
# ============================================================================

test_that("ptseries creates parcel timeseries CiftiImage", {
  bm1 <- brain_model_from_surface(c(10L, 20L), nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(c(5L), nvertex = 80, name = "cortex_right")
  pa <- parcels_from_brain_models(list(Region1 = bm1, Region2 = bm2))

  data <- matrix(rnorm(6), nrow = 3, ncol = 2)
  img <- ptseries(data, pa, step = 0.72)

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3004L)
  expect_s4_class(row_axis(img), "SeriesAxis")
  expect_s4_class(col_axis(img), "ParcelsAxis")
  expect_equal(row_axis(img)@step, 0.72)
  expect_equal(dim(img), c(3L, 2L))
})

test_that("pscalar creates parcel scalar CiftiImage", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))

  data <- matrix(1:2, nrow = 2, ncol = 1)
  img <- pscalar(data, pa, names = c("mean", "sd"))

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3008L)
  expect_s4_class(row_axis(img), "ScalarAxis")
  expect_equal(row_axis(img)@name, c("mean", "sd"))
})

test_that("pscalar generates default names", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))

  data <- matrix(1:3, nrow = 3, ncol = 1)
  img <- pscalar(data, pa)

  expect_equal(row_axis(img)@name, c("map_1", "map_2", "map_3"))
})

test_that("plabel creates parcel label CiftiImage", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))

  lt <- new("CiftiLabelTable", labels = list(
    new("CiftiLabel", key = 0L, label = "bg", red = 1, green = 1, blue = 1, alpha = 0),
    new("CiftiLabel", key = 1L, label = "active", red = 1, green = 0, blue = 0, alpha = 1)
  ))

  data <- matrix(c(0L, 1L), nrow = 2, ncol = 1)
  img <- plabel(data, pa, names = c("map1", "map2"), label_tables = list(lt, lt))

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3012L)
  expect_s4_class(row_axis(img), "LabelAxis")
  expect_equal(row_axis(img)@name, c("map1", "map2"))
})

test_that("plabel generates default names and tables", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))

  data <- matrix(0L, nrow = 2, ncol = 1)
  img <- plabel(data, pa)

  expect_equal(row_axis(img)@name, c("label_1", "label_2"))
  expect_length(row_axis(img)@label, 2L)
})

test_that("pcseries creates parcel series CiftiImage", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))

  data <- matrix(rnorm(5), nrow = 5, ncol = 1)
  img <- pcseries(data, pa, step = 2, unit = "HERTZ")

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3009L)
  expect_s4_class(row_axis(img), "SeriesAxis")
  expect_equal(row_axis(img)@unit, "HERTZ")
  expect_equal(row_axis(img)@step, 2)
})


# ============================================================================
# Connectivity Intent Constructors
# ============================================================================

test_that("dconn creates dense connectivity CiftiImage", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(25), nrow = 5, ncol = 5)
  img <- dconn(data, bm)

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3001L)
  expect_s4_class(row_axis(img), "BrainModelAxis")
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_equal(dim(img), c(5L, 5L))
})

test_that("dconn with asymmetric brain models", {
  bm_row <- brain_model_from_surface(0:2, nvertex = 50, name = "cortex_left")
  bm_col <- brain_model_from_surface(0:4, nvertex = 80, name = "cortex_right")
  data <- matrix(rnorm(15), nrow = 3, ncol = 5)
  img <- dconn(data, bm_row, bm_col)

  expect_equal(dim(img), c(3L, 5L))
  expect_equal(length(row_axis(img)), 3L)
  expect_equal(length(col_axis(img)), 5L)
})

test_that("pconn creates parcel connectivity CiftiImage", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(0:2, nvertex = 80, name = "cortex_right")
  pa <- parcels_from_brain_models(list(A = bm1, B = bm2))

  data <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  img <- pconn(data, pa)

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3003L)
  expect_s4_class(row_axis(img), "ParcelsAxis")
  expect_s4_class(col_axis(img), "ParcelsAxis")
})

test_that("dpconn creates dense-to-parcel connectivity", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  pa <- parcels_from_brain_models(list(
    P1 = brain_model_from_surface(0:1, nvertex = 100, name = "cortex_left"),
    P2 = brain_model_from_surface(2:4, nvertex = 100, name = "cortex_left")
  ))

  data <- matrix(rnorm(10), nrow = 5, ncol = 2)
  img <- dpconn(data, bm, pa)

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3010L)
  expect_s4_class(row_axis(img), "BrainModelAxis")
  expect_s4_class(col_axis(img), "ParcelsAxis")
})

test_that("pdconn creates parcel-to-dense connectivity", {
  pa <- parcels_from_brain_models(list(
    P1 = brain_model_from_surface(0:1, nvertex = 100, name = "cortex_left")
  ))
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")

  data <- matrix(rnorm(5), nrow = 1, ncol = 5)
  img <- pdconn(data, pa, bm)

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3011L)
  expect_s4_class(row_axis(img), "ParcelsAxis")
  expect_s4_class(col_axis(img), "BrainModelAxis")
})


# ============================================================================
# brain_models(), named_maps(), volume_info()
# ============================================================================

test_that("brain_models() returns brain model objects from dtseries", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  bms <- brain_models(img)
  expect_true(is.list(bms))
  expect_length(bms, 1L)
  expect_s4_class(bms[[1]], "CiftiBrainModel")
  expect_equal(bms[[1]]@brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(bms[[1]]@index_count, 5L)
})

test_that("brain_models() returns multiple structures", {
  bm_l <- brain_model_from_surface(0:2, nvertex = 50, name = "cortex_left")
  bm_r <- brain_model_from_surface(0:1, nvertex = 50, name = "cortex_right")
  bm <- c(bm_l, bm_r)

  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  bms <- brain_models(img)
  expect_length(bms, 2L)
  expect_equal(bms[[1]]@brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(bms[[2]]@brain_structure, "CIFTI_STRUCTURE_CORTEX_RIGHT")
})

test_that("brain_models() returns empty list for non-brain-model intents", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))
  data <- matrix(rnorm(5), nrow = 5, ncol = 1)
  img <- ptseries(data, pa)

  bms <- brain_models(img)
  expect_true(is.list(bms))
  expect_length(bms, 0L)
})

test_that("named_maps() returns named maps from dscalar", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dscalar(data, bm, names = c("thickness", "curvature"))

  nms <- named_maps(img)
  expect_true(is.list(nms))
  expect_length(nms, 2L)
  expect_s4_class(nms[[1]], "CiftiNamedMap")
  expect_equal(nms[[1]]@map_name, "thickness")
  expect_equal(nms[[2]]@map_name, "curvature")
})

test_that("named_maps() returns maps from dlabel", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(0L, nrow = 1, ncol = 5)
  img <- dlabel(data, bm, names = "parcellation")

  nms <- named_maps(img)
  expect_length(nms, 1L)
  expect_equal(nms[[1]]@map_name, "parcellation")
})

test_that("named_maps() returns empty list for dtseries", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  nms <- named_maps(img)
  expect_length(nms, 0L)
})

test_that("volume_info() returns CiftiVolume for volume data", {
  mask <- array(FALSE, dim = c(4, 4, 4))
  mask[1, 1, 1] <- TRUE
  mask[2, 3, 4] <- TRUE
  aff <- diag(4)
  aff[1, 4] <- -10

  bm <- brain_model_from_mask(mask, "thalamus_left", affine = aff)
  data <- matrix(rnorm(4), nrow = 2, ncol = 2)
  img <- dtseries(data, bm)

  vi <- volume_info(img)
  expect_s4_class(vi, "CiftiVolume")
  expect_equal(vi@dimensions, c(4L, 4L, 4L))
  expect_equal(vi@transform[1, 4], -10)
})

test_that("volume_info() returns NULL for surface-only data", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  vi <- volume_info(img)
  expect_null(vi)
})


# ============================================================================
# extract_structure()
# ============================================================================

test_that("extract_structure extracts cortex_left from BrainModelAxis", {
  bm_l <- brain_model_from_surface(0:2, nvertex = 50, name = "cortex_left")
  bm_r <- brain_model_from_surface(0:1, nvertex = 50, name = "cortex_right")
  bm <- c(bm_l, bm_r)

  data <- matrix(1:10, nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  es <- extract_structure(img, "cortex_left")
  expect_equal(ncol(es$data), 3L)
  expect_equal(es$indices, 1:3)
  expect_s4_class(es$axis, "BrainModelAxis")
  expect_equal(length(es$axis), 3L)
  expect_true(all(es$axis@name == "CIFTI_STRUCTURE_CORTEX_LEFT"))
})

test_that("extract_structure extracts cortex_right", {
  bm_l <- brain_model_from_surface(0:2, nvertex = 50, name = "cortex_left")
  bm_r <- brain_model_from_surface(0:1, nvertex = 50, name = "cortex_right")
  bm <- c(bm_l, bm_r)

  data <- matrix(1:10, nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  es <- extract_structure(img, "cortex_right")
  expect_equal(ncol(es$data), 2L)
  expect_equal(es$indices, 4:5)
  expect_equal(es$data, data[, 4:5, drop = FALSE])
})

test_that("extract_structure works with CamelCase name", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  es <- extract_structure(img, "CortexLeft")
  expect_equal(ncol(es$data), 5L)
})

test_that("extract_structure errors on missing structure", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  expect_error(extract_structure(img, "cortex_right"), "not found")
})

test_that("extract_structure works with ParcelsAxis", {
  bm1 <- brain_model_from_surface(c(10L, 20L), nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(c(5L), nvertex = 80, name = "cortex_right")
  pa <- parcels_from_brain_models(list(RegionA = bm1, RegionB = bm2))

  data <- matrix(1:6, nrow = 3, ncol = 2)
  img <- ptseries(data, pa)

  es <- extract_structure(img, "cortex_left")
  expect_equal(ncol(es$data), 1L)
  expect_equal(es$indices, 1L)
  expect_equal(es$axis@parcel_name, "RegionA")
})

test_that("extract_structure with NULL data returns NULL data", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  img <- CiftiImage(data = NULL, row_axis = sa, col_axis = bm)

  es <- extract_structure(img, "cortex_left")
  expect_null(es$data)
  expect_equal(length(es$axis), 5L)
})

test_that("extract_structure errors on non-spatial axis", {
  sa1 <- new("SeriesAxis", start = 0, step = 1, size = 5L, unit = "SECOND")
  sa2 <- new("SeriesAxis", start = 0, step = 1, size = 3L, unit = "SECOND")
  img <- CiftiImage(data = matrix(0, 5, 3), row_axis = sa1, col_axis = sa2)

  expect_error(extract_structure(img, "cortex_left"),
               "requires a BrainModelAxis or ParcelsAxis")
})


# ============================================================================
# Write/read round-trip for new intent types
# ============================================================================

test_that("ptseries write/read round-trip", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))
  data <- matrix(rnorm(5), nrow = 5, ncol = 1)

  img <- ptseries(data, pa, step = 0.72)

  tmp <- tempfile(fileext = ".ptseries.nii")
  on.exit(unlink(tmp))

  write_cifti(img, tmp)
  img2 <- read_cifti(tmp)

  expect_equal(cifti_intent(img2), 3004L)
  expect_s4_class(row_axis(img2), "SeriesAxis")
  expect_s4_class(col_axis(img2), "ParcelsAxis")
  expect_equal(col_axis(img2)@parcel_name, "P1")
  expect_equal(cifti_data(img2), data, tolerance = 1e-6)
})

test_that("dconn write/read round-trip", {
  bm <- brain_model_from_surface(0:2, nvertex = 50, name = "cortex_left")
  data <- matrix(c(1, 0.5, 0.3, 0.5, 1, 0.7, 0.3, 0.7, 1), nrow = 3, ncol = 3)

  img <- dconn(data, bm)

  tmp <- tempfile(fileext = ".dconn.nii")
  on.exit(unlink(tmp))

  write_cifti(img, tmp)
  img2 <- read_cifti(tmp)

  expect_equal(cifti_intent(img2), 3001L)
  expect_s4_class(row_axis(img2), "BrainModelAxis")
  expect_s4_class(col_axis(img2), "BrainModelAxis")
  expect_equal(cifti_data(img2), data, tolerance = 1e-6)
})

test_that("pconn write/read round-trip", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(0:2, nvertex = 80, name = "cortex_right")
  pa <- parcels_from_brain_models(list(A = bm1, B = bm2))
  data <- matrix(c(1, 0.8, 0.8, 1), nrow = 2, ncol = 2)

  img <- pconn(data, pa)

  tmp <- tempfile(fileext = ".pconn.nii")
  on.exit(unlink(tmp))

  write_cifti(img, tmp)
  img2 <- read_cifti(tmp)

  expect_equal(cifti_intent(img2), 3003L)
  expect_equal(cifti_data(img2), data, tolerance = 1e-6)
})
