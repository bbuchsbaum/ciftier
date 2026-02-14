test_that("BrainModelAxis show method works", {
  bm <- brain_model_from_surface(0:2, nvertex = 100, name = "cortex_left")
  expect_output(show(bm), "BrainModelAxis")
  expect_output(show(bm), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_output(show(bm), "3 / 100 vertices")
})

test_that("BrainModelAxis show with volume entries", {
  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  mask[2, 2, 2] <- TRUE
  bm <- brain_model_from_mask(mask, "thalamus_left", affine = diag(4))
  expect_output(show(bm), "BrainModelAxis")
  expect_output(show(bm), "2 voxels")
  expect_output(show(bm), "Volume: 3 x 3 x 3")
})

test_that("ParcelsAxis show method works", {
  bm1 <- brain_model_from_surface(c(10L, 20L), nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(c(5L), nvertex = 80, name = "cortex_right")
  pa <- parcels_from_brain_models(list(FrontalPole = bm1, V1 = bm2))
  expect_output(show(pa), "ParcelsAxis")
  expect_output(show(pa), "2 parcels")
  expect_output(show(pa), "FrontalPole")
})

test_that("SeriesAxis show method works", {
  sa <- new("SeriesAxis", start = 0, step = 0.72, size = 100L, unit = "SECOND")
  expect_output(show(sa), "SeriesAxis")
  expect_output(show(sa), "100 points")
  expect_output(show(sa), "0.72")
})

test_that("ScalarAxis show method works", {
  sc <- new("ScalarAxis", name = c("thickness", "curvature"), meta = list(list(), list()))
  expect_output(show(sc), "ScalarAxis")
  expect_output(show(sc), "2 maps")
  expect_output(show(sc), "thickness")
})

test_that("LabelAxis show method works", {
  lt <- new("CiftiLabelTable", labels = list(
    new("CiftiLabel", key = 0L, label = "bg", red = 1, green = 1, blue = 1, alpha = 0)
  ))
  la <- new("LabelAxis", name = "parcellation", label = list(lt), meta = list(list()))
  expect_output(show(la), "LabelAxis")
  expect_output(show(la), "1 maps")
  expect_output(show(la), "1 labels")
})

test_that("CiftiImage show method works", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  img <- dtseries(matrix(1:10, nrow = 2, ncol = 5), bm, step = 0.72)
  expect_output(show(img), "CiftiImage")
  expect_output(show(img), "3002")
  expect_output(show(img), "2 x 5")
})

test_that("CiftiImage show with no data", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  img <- CiftiImage(data = NULL, row_axis = sa, col_axis = bm)
  expect_output(show(img), "not loaded")
})

test_that("CiftiImage accessor generics work", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data_mat <- matrix(1:10, nrow = 2, ncol = 5)
  img <- dtseries(data_mat, bm, step = 0.72)

  expect_s4_class(row_axis(img), "SeriesAxis")
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_s4_class(cifti_header(img), "CiftiHeader")
  expect_equal(cifti_intent(img), 3002L)
  expect_equal(cifti_data(img), data_mat)
  expect_equal(dim(img), c(2L, 5L))
})

test_that("CiftiImage dim with NULL data uses axis lengths", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  img <- CiftiImage(data = NULL, row_axis = sa, col_axis = bm)
  expect_equal(dim(img), c(10L, 5L))
})

test_that("CiftiImage subsetting: rows only", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data_mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  img <- dtseries(data_mat, bm)

  sub <- img[2:3, ]
  expect_equal(dim(sub), c(2L, 5L))
  expect_equal(sub@data, data_mat[2:3, , drop = FALSE])
})

test_that("CiftiImage subsetting: cols only", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data_mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  img <- dtseries(data_mat, bm)

  sub <- img[, 1:3]
  expect_equal(dim(sub), c(4L, 3L))
  expect_equal(sub@data, data_mat[, 1:3, drop = FALSE])
})

test_that("BrainModelAxis logical subsetting", {
  bm <- brain_model_from_surface(0:9, nvertex = 100, name = "cortex_left")
  mask <- rep(c(TRUE, FALSE), 5)
  sub <- bm[mask]
  expect_equal(length(sub), 5L)
  expect_equal(sub@vertex, c(0L, 2L, 4L, 6L, 8L))
})

test_that("ParcelsAxis subsetting", {
  bm1 <- brain_model_from_surface(c(10L, 20L), nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(c(5L), nvertex = 80, name = "cortex_right")
  bm3 <- brain_model_from_surface(c(1L, 2L, 3L), nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(A = bm1, B = bm2, C = bm3))

  sub <- pa[c(1, 3)]
  expect_equal(length(sub), 2L)
  expect_equal(sub@parcel_name, c("A", "C"))
})

test_that("LabelAxis subsetting", {
  lt1 <- new("CiftiLabelTable", labels = list(
    new("CiftiLabel", key = 0L, label = "bg", red = 1, green = 1, blue = 1, alpha = 0)
  ))
  lt2 <- new("CiftiLabelTable", labels = list(
    new("CiftiLabel", key = 1L, label = "V1", red = 1, green = 0, blue = 0, alpha = 1)
  ))
  la <- new("LabelAxis", name = c("map1", "map2"), label = list(lt1, lt2),
            meta = list(list(), list()))

  sub <- la[2]
  expect_equal(length(sub), 1L)
  expect_equal(sub@name, "map2")
  expect_equal(sub@label[[1]]@labels[[1]]@label, "V1")
})

test_that("SeriesAxis non-contiguous subset degrades to ScalarAxis", {
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  sub <- sa[c(1, 3, 7)]
  expect_s4_class(sub, "ScalarAxis")
  expect_equal(length(sub), 3L)
})

test_that("ScalarAxis equality", {
  sc1 <- new("ScalarAxis", name = c("a", "b"), meta = list(list(), list()))
  sc2 <- new("ScalarAxis", name = c("a", "b"), meta = list(list(), list()))
  sc3 <- new("ScalarAxis", name = c("a", "c"), meta = list(list(), list()))
  expect_true(sc1 == sc2)
  expect_false(sc1 == sc3)
})

test_that("BrainModelAxis equality", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  bm3 <- brain_model_from_surface(0:3, nvertex = 100, name = "cortex_left")
  expect_true(bm1 == bm2)
  expect_false(bm1 == bm3)
})

test_that("ScalarAxis concatenation", {
  sc1 <- new("ScalarAxis", name = c("a"), meta = list(list()))
  sc2 <- new("ScalarAxis", name = c("b", "c"), meta = list(list(), list()))
  combined <- c(sc1, sc2)
  expect_equal(length(combined), 3L)
  expect_equal(combined@name, c("a", "b", "c"))
})

test_that("SeriesAxis concatenation", {
  sa1 <- new("SeriesAxis", start = 0, step = 1, size = 5L, unit = "SECOND")
  sa2 <- new("SeriesAxis", start = 5, step = 1, size = 3L, unit = "SECOND")
  combined <- c(sa1, sa2)
  expect_equal(combined@size, 8L)
  expect_equal(combined@start, 0)
})

test_that("ParcelsAxis iter_structures", {
  bm1 <- brain_model_from_surface(c(10L), nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(c(5L), nvertex = 80, name = "cortex_right")
  pa <- parcels_from_brain_models(list(A = bm1, B = bm2))

  structs <- iter_structures(pa)
  expect_equal(names(structs), c("A", "B"))
})

test_that("CiftiImage constructor from header only", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  hdr <- axes_to_header(sa, bm)

  img <- CiftiImage(header = hdr, intent = 3002L)
  expect_s4_class(row_axis(img), "SeriesAxis")
  expect_s4_class(col_axis(img), "BrainModelAxis")
})

test_that("CiftiImage constructor from axes only", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  sa <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")

  img <- CiftiImage(row_axis = sa, col_axis = bm, intent = 3002L)
  expect_s4_class(cifti_header(img), "CiftiHeader")
})

test_that("is_surface_structure works", {
  expect_true(ciftier:::is_surface_structure("CIFTI_STRUCTURE_CORTEX_LEFT"))
  expect_true(ciftier:::is_surface_structure("CIFTI_STRUCTURE_CORTEX_RIGHT"))
  expect_false(ciftier:::is_surface_structure("CIFTI_STRUCTURE_THALAMUS_LEFT"))
})

test_that("pad_to_16 works", {
  expect_equal(ciftier:::pad_to_16(16), 16)
  expect_equal(ciftier:::pad_to_16(17), 32)
  expect_equal(ciftier:::pad_to_16(32), 32)
  expect_equal(ciftier:::pad_to_16(1), 16)
})
