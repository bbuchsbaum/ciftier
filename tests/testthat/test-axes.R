test_that("BrainModelAxis from surface mask", {
  mask <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  bm <- brain_model_from_mask(mask, "cortex_left")

  expect_s4_class(bm, "BrainModelAxis")
  expect_equal(length(bm), 3L)
  expect_equal(bm@vertex, c(0L, 2L, 4L))  # 0-based
  expect_true(all(bm@surface_mask))
  expect_equal(bm@nvertices[["CIFTI_STRUCTURE_CORTEX_LEFT"]], 5)
})

test_that("BrainModelAxis from volume mask", {
  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 2, 3] <- TRUE
  mask[3, 3, 1] <- TRUE
  aff <- diag(4)
  bm <- brain_model_from_mask(mask, "thalamus_left", affine = aff)

  expect_s4_class(bm, "BrainModelAxis")
  expect_equal(length(bm), 2L)
  expect_false(any(bm@surface_mask))
  expect_equal(bm@volume_shape, c(3L, 3L, 3L))
  # IJK should be 0-based
  expect_true(all(bm@voxel >= 0))
})

test_that("BrainModelAxis from surface vertices", {
  bm <- brain_model_from_surface(c(10L, 20L, 30L), nvertex = 100, name = "cortex_right")
  expect_equal(length(bm), 3L)
  expect_equal(bm@vertex, c(10L, 20L, 30L))
  expect_equal(bm@nvertices[["CIFTI_STRUCTURE_CORTEX_RIGHT"]], 100L)
})

test_that("BrainModelAxis concatenation", {
  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(0:2, nvertex = 80, name = "cortex_right")
  bm <- c(bm1, bm2)

  expect_equal(length(bm), 8L)
  expect_equal(bm@name[1], "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(bm@name[6], "CIFTI_STRUCTURE_CORTEX_RIGHT")
  expect_equal(bm@nvertices[["CIFTI_STRUCTURE_CORTEX_LEFT"]], 100L)
  expect_equal(bm@nvertices[["CIFTI_STRUCTURE_CORTEX_RIGHT"]], 80L)
})

test_that("BrainModelAxis subsetting", {
  bm <- brain_model_from_surface(0:9, nvertex = 100, name = "cortex_left")
  sub <- bm[3:5]
  expect_equal(length(sub), 3L)
  expect_equal(sub@vertex, c(2L, 3L, 4L))
})

test_that("BrainModelAxis iter_structures", {
  bm1 <- brain_model_from_surface(0:2, nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(0:1, nvertex = 80, name = "cortex_right")
  bm <- c(bm1, bm2)

  structs <- iter_structures(bm)
  expect_equal(names(structs),
    c("CIFTI_STRUCTURE_CORTEX_LEFT", "CIFTI_STRUCTURE_CORTEX_RIGHT"))
  expect_equal(structs[["CIFTI_STRUCTURE_CORTEX_LEFT"]], 1:3)
  expect_equal(structs[["CIFTI_STRUCTURE_CORTEX_RIGHT"]], 4:5)
})

test_that("SeriesAxis basics", {
  sa <- new("SeriesAxis", start = 0, step = 0.72, size = 100L, unit = "SECOND")
  expect_equal(length(sa), 100L)

  sub <- sa[1:10]
  expect_s4_class(sub, "SeriesAxis")
  expect_equal(sub@size, 10L)
  expect_equal(sub@start, 0)

  sub2 <- sa[51:60]
  expect_equal(sub2@start, 50 * 0.72)
})

test_that("SeriesAxis equality", {
  sa1 <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  sa2 <- new("SeriesAxis", start = 0, step = 1, size = 10L, unit = "SECOND")
  sa3 <- new("SeriesAxis", start = 0, step = 2, size = 10L, unit = "SECOND")
  expect_true(sa1 == sa2)
  expect_false(sa1 == sa3)
})

test_that("ScalarAxis basics", {
  sc <- new("ScalarAxis", name = c("thickness", "curvature"), meta = list(list(), list()))
  expect_equal(length(sc), 2L)

  sub <- sc[1]
  expect_equal(sub@name, "thickness")
})

test_that("LabelAxis basics", {
  lt <- new("CiftiLabelTable", labels = list(
    new("CiftiLabel", key = 0L, label = "unknown", red = 1, green = 1, blue = 1, alpha = 0),
    new("CiftiLabel", key = 1L, label = "V1", red = 1, green = 0, blue = 0, alpha = 1)
  ))

  la <- new("LabelAxis",
    name = "parcellation",
    label = list(lt),
    meta = list(list())
  )
  expect_equal(length(la), 1L)
  expect_equal(la@label[[1]]@labels[[2]]@label, "V1")
})

test_that("header_to_axes and axes_to_header round-trip", {
  bm <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  sa <- new("SeriesAxis", start = 0, step = 0.72, size = 50L, unit = "SECOND")

  header <- axes_to_header(sa, bm)
  axes <- header_to_axes(header)

  expect_s4_class(axes$row_axis, "SeriesAxis")
  expect_equal(axes$row_axis@size, 50L)
  expect_equal(axes$row_axis@step, 0.72)

  expect_s4_class(axes$col_axis, "BrainModelAxis")
  expect_equal(length(axes$col_axis), 5L)
  expect_equal(axes$col_axis@vertex, 0:4)
})

test_that("ParcelsAxis from brain models", {
  bm1 <- brain_model_from_surface(c(10L, 20L), nvertex = 100, name = "cortex_left")
  bm2 <- brain_model_from_surface(c(5L, 15L), nvertex = 80, name = "cortex_right")

  pa <- parcels_from_brain_models(list(Parcel1 = bm1, Parcel2 = bm2))

  expect_s4_class(pa, "ParcelsAxis")
  expect_equal(length(pa), 2L)
  expect_equal(pa@parcel_name, c("Parcel1", "Parcel2"))
})
