test_that("write_cifti and read_cifti round-trip", {
  # Build a small dtseries CIFTI image
  bm <- brain_model_from_surface(0:9, nvertex = 100, name = "cortex_left")
  ntime <- 5L
  ngray <- 10L
  data_mat <- matrix(rnorm(ntime * ngray), nrow = ntime, ncol = ngray)

  img <- dtseries(data_mat, bm, start = 0, step = 0.72)
  expect_s4_class(img, "CiftiImage")
  expect_equal(img@intent, 3002L)

  # Write to temp file

  tmp <- tempfile(fileext = ".dtseries.nii")
  on.exit(unlink(tmp), add = TRUE)
  write_cifti(img, tmp)

  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)

  # Read back
  img2 <- read_cifti(tmp)
  expect_s4_class(img2, "CiftiImage")
  expect_equal(img2@intent, 3002L)

  # Check axes
  expect_s4_class(row_axis(img2), "SeriesAxis")
  expect_equal(row_axis(img2)@size, 5L)
  expect_equal(row_axis(img2)@step, 0.72)

  expect_s4_class(col_axis(img2), "BrainModelAxis")
  expect_equal(length(col_axis(img2)), 10L)
  expect_equal(col_axis(img2)@vertex, 0:9)

  # Check data
  expect_equal(dim(img2), c(5L, 10L))
  expect_equal(img2@data, data_mat, tolerance = 1e-6)
})


test_that("read_cifti with drop_data=TRUE returns NULL data", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_right")
  data_mat <- matrix(1:10, nrow = 2, ncol = 5)

  img <- dtseries(data_mat, bm, start = 0, step = 1)

  tmp <- tempfile(fileext = ".dtseries.nii")
  on.exit(unlink(tmp), add = TRUE)
  write_cifti(img, tmp)

  img_hdr <- read_cifti(tmp, drop_data = TRUE)
  expect_null(img_hdr@data)
  expect_s4_class(row_axis(img_hdr), "SeriesAxis")
  expect_equal(row_axis(img_hdr)@size, 2L)
})


test_that("dscalar intent helper works", {
  bm <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  data_mat <- matrix(rnorm(10), nrow = 2, ncol = 5)

  img <- dscalar(data_mat, bm, names = c("thickness", "curvature"))
  expect_equal(img@intent, 3006L)
  expect_s4_class(row_axis(img), "ScalarAxis")
  expect_equal(row_axis(img)@name, c("thickness", "curvature"))

  # Write and re-read
  tmp <- tempfile(fileext = ".dscalar.nii")
  on.exit(unlink(tmp), add = TRUE)
  write_cifti(img, tmp)

  img2 <- read_cifti(tmp)
  expect_equal(img2@intent, 3006L)
  expect_s4_class(row_axis(img2), "ScalarAxis")
  expect_equal(row_axis(img2)@name, c("thickness", "curvature"))
  expect_equal(img2@data, data_mat, tolerance = 1e-6)
})


test_that("dlabel intent helper works", {
  bm <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  data_mat <- matrix(c(0L, 1L, 2L, 1L, 0L), nrow = 1, ncol = 5)

  lt <- new("CiftiLabelTable", labels = list(
    new("CiftiLabel", key = 0L, label = "unknown", red = 1, green = 1, blue = 1, alpha = 0),
    new("CiftiLabel", key = 1L, label = "V1", red = 1, green = 0, blue = 0, alpha = 1),
    new("CiftiLabel", key = 2L, label = "V2", red = 0, green = 1, blue = 0, alpha = 1)
  ))

  img <- dlabel(data_mat, bm, names = "parcellation", label_tables = list(lt))
  expect_equal(img@intent, 3007L)
  expect_s4_class(row_axis(img), "LabelAxis")

  # Write and re-read
  tmp <- tempfile(fileext = ".dlabel.nii")
  on.exit(unlink(tmp), add = TRUE)
  write_cifti(img, tmp)

  img2 <- read_cifti(tmp)
  expect_equal(img2@intent, 3007L)
  expect_s4_class(row_axis(img2), "LabelAxis")
  la <- row_axis(img2)
  expect_equal(la@name, "parcellation")
  expect_length(la@label[[1]]@labels, 3)
  expect_equal(la@label[[1]]@labels[[2]]@label, "V1")
})


test_that("CiftiImage subsetting preserves axes", {
  bm <- brain_model_from_surface(0:9, nvertex = 100, name = "cortex_left")
  data_mat <- matrix(rnorm(50), nrow = 5, ncol = 10)
  img <- dtseries(data_mat, bm, start = 0, step = 0.72)

  # Subset rows (time)
  sub <- img[1:3, ]
  expect_equal(dim(sub), c(3L, 10L))
  expect_s4_class(row_axis(sub), "SeriesAxis")
  expect_equal(row_axis(sub)@size, 3L)

  # Subset columns (grayordinates)
  sub2 <- img[, 2:5]
  expect_equal(dim(sub2), c(5L, 4L))
  expect_s4_class(col_axis(sub2), "BrainModelAxis")
  expect_equal(length(col_axis(sub2)), 4L)
  expect_equal(col_axis(sub2)@vertex, 1:4)
})


test_that("write_cifti with FLOAT64 data type", {
  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data_mat <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data_mat, bm)

  tmp <- tempfile(fileext = ".dtseries.nii")
  on.exit(unlink(tmp), add = TRUE)
  write_cifti(img, tmp, data_type = "FLOAT64")

  img2 <- read_cifti(tmp)
  expect_equal(img2@data, data_mat, tolerance = 1e-12)
})
