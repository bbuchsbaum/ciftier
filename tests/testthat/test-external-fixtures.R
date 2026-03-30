test_that("external dscalar fixture reads with expected scalar metadata", {
  skip_if_external_cifti_disabled()

  img <- read_cifti(external_cifti_fixture("dscalar_6k"), drop_data = TRUE)

  expect_equal(cifti_intent(img), 3006L)
  expect_equal(dim(img), c(2L, 10846L))
  expect_s4_class(row_axis(img), "ScalarAxis")
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_true(all(col_axis(img)@surface_mask))
  expect_equal(row_axis(img)@name, c("MyelinMap_BC_decurv", "corrThickness"))
})

test_that("external dlabel fixture reads label metadata", {
  skip_if_external_cifti_disabled()

  img <- read_cifti(external_cifti_fixture("dlabel_6k"), drop_data = TRUE)

  expect_equal(cifti_intent(img), 3007L)
  expect_equal(dim(img), c(3L, 11524L))
  expect_s4_class(row_axis(img), "LabelAxis")
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_equal(length(row_axis(img)@label[[1]]@labels), 96L)
  expect_equal(row_axis(img)@name[[1]], "Composite Parcellation-lh (FRB08_OFP03_retinotopic)")
})

test_that("external ones dscalar fixture preserves subcortical layout and data", {
  skip_if_external_cifti_disabled()

  img <- read_cifti(external_cifti_fixture("ones_1k"))

  expect_equal(cifti_intent(img), 3006L)
  expect_equal(dim(img), c(1L, 33709L))
  expect_equal(as.numeric(img@data), rep(1, 33709L))
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_gt(sum(!col_axis(img)@surface_mask), 0L)
  expect_s4_class(volume_info(img), "CiftiVolume")
  expect_equal(volume_info(img)@dimensions, c(91L, 109L, 91L))
})

test_that("external dtseries fixture reads as dense time series", {
  skip_if_external_cifti_disabled()

  img <- read_cifti(external_cifti_fixture("dtseries_32k"), drop_data = TRUE)

  expect_equal(cifti_intent(img), 3002L)
  expect_equal(dim(img), c(2L, 60951L))
  expect_s4_class(row_axis(img), "SeriesAxis")
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_equal(row_axis(img)@step, 1)
  expect_equal(row_axis(img)@unit, "SECOND")
  expect_true(all(col_axis(img)@surface_mask))
})

test_that("external ptseries fixture reads as parcel time series", {
  skip_if_external_cifti_disabled()

  img <- read_cifti(external_cifti_fixture("ptseries_32k"), drop_data = TRUE)

  expect_equal(cifti_intent(img), 3004L)
  expect_equal(dim(img), c(2L, 54L))
  expect_s4_class(row_axis(img), "SeriesAxis")
  expect_s4_class(col_axis(img), "ParcelsAxis")
  expect_equal(row_axis(img)@step, 1)
  expect_equal(row_axis(img)@unit, "SECOND")
  expect_true("MEDIAL.WALL" %in% col_axis(img)@parcel_name)
})
