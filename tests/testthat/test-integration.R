# Tests for Phase 6: Integration with neuroim2 / neurosurf
# These tests are skipped if the packages are not installed.

# Helper to create a simple SurfaceGeometry with n vertices
# Faces form a triangle fan from vertex 0.
.make_test_geom <- function(n, hemi = "lh") {
  verts <- matrix(rnorm(n * 3), ncol = 3)
  # Build triangles: (0,1,2), (0,2,3), ..., (0,n-2,n-1)
  nf <- max(n - 2L, 1L)
  faces <- cbind(rep(0L, nf), seq_len(nf), seq_len(nf) + 1L)
  neurosurf::SurfaceGeometry(verts, faces, hemi = hemi)
}


# ============================================================================
# Volume Extraction: as_neuro_vol
# ============================================================================

test_that("as_neuro_vol extracts single-map subcortical volume", {
  skip_if_not_installed("neuroim2")

  mask <- array(FALSE, dim = c(4, 4, 4))
  mask[1, 1, 1] <- TRUE
  mask[2, 3, 4] <- TRUE
  mask[4, 4, 4] <- TRUE
  aff <- diag(4)
  aff[1, 4] <- -10

  bm <- brain_model_from_mask(mask, "thalamus_left", affine = aff)
  data <- matrix(c(10, 20, 30), nrow = 1, ncol = 3)
  img <- dscalar(data, bm, names = "test_map")

  vol <- as_neuro_vol(img, "thalamus_left")
  expect_s4_class(vol, "SparseNeuroVol")
  expect_equal(dim(vol@space)[1:3], c(4L, 4L, 4L))

  # Check that data values are preserved
  sv <- vol@data
  expect_equal(length(sv@x), 3L)
  expect_equal(sort(sv@x), c(10, 20, 30))
})

test_that("as_neuro_vol extracts multi-map subcortical volume", {
  skip_if_not_installed("neuroim2")

  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  mask[2, 2, 2] <- TRUE
  aff <- diag(4)

  bm <- brain_model_from_mask(mask, "putamen_left", affine = aff)
  data <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  img <- dtseries(data, bm, step = 0.72)

  vec <- as_neuro_vol(img, "putamen_left")
  expect_s4_class(vec, "SparseNeuroVec")
  expect_equal(dim(vec@space)[4], 2L)
})

test_that("as_neuro_vol errors on surface structure", {
  skip_if_not_installed("neuroim2")

  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(5), nrow = 1, ncol = 5)
  img <- dscalar(data, bm, names = "test")

  expect_error(as_neuro_vol(img, "cortex_left"), "not surfaces")
})

test_that("as_neuro_vol errors on missing structure", {
  skip_if_not_installed("neuroim2")

  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  bm <- brain_model_from_mask(mask, "thalamus_left", affine = diag(4))
  data <- matrix(1, nrow = 1, ncol = 1)
  img <- dscalar(data, bm, names = "test")

  expect_error(as_neuro_vol(img, "putamen_left"), "not found")
})


# ============================================================================
# as_neuro_vec (all subcortical)
# ============================================================================

test_that("as_neuro_vec extracts all volume data", {
  skip_if_not_installed("neuroim2")

  bm_surf <- brain_model_from_surface(0:2, nvertex = 50, name = "cortex_left")

  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  mask[2, 2, 2] <- TRUE
  bm_vol <- brain_model_from_mask(mask, "thalamus_left", affine = diag(4))

  combined <- c(bm_surf, bm_vol)
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, combined)

  vec <- as_neuro_vec(img)
  expect_s4_class(vec, "SparseNeuroVec")
  expect_equal(sum(as.array(vec@mask)), 2L)
})

test_that("as_neuro_vec single-map returns SparseNeuroVol", {
  skip_if_not_installed("neuroim2")

  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  bm <- brain_model_from_mask(mask, "thalamus_left", affine = diag(4))
  data <- matrix(42, nrow = 1, ncol = 1)
  img <- dscalar(data, bm, names = "test")

  vol <- as_neuro_vec(img)
  expect_s4_class(vol, "SparseNeuroVol")
})

test_that("as_neuro_vec errors on surface-only data", {
  skip_if_not_installed("neuroim2")

  bm <- brain_model_from_surface(0:4, nvertex = 50, name = "cortex_left")
  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  expect_error(as_neuro_vec(img), "No volume")
})

test_that("as_neuro_vec errors on non-BrainModelAxis", {
  skip_if_not_installed("neuroim2")

  bm1 <- brain_model_from_surface(0:4, nvertex = 100, name = "cortex_left")
  pa <- parcels_from_brain_models(list(P1 = bm1))
  data <- matrix(rnorm(5), nrow = 5, ncol = 1)
  img <- ptseries(data, pa)

  expect_error(as_neuro_vec(img), "BrainModelAxis")
})


# ============================================================================
# Surface Extraction: as_neuro_surface
# ============================================================================

test_that("as_neuro_surface extracts single-map surface data", {
  skip_if_not_installed("neurosurf")

  bm <- brain_model_from_surface(c(0L, 2L, 5L), nvertex = 10, name = "cortex_left")
  data <- matrix(c(1.5, 2.5, 3.5), nrow = 1, ncol = 3)
  img <- dscalar(data, bm, names = "thickness")

  geom <- .make_test_geom(10, "lh")

  ns <- as_neuro_surface(img, "cortex_left", geom)
  expect_s4_class(ns, "NeuroSurface")
  expect_equal(ns@indices, c(1L, 3L, 6L))  # 0-based -> 1-based
  expect_equal(ns@data, c(1.5, 2.5, 3.5))
})

test_that("as_neuro_surface extracts multi-map as NeuroSurfaceVector", {
  skip_if_not_installed("neurosurf")

  bm <- brain_model_from_surface(c(0L, 1L, 2L), nvertex = 10, name = "cortex_left")
  data <- matrix(1:9, nrow = 3, ncol = 3)
  img <- dtseries(data, bm)

  geom <- .make_test_geom(10, "lh")

  nsv <- as_neuro_surface(img, "cortex_left", geom)
  expect_s4_class(nsv, "NeuroSurfaceVector")
  expect_equal(nsv@indices, c(1L, 2L, 3L))
  # mat should be [vertices x time] = [3 x 3]
  expect_equal(nrow(nsv@data), 3L)
  expect_equal(ncol(nsv@data), 3L)
})

test_that("as_neuro_surface errors on volume structure", {
  skip_if_not_installed("neurosurf")

  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  bm <- brain_model_from_mask(mask, "thalamus_left", affine = diag(4))
  data <- matrix(1, nrow = 1, ncol = 1)
  img <- dscalar(data, bm, names = "test")

  geom <- .make_test_geom(3, "lh")

  expect_error(as_neuro_surface(img, "thalamus_left", geom), "not volumes")
})


# ============================================================================
# as_bilat_surface_vector
# ============================================================================

test_that("as_bilat_surface_vector creates bilateral surface", {
  skip_if_not_installed("neurosurf")

  bm_l <- brain_model_from_surface(0:2, nvertex = 10, name = "cortex_left")
  bm_r <- brain_model_from_surface(0:1, nvertex = 8, name = "cortex_right")
  bm <- c(bm_l, bm_r)

  data <- matrix(rnorm(10), nrow = 2, ncol = 5)
  img <- dtseries(data, bm)

  geom_l <- .make_test_geom(10, "lh")
  geom_r <- .make_test_geom(8, "rh")

  bilat <- as_bilat_surface_vector(img, geom_l, geom_r)
  expect_s4_class(bilat, "BilatNeuroSurfaceVector")
  expect_s4_class(bilat@left, "NeuroSurfaceVector")
  expect_s4_class(bilat@right, "NeuroSurfaceVector")
  expect_equal(bilat@left@indices, c(1L, 2L, 3L))
  expect_equal(bilat@right@indices, c(1L, 2L))
})


# ============================================================================
# cifti_from_data (surfaces)
# ============================================================================

test_that("cifti_from_data creates CiftiImage from surface data", {
  skip_if_not_installed("neurosurf")

  geom <- .make_test_geom(10, "lh")

  nsv <- neurosurf::NeuroSurfaceVector(
    geometry = geom,
    indices = c(1L, 3L, 5L),
    mat = matrix(rnorm(9), nrow = 3, ncol = 3)
  )

  img <- cifti_from_data(
    surfaces = list(cortex_left = nsv),
    intent = 3002L
  )

  expect_s4_class(img, "CiftiImage")
  expect_equal(cifti_intent(img), 3002L)
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_equal(length(col_axis(img)), 3L)
  # Vertices should be 0-based in CIFTI
  expect_equal(col_axis(img)@vertex, c(0L, 2L, 4L))
  expect_equal(nrow(cifti_data(img)), 3L)  # 3 time points
})

test_that("cifti_from_data creates CiftiImage from single NeuroSurface", {
  skip_if_not_installed("neurosurf")

  geom <- .make_test_geom(10, "lh")

  ns <- neurosurf::NeuroSurface(
    geometry = geom,
    indices = c(1L, 2L, 3L),
    data = c(1.5, 2.5, 3.5)
  )

  img <- cifti_from_data(
    surfaces = list(cortex_left = ns),
    intent = 3006L
  )

  expect_equal(dim(img), c(1L, 3L))
  expect_equal(as.numeric(cifti_data(img)), c(1.5, 2.5, 3.5))
})


# ============================================================================
# cifti_from_data (volumes)
# ============================================================================

test_that("cifti_from_data creates CiftiImage from volume data", {
  skip_if_not_installed("neuroim2")

  vol_dims <- c(4L, 4L, 4L)
  space3 <- neuroim2::NeuroSpace(dim = vol_dims, trans = diag(4))

  sv <- neuroim2::SparseNeuroVol(
    data = c(10, 20, 30),
    space = space3,
    indices = c(1L, 5L, 10L)
  )

  img <- cifti_from_data(
    volumes = list(thalamus_left = sv),
    intent = 3006L
  )

  expect_s4_class(img, "CiftiImage")
  expect_equal(dim(img), c(1L, 3L))
  expect_equal(as.numeric(cifti_data(img)), c(10, 20, 30))
  expect_s4_class(col_axis(img), "BrainModelAxis")
  expect_false(any(col_axis(img)@surface_mask))
})


# ============================================================================
# cifti_from_data (mixed surfaces + volumes)
# ============================================================================

test_that("cifti_from_data combines surface and volume data", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("neurosurf")

  geom <- .make_test_geom(10, "lh")

  # Single-map surface (1 x 2) to match single-map volume
  ns <- neurosurf::NeuroSurface(
    geometry = geom,
    indices = c(1L, 2L),
    data = c(1.0, 2.0)
  )

  vol_dims <- c(3L, 3L, 3L)
  space3 <- neuroim2::NeuroSpace(dim = vol_dims, trans = diag(4))

  sv <- neuroim2::SparseNeuroVol(
    data = c(5),
    space = space3,
    indices = c(1L)
  )

  img <- cifti_from_data(
    surfaces = list(cortex_left = ns),
    volumes = list(thalamus_left = sv),
    intent = 3006L
  )

  expect_s4_class(img, "CiftiImage")
  ca <- col_axis(img)
  expect_equal(length(ca), 3L)  # 2 surface + 1 volume
  expect_equal(sum(ca@surface_mask), 2L)
  expect_equal(sum(!ca@surface_mask), 1L)
})


# ============================================================================
# cifti_from_data error cases
# ============================================================================

test_that("cifti_from_data errors with no inputs", {
  expect_error(cifti_from_data(), "At least one surface or volume")
})

test_that("cifti_from_data errors with wrong surface type", {
  skip_if_not_installed("neurosurf")

  expect_error(
    cifti_from_data(surfaces = list(cortex_left = "not_a_surface")),
    "must be a NeuroSurface"
  )
})


# ============================================================================
# Round-trip: CiftiImage -> neurosurf -> CiftiImage
# ============================================================================

test_that("surface data survives CiftiImage -> NeuroSurfaceVector -> CiftiImage", {
  skip_if_not_installed("neurosurf")

  bm <- brain_model_from_surface(c(0L, 2L, 5L), nvertex = 10, name = "cortex_left")
  orig_data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  img <- dtseries(orig_data, bm, step = 0.72)

  geom <- .make_test_geom(10, "lh")

  nsv <- as_neuro_surface(img, "cortex_left", geom)

  img2 <- cifti_from_data(
    surfaces = list(cortex_left = nsv),
    intent = 3002L
  )

  expect_equal(cifti_data(img2), orig_data)
  expect_equal(col_axis(img2)@vertex, c(0L, 2L, 5L))
})

test_that("volume data survives CiftiImage -> SparseNeuroVol -> CiftiImage", {
  skip_if_not_installed("neuroim2")

  mask <- array(FALSE, dim = c(3, 3, 3))
  mask[1, 1, 1] <- TRUE
  mask[2, 2, 2] <- TRUE
  aff <- diag(4)
  aff[1, 4] <- -5

  bm <- brain_model_from_mask(mask, "thalamus_left", affine = aff)
  orig_data <- matrix(c(10, 20), nrow = 1, ncol = 2)
  img <- dscalar(orig_data, bm, names = "map1")

  vol <- as_neuro_vol(img, "thalamus_left")

  img2 <- cifti_from_data(
    volumes = list(thalamus_left = vol),
    intent = 3006L
  )

  expect_equal(sort(as.numeric(cifti_data(img2))), c(10, 20))
})


# ============================================================================
# .cifti_to_neurospace helper
# ============================================================================

test_that(".cifti_to_neurospace creates 3D NeuroSpace", {
  skip_if_not_installed("neuroim2")

  aff <- diag(4)
  aff[1, 1] <- 2
  aff[2, 2] <- 2
  aff[3, 3] <- 2

  sp <- ciftier:::.cifti_to_neurospace(c(10L, 10L, 10L), aff, ndim = 3L)
  expect_equal(length(dim(sp)), 3L)
  expect_equal(dim(sp), c(10L, 10L, 10L))
})

test_that(".cifti_to_neurospace creates 4D NeuroSpace", {
  skip_if_not_installed("neuroim2")

  sp <- ciftier:::.cifti_to_neurospace(c(5L, 5L, 5L), diag(4), ndim = 4L, n4 = 20L)
  expect_equal(length(dim(sp)), 4L)
  expect_equal(dim(sp)[4], 20L)
})


# ============================================================================
# .check_neuroim2 / .check_neurosurf helpers exist
# ============================================================================

test_that(".check_neuroim2 is a function", {
  expect_true(is.function(ciftier:::.check_neuroim2))
})

test_that(".check_neurosurf is a function", {
  expect_true(is.function(ciftier:::.check_neurosurf))
})
