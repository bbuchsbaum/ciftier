test_that("CIFTI_STRUCTURES has expected entries", {
  expect_true("CIFTI_STRUCTURE_CORTEX_LEFT" %in% CIFTI_STRUCTURES)
  expect_true("CIFTI_STRUCTURE_CORTEX_RIGHT" %in% CIFTI_STRUCTURES)
  expect_true("CIFTI_STRUCTURE_THALAMUS_LEFT" %in% CIFTI_STRUCTURES)
  expect_equal(unname(CIFTI_STRUCTURES["CORTEX_LEFT"]), "CIFTI_STRUCTURE_CORTEX_LEFT")
})

test_that("CIFTI_INDEX_TYPES has all five types", {
  expect_length(CIFTI_INDEX_TYPES, 5)
  expect_true("CIFTI_INDEX_TYPE_BRAIN_MODELS" %in% CIFTI_INDEX_TYPES)
  expect_true("CIFTI_INDEX_TYPE_SERIES" %in% CIFTI_INDEX_TYPES)
})

test_that("CIFTI_INTENTS has expected intent codes", {
  expect_equal(CIFTI_INTENTS[["ConnDenseTimeSeries"]], 3002L)
  expect_equal(CIFTI_INTENTS[["ConnDenseScalar"]], 3006L)
  expect_equal(CIFTI_INTENTS[["ConnDenseLabel"]], 3007L)
})

test_that("to_cifti_structure_name normalizes correctly", {
  expect_equal(to_cifti_structure_name("cortex_left"), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(to_cifti_structure_name("CORTEX_LEFT"), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(to_cifti_structure_name("CortexLeft"), "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(to_cifti_structure_name("CIFTI_STRUCTURE_CORTEX_LEFT"),
               "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_error(to_cifti_structure_name("BOGUS_STRUCTURE"))
})

test_that("index conversion round-trips", {
  cifti_idx <- c(0L, 5L, 99L)
  expect_equal(to_cifti_index(to_r_index(cifti_idx)), cifti_idx)
  r_idx <- c(1L, 6L, 100L)
  expect_equal(to_r_index(to_cifti_index(r_idx)), r_idx)
})

test_that("ijk_to_linear and linear_to_ijk round-trip", {
  vol_dims <- c(10L, 12L, 8L)
  ijk <- matrix(c(3L, 5L, 2L, 0L, 0L, 0L, 9L, 11L, 7L), ncol = 3, byrow = TRUE)
  lin <- ciftier:::ijk_to_linear(ijk, vol_dims)
  ijk2 <- ciftier:::linear_to_ijk(lin, vol_dims)
  expect_equal(unname(ijk2), ijk)
})
