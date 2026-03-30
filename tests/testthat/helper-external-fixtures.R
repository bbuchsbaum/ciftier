# External CIFTI compatibility fixtures are opt-in and cached locally.
# Set CIFTIER_ONLINE_TESTS=1 to run these tests.

.external_cifti_fixtures <- list(
  dscalar_6k = list(
    url = "https://raw.githubusercontent.com/mandymejia/ciftiTools/1dc7110788c6cb12f15f28eb0a2c012df0387047/inst/extdata/Conte69.MyelinAndCorrThickness.6k_fs_LR.dscalar.nii",
    size = 145712L,
    md5 = "1c189c7e88dcf539c9cb33f14d319371"
  ),
  dlabel_6k = list(
    url = "https://raw.githubusercontent.com/mandymejia/ciftiTools/1dc7110788c6cb12f15f28eb0a2c012df0387047/inst/extdata/Conte69.parcellations_VGD11b.6k_fs_LR.dlabel.nii",
    size = 228240L,
    md5 = "94072903021f0dedbefc2abd6b18732c"
  ),
  ones_1k = list(
    url = "https://raw.githubusercontent.com/mandymejia/ciftiTools/1dc7110788c6cb12f15f28eb0a2c012df0387047/inst/extdata/ones_1k.dscalar.nii",
    size = 434308L,
    md5 = "086f0a9e2877f59187cfea6642d08d92"
  ),
  dtseries_32k = list(
    url = "https://raw.githubusercontent.com/mandymejia/ciftiTools/1dc7110788c6cb12f15f28eb0a2c012df0387047/inst/extdata/Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii",
    size = 837864L,
    md5 = "3ad43d455ecfc549e0a6c45739ab103b"
  ),
  ptseries_32k = list(
    url = "https://raw.githubusercontent.com/demianw/nibabel-nitest-cifti2/26b7cb95d76066b93fd2ee65121b9bdb7e034713/Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii",
    size = 139264L,
    md5 = "e84f3bba2efcafc990b5128f7366f049"
  )
)

.is_truthy_env <- function(name) {
  tolower(Sys.getenv(name, "")) %in% c("1", "true", "yes", "on")
}

.external_fixture_cache_dir <- function() {
  override <- Sys.getenv("CIFTIER_FIXTURE_CACHE_DIR", "")
  if (nzchar(override)) {
    dir.create(override, recursive = TRUE, showWarnings = FALSE)
    return(override)
  }

  preferred <- tryCatch(
    file.path(tools::R_user_dir("ciftier", which = "cache"), "test-fixtures"),
    error = function(...) ""
  )
  if (nzchar(preferred)) {
    ok <- tryCatch({
      dir.create(preferred, recursive = TRUE, showWarnings = FALSE)
      file.access(preferred, 2L) == 0L
    }, error = function(...) FALSE)
    if (ok) return(preferred)
  }

  fallback <- file.path(tempdir(), "ciftier-test-fixtures")
  dir.create(fallback, recursive = TRUE, showWarnings = FALSE)
  fallback
}

skip_if_external_cifti_disabled <- function() {
  testthat::skip_on_cran()
  if (!.is_truthy_env("CIFTIER_ONLINE_TESTS")) {
    testthat::skip("Set CIFTIER_ONLINE_TESTS=1 to run external CIFTI compatibility tests")
  }
}

external_cifti_fixture <- function(name) {
  spec <- .external_cifti_fixtures[[name]]
  if (is.null(spec)) stop(sprintf("Unknown external fixture '%s'", name))

  cache_dir <- .external_fixture_cache_dir()
  dest <- file.path(cache_dir, basename(spec$url))
  current_md5 <- if (file.exists(dest)) unname(tools::md5sum(dest)) else NA_character_

  if (!identical(current_md5, spec$md5)) {
    tmp <- tempfile(pattern = paste0(name, "-"), tmpdir = cache_dir, fileext = ".download")
    on.exit(unlink(tmp), add = TRUE)

    utils::download.file(spec$url, destfile = tmp, method = "libcurl", mode = "wb", quiet = TRUE)

    info <- file.info(tmp)
    if (is.na(info$size) || info$size != spec$size) {
      stop(sprintf("Downloaded fixture '%s' has unexpected size", name))
    }

    downloaded_md5 <- unname(tools::md5sum(tmp))
    if (!identical(downloaded_md5, spec$md5)) {
      stop(sprintf("Downloaded fixture '%s' failed checksum verification", name))
    }

    if (file.exists(dest)) unlink(dest)
    if (!file.rename(tmp, dest)) {
      stop(sprintf("Could not move downloaded fixture '%s' into cache", name))
    }
  }

  dest
}
