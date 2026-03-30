# ciftier

[![CI](https://github.com/bbuchsbaum/ciftier/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/bbuchsbaum/ciftier/actions/workflows/ci.yml)

`ciftier` is a pure R package for reading, writing, and constructing CIFTI-2 neuroimaging files.

It focuses on the common single-file CIFTI-2 formats used for grayordinate data:

- dense time series: `.dtseries.nii`
- dense scalars: `.dscalar.nii`
- dense labels: `.dlabel.nii`

The package also exposes axis classes and constructors for parcel and connectivity CIFTI objects, so you can work directly with CIFTI metadata in R instead of treating the file as an opaque container.

## Why use it

- Pure R I/O for supported CIFTI-2 use cases
- No Connectome Workbench dependency for reading and writing the supported file types
- S4 classes that mirror the CIFTI-2 XML structure
- User-facing axis abstractions for surfaces, parcels, scalars, labels, and series
- Optional conversion to and from `neuroim2` and `neurosurf` objects
- Real interoperability tests against small external CIFTI fixtures

## Installation

`ciftier` is not on CRAN at the moment.

```r
install.packages("remotes")
remotes::install_github("bbuchsbaum/ciftier")
```

Optional integration packages:

```r
install.packages(
  c("neuroim2", "neurosurf"),
  repos = c("https://bbuchsbaum.r-universe.dev", "https://cloud.r-project.org")
)
```

## Quick start

### Read a CIFTI file

```r
library(ciftier)

img <- read_cifti("subject.dtseries.nii")
img

dim(img)
class(row_axis(img))
class(col_axis(img))
cifti_intent(img)
```

To inspect the header without loading the data matrix:

```r
hdr_only <- read_cifti("subject.dtseries.nii", drop_data = TRUE)
dim(hdr_only)
row_axis(hdr_only)
col_axis(hdr_only)
```

### Create and write a small CIFTI image

```r
library(ciftier)

# 10 left-hemisphere surface vertices, indexed in CIFTI's 0-based convention
bm <- brain_model_from_surface(
  vertices = 0:9,
  nvertex = 100,
  name = "cortex_left"
)

# 5 time points x 10 grayordinates
mat <- matrix(rnorm(50), nrow = 5, ncol = 10)

img <- dtseries(mat, bm, start = 0, step = 0.72)
write_cifti(img, "toy.dtseries.nii")

img2 <- read_cifti("toy.dtseries.nii")
all.equal(cifti_data(img2), mat, tolerance = 1e-6)
```

### Work with structures and axes

```r
library(ciftier)

img <- read_cifti("subject.dtseries.nii")

left <- extract_structure(img, "cortex_left")
dim(left$data)
left$indices[1:5]

iter_structures(col_axis(img))
volume_info(img)
brain_models(img)
named_maps(img)
```

## Supported constructors

These helpers build a `CiftiImage` with the correct intent and row/column axes:

| CIFTI object | Helper |
| --- | --- |
| dense time series | `dtseries()` |
| dense scalars | `dscalar()` |
| dense labels | `dlabel()` |
| parcel time series | `ptseries()` |
| parcel scalars | `pscalar()` |
| parcel labels | `plabel()` |
| dense connectivity | `dconn()` |
| parcel connectivity | `pconn()` |
| dense-to-parcel connectivity | `dpconn()` |
| parcel-to-dense connectivity | `pdconn()` |

Lower-level helpers are also available if you want to assemble axes directly:

- `brain_model_from_surface()`
- `brain_model_from_mask()`
- `parcels_from_brain_models()`
- `axes_to_header()`
- `header_to_axes()`
- `build_cifti_xml()`
- `parse_cifti_xml()`

## Optional spatial integration

If `neuroim2` is installed, you can pull subcortical data out of a `CiftiImage` as sparse volumetric objects:

```r
library(ciftier)

img <- read_cifti("subject.dscalar.nii")
subcort <- as_neuro_vec(img)
```

If `neurosurf` is installed, you can extract cortical data as surface objects:

```r
library(ciftier)

img <- read_cifti("subject.dscalar.nii")

# geometry must be provided by the caller
surf <- as_neuro_surface(img, "cortex_left", geometry = left_geom)
```

`cifti_from_data()` goes the other direction and builds a `CiftiImage` from `neuroim2` and `neurosurf` objects.

## Testing

Run the standard test suite:

```r
testthat::test_local()
```

The repo also includes opt-in compatibility tests that download small external fixtures from pinned upstream commits and cache them locally:

```r
Sys.setenv(CIFTIER_ONLINE_TESTS = "1", NOT_CRAN = "true")
testthat::test_local(filter = "external-fixtures")
```

You can override the cache location with `CIFTIER_FIXTURE_CACHE_DIR`.

## Status

The package is already useful for pure-R CIFTI I/O and metadata manipulation, but it is still early-stage software. The core dense file types are the main focus right now, and the public API may still evolve as more real-world CIFTI files are tested.

## License

MIT. See [LICENSE](LICENSE).
