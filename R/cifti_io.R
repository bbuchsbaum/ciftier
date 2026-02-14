#' @include cifti_image.R
NULL

# ============================================================================
# Read CIFTI-2 Files
# ============================================================================

#' Read a CIFTI-2 File
#'
#' Reads a CIFTI-2 file (.dtseries.nii, .dscalar.nii, .dlabel.nii, etc.)
#' and returns a \code{\linkS4class{CiftiImage}}.
#'
#' @param file_name Path to the CIFTI-2 file.
#' @param drop_data If TRUE, only read the header/axes without loading
#'   the data matrix. Useful for inspecting large files.
#' @return A \code{\linkS4class{CiftiImage}} object.
#'
#' @details
#' The function reads the NIfTI-2 header using RNifti, extracts the
#' CIFTI-2 XML extension (ecode=32), parses it into axis objects, and
#' optionally reads the binary data matrix.
#'
#' @export
read_cifti <- function(file_name, drop_data = FALSE) {
  file_name <- normalizePath(file_name, mustWork = TRUE)

  # Step 1: Read NIfTI-2 header via RNifti
  nhdr <- RNifti::niftiHeader(file_name)
  intent_code <- as.integer(nhdr$intent_code)
  vox_offset <- as.numeric(nhdr$vox_offset)
  datatype <- as.integer(nhdr$datatype)
  bitpix <- as.integer(nhdr$bitpix)

  # Dimensions: for CIFTI, dim[1]=6 (intent), dim[6]=nrow, dim[7]=ncol
  # But RNifti normalizes these — we need to figure out the data shape
  # from the axes after parsing XML
  nifti_dim <- nhdr$dim

  # Step 2: Read the CIFTI XML extension
  xml_raw <- .read_cifti_extension(file_name, vox_offset)
  if (is.null(xml_raw)) {
    stop("No CIFTI extension (ecode=32) found in file")
  }

  # Step 3: Parse XML → header → axes
  header <- parse_cifti_xml(xml_raw)
  axes <- header_to_axes(header)
  row_axis <- axes$row_axis
  col_axis <- axes$col_axis

  nrow_data <- length(row_axis)
  ncol_data <- length(col_axis)

  # Step 4: Read binary data
  data_mat <- NULL
  if (!drop_data) {
    data_mat <- .read_cifti_data(file_name, vox_offset, datatype, bitpix,
                                  nrow_data, ncol_data)
  }

  CiftiImage(
    data = data_mat,
    header = header,
    row_axis = row_axis,
    col_axis = col_axis,
    intent = intent_code,
    file_name = file_name
  )
}


# ============================================================================
# Write CIFTI-2 Files
# ============================================================================

#' Write a CIFTI-2 File
#'
#' Writes a \code{\linkS4class{CiftiImage}} to a CIFTI-2 NIfTI file.
#'
#' @param x A \code{\linkS4class{CiftiImage}} object.
#' @param file_name Output file path.
#' @param data_type Data type for the binary data. One of "FLOAT32" (default)
#'   or "FLOAT64".
#' @return Invisibly returns the file path.
#'
#' @export
write_cifti <- function(x, file_name, data_type = "FLOAT32") {
  stopifnot(is(x, "CiftiImage"))
  stopifnot(!is.null(x@data))

  # Build header from axes
  header <- axes_to_header(x@row_axis, x@col_axis)
  xml_str <- build_cifti_xml(header)
  xml_raw <- c(charToRaw(xml_str), as.raw(0))  # null-terminated

  # Compute extension size (8-byte header + data, padded to 16)
  ext_data_size <- length(xml_raw)
  esize <- pad_to_16(8L + ext_data_size)
  padding_needed <- esize - 8L - ext_data_size

  # NIfTI-2 header is 540 bytes + 4 byte extender + extension
  vox_offset <- NIFTI2_HEADER_SIZE + 4L + esize

  # Data type info
  dtype_code <- NIFTI2_DTYPES[[data_type]]
  if (is.null(dtype_code)) stop(sprintf("Unknown data_type: '%s'", data_type))
  dtype_size <- NIFTI2_DTYPE_SIZES[[as.character(dtype_code)]]

  nrows <- nrow(x@data)
  ncols <- ncol(x@data)

  # Open file for writing

  con <- file(file_name, "wb")
  on.exit(close(con))

  # Write NIfTI-2 header (540 bytes)
  .write_nifti2_header(con, intent = x@intent, datatype = dtype_code,
                        bitpix = dtype_size * 8L, nrows = nrows,
                        ncols = ncols, vox_offset = vox_offset)

  # Write extender (4 bytes): extensions present
  writeBin(as.raw(c(1L, 0L, 0L, 0L)), con)

  # Write extension: esize + ecode + data + padding
  writeBin(as.integer(esize), con, size = 4L, endian = "little")
  writeBin(32L, con, size = 4L, endian = "little")  # ecode = CIFTI
  writeBin(xml_raw, con)
  if (padding_needed > 0) writeBin(raw(padding_needed), con)

  # Write data matrix (column-major → row-major for CIFTI)
  # CIFTI stores data as [ncols x nrows] in C order, which means
  # we write the transpose in Fortran order
  what <- if (data_type == "FLOAT64") "double" else "numeric"
  size <- if (data_type == "FLOAT64") 8L else 4L
  writeBin(as.vector(t(x@data)), con, size = size, endian = "little")

  invisible(file_name)
}


# ============================================================================
# Internal I/O Helpers
# ============================================================================

#' Read the CIFTI XML extension from a NIfTI-2 file
#' @keywords internal
.read_cifti_extension <- function(file_name, vox_offset) {
  con <- file(file_name, "rb")
  on.exit(close(con))

  # Seek past NIfTI-2 header (540 bytes)
  seek(con, NIFTI2_HEADER_SIZE)

  # Read 4-byte extender
  extender <- readBin(con, "raw", n = 4)
  if (extender[1] == as.raw(0)) return(NULL)

  current_pos <- NIFTI2_HEADER_SIZE + 4L

  while (current_pos < vox_offset) {
    esize <- readBin(con, "integer", n = 1, size = 4, endian = "little")
    ecode <- readBin(con, "integer", n = 1, size = 4, endian = "little")

    if (is.na(esize) || esize < 16) break

    edata_size <- esize - 8L

    if (ecode == 32L) {
      # Found CIFTI extension
      return(readBin(con, "raw", n = edata_size))
    }

    # Skip this extension's data
    seek(con, current_pos + esize)
    current_pos <- current_pos + esize
  }

  NULL
}


#' Read binary data from a CIFTI file
#' @keywords internal
.read_cifti_data <- function(file_name, vox_offset, datatype, bitpix,
                              nrows, ncols) {
  con <- file(file_name, "rb")
  on.exit(close(con))

  seek(con, as.integer(vox_offset))

  n_elements <- as.integer(nrows) * as.integer(ncols)
  byte_size <- bitpix %/% 8L

  # Determine what to read
  what <- switch(as.character(datatype),
    "16" = "numeric",   # FLOAT32
    "64" = "double",    # FLOAT64
    "8"  = "integer",   # INT32
    "4"  = "integer",   # INT16
    "256" = "integer",  # INT8
    "numeric"           # default
  )

  vals <- readBin(con, what = what, n = n_elements, size = byte_size,
                   endian = "little")

  # CIFTI data is stored as [cols x rows] in row-major (C) order
  # which means reading in Fortran order gives us [ncols, nrows]
  # We need to transpose to get [nrows, ncols]
  matrix(vals, nrow = ncols, ncol = nrows, byrow = FALSE) |> t()
}


#' Write an int64 value as 8 raw bytes (little-endian)
#' @keywords internal
.write_int64 <- function(con, value) {
  # For values that fit in 32 bits, pack low 4 bytes + 4 zero bytes
  value <- as.numeric(value)
  lo <- as.integer(value %% 2^32)
  hi <- as.integer(value %/% 2^32)
  writeBin(lo, con, size = 4L, endian = "little")
  writeBin(hi, con, size = 4L, endian = "little")
}

#' Write a NIfTI-2 header (540 bytes, exact layout per spec)
#' @keywords internal
.write_nifti2_header <- function(con, intent, datatype, bitpix,
                                  nrows, ncols, vox_offset) {
  # Offset 0: sizeof_hdr (int32) = 540
  writeBin(540L, con, size = 4L, endian = "little")

  # Offset 4: magic[8] = "n+2\0\r\n\032\n"
  writeBin(as.raw(c(0x6e, 0x2b, 0x32, 0x00, 0x0d, 0x0a, 0x1a, 0x0a)), con)

  # Offset 12: datatype (int16)
  writeBin(as.integer(datatype), con, size = 2L, endian = "little")

  # Offset 14: bitpix (int16)
  writeBin(as.integer(bitpix), con, size = 2L, endian = "little")

  # Offset 16: dim[0..7] (int64 x 8 = 64 bytes)
  # CIFTI convention: dim[0]=6, dim[1..5]=1, dim[6]=ncols, dim[7]=nrows
  # (dim[6] is the number of data points per "row" of the matrix)
  dims <- c(6, 1, 1, 1, 1, 1, ncols, nrows)
  for (d in dims) .write_int64(con, d)

  # Offset 80: intent_p1, intent_p2, intent_p3 (double x 3 = 24 bytes)
  for (i in 1:3) writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 104: pixdim[0..7] (double x 8 = 64 bytes)
  for (i in 1:8) writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 168: vox_offset (int64)
  .write_int64(con, vox_offset)

  # Offset 176: scl_slope (double)
  writeBin(0.0, con, size = 8L, endian = "little")
  # Offset 184: scl_inter (double)
  writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 192: cal_max (double)
  writeBin(0.0, con, size = 8L, endian = "little")
  # Offset 200: cal_min (double)
  writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 208: slice_duration (double)
  writeBin(0.0, con, size = 8L, endian = "little")
  # Offset 216: toffset (double)
  writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 224: slice_start (int64)
  .write_int64(con, 0)
  # Offset 232: slice_end (int64)
  .write_int64(con, 0)

  # Offset 240: descrip[80]
  writeBin(raw(80), con)

  # Offset 320: aux_file[24]
  writeBin(raw(24), con)

  # Offset 344: qform_code (int32)
  writeBin(0L, con, size = 4L, endian = "little")
  # Offset 348: sform_code (int32)
  writeBin(0L, con, size = 4L, endian = "little")

  # Offset 352: quatern_b, quatern_c, quatern_d (double x 3)
  for (i in 1:3) writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 376: qoffset_x, qoffset_y, qoffset_z (double x 3)
  for (i in 1:3) writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 400: srow_x[4], srow_y[4], srow_z[4] (double x 12)
  for (i in 1:12) writeBin(0.0, con, size = 8L, endian = "little")

  # Offset 496: slice_code (int32)
  writeBin(0L, con, size = 4L, endian = "little")

  # Offset 500: xyzt_units (int32)
  writeBin(0L, con, size = 4L, endian = "little")

  # Offset 504: intent_code (int32)
  writeBin(as.integer(intent), con, size = 4L, endian = "little")

  # Offset 508: intent_name[16]
  writeBin(raw(16), con)

  # Offset 524: dim_info (1 byte)
  writeBin(as.raw(0), con)

  # Offset 525: unused_str[15] (padding)
  writeBin(raw(15), con)

  # Total: 540 bytes
  invisible(NULL)
}
