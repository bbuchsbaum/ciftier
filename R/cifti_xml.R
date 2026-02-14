#' @include utils.R
NULL

# ============================================================================
# CIFTI-2 XML Parsing
# ============================================================================

#' Parse CIFTI-2 XML Header
#'
#' Parses raw XML bytes (or a character string) from a CIFTI-2 NIfTI extension
#' into a \code{\linkS4class{CiftiHeader}} object.
#'
#' @param xml_data Raw vector, character string, or xml2 document.
#' @return A \code{\linkS4class{CiftiHeader}} object.
#'
#' @export
parse_cifti_xml <- function(xml_data) {
  if (is.raw(xml_data)) {
    # Strip trailing nulls
    nz <- which(xml_data != as.raw(0))
    if (length(nz) > 0) {
      xml_data <- xml_data[seq_len(max(nz))]
    }
    xml_data <- rawToChar(xml_data)
  }

  if (is.character(xml_data)) {
    doc <- xml2::read_xml(xml_data)
  } else if (inherits(xml_data, "xml_document") || inherits(xml_data, "xml_node")) {
    doc <- xml_data
  } else {
    stop("xml_data must be raw, character, or xml2 document")
  }

  xml2::xml_ns_strip(doc)

  root <- doc
  root_name <- xml2::xml_name(root)
  if (root_name != "CIFTI") {
    stop(sprintf("Expected root element 'CIFTI', got '%s'", root_name))
  }

  version <- xml2::xml_attr(root, "Version") %||% "2"

  # Parse Matrix element
  matrix_node <- xml2::xml_find_first(root, ".//Matrix")
  if (is.na(matrix_node)) {
    stop("No <Matrix> element found in CIFTI XML")
  }

  cifti_matrix <- .parse_matrix_node(matrix_node)

  new("CiftiHeader", version = version, matrix = cifti_matrix)
}


#' Build CIFTI-2 XML from Header
#'
#' Converts a \code{\linkS4class{CiftiHeader}} to an XML character string.
#'
#' @param header A \code{\linkS4class{CiftiHeader}} object.
#' @return A character string containing the CIFTI-2 XML.
#'
#' @export
build_cifti_xml <- function(header) {
  stopifnot(is(header, "CiftiHeader"))

  doc <- xml2::xml_new_root("CIFTI",
    Version = header@version,
    NumberOfMatrices = "1"
  )

  matrix_node <- xml2::xml_add_child(doc, "Matrix")

  # Add metadata if present
  if (length(header@matrix@metadata) > 0) {
    md_node <- xml2::xml_add_child(matrix_node, "MetaData")
    for (nm in names(header@matrix@metadata)) {
      md_entry <- xml2::xml_add_child(md_node, "MD")
      xml2::xml_add_child(md_entry, "Name", nm)
      xml2::xml_add_child(md_entry, "Value", as.character(header@matrix@metadata[[nm]]))
    }
  }

  # Add each MatrixIndicesMap

  for (mim in header@matrix@maps) {
    .build_matrix_indices_map(matrix_node, mim)
  }

  as.character(doc)
}


# ============================================================================
# Internal XML Parsing Helpers
# ============================================================================

#' Null-coalescing operator
#' @param a First value.
#' @param b Fallback value.
#' @return \code{a} if non-NULL and non-NA, else \code{b}.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' Parse <Matrix> node
#' @keywords internal
.parse_matrix_node <- function(node) {
  # Parse metadata
  metadata <- .parse_metadata(node)

  # Parse MatrixIndicesMap elements
  mim_nodes <- xml2::xml_find_all(node, "MatrixIndicesMap")
  maps <- lapply(mim_nodes, .parse_matrix_indices_map)

  new("CiftiMatrix", maps = maps, metadata = metadata)
}


#' Parse <MetaData> from a node
#' @keywords internal
.parse_metadata <- function(node) {
  md_node <- xml2::xml_find_first(node, "MetaData")
  if (is.na(md_node)) return(list())

  entries <- xml2::xml_find_all(md_node, "MD")
  if (length(entries) == 0) return(list())

  keys <- vapply(entries, function(e) {
    xml2::xml_text(xml2::xml_find_first(e, "Name"))
  }, character(1))

  vals <- vapply(entries, function(e) {
    xml2::xml_text(xml2::xml_find_first(e, "Value"))
  }, character(1))

  stats::setNames(as.list(vals), keys)
}


#' Parse <MatrixIndicesMap>
#' @keywords internal
.parse_matrix_indices_map <- function(node) {
  # Dimension indices (comma-separated, 0-based)
  dim_str <- xml2::xml_attr(node, "AppliesToMatrixDimension")
  dims <- as.integer(strsplit(dim_str, ",")[[1]])

  index_type <- xml2::xml_attr(node, "IndicesMapToDataType")

  mim <- new("CiftiMatrixIndicesMap",
    applies_to_dimension = dims,
    index_type = index_type
  )

  # Parse Volume element if present
  vol_node <- xml2::xml_find_first(node, "Volume")
  if (!is.na(vol_node)) {
    mim@volume <- .parse_volume(vol_node)
  }

  # Type-specific parsing
  if (index_type == "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    bm_nodes <- xml2::xml_find_all(node, "BrainModel")
    mim@brain_models <- lapply(bm_nodes, .parse_brain_model)

  } else if (index_type == "CIFTI_INDEX_TYPE_PARCELS") {
    parcel_nodes <- xml2::xml_find_all(node, "Parcel")
    mim@parcels <- lapply(parcel_nodes, .parse_parcel)
    surf_nodes <- xml2::xml_find_all(node, "Surface")
    mim@surfaces <- lapply(surf_nodes, .parse_surface)

  } else if (index_type == "CIFTI_INDEX_TYPE_SERIES") {
    mim@series_start <- as.numeric(xml2::xml_attr(node, "SeriesStart") %||% "0")
    mim@series_step <- as.numeric(xml2::xml_attr(node, "SeriesStep") %||% "1")
    mim@series_unit <- xml2::xml_attr(node, "SeriesUnit") %||% "SECOND"
    mim@number_of_series_points <- as.integer(xml2::xml_attr(node, "NumberOfSeriesPoints") %||% "0")

  } else if (index_type %in% c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS")) {
    nm_nodes <- xml2::xml_find_all(node, "NamedMap")
    mim@named_maps <- lapply(nm_nodes, .parse_named_map)
  }

  mim
}


#' Parse <Volume>
#' @keywords internal
.parse_volume <- function(node) {
  dim_str <- xml2::xml_attr(node, "VolumeDimensions")
  dims <- as.integer(strsplit(dim_str, ",")[[1]])

  # Parse TransformationMatrixVoxelIndicesIJKtoXYZ
  tf_node <- xml2::xml_find_first(node, "TransformationMatrixVoxelIndicesIJKtoXYZ")
  if (!is.na(tf_node)) {
    tf_text <- trimws(xml2::xml_text(tf_node))
    tf_vals <- as.numeric(strsplit(tf_text, "\\s+")[[1]])
    transform <- matrix(tf_vals, nrow = 4, ncol = 4, byrow = TRUE)
  } else {
    transform <- diag(4)
  }

  new("CiftiVolume", dimensions = dims, transform = transform)
}


#' Parse <BrainModel>
#' @keywords internal
.parse_brain_model <- function(node) {
  offset <- as.integer(xml2::xml_attr(node, "IndexOffset"))
  count  <- as.integer(xml2::xml_attr(node, "IndexCount"))
  mtype  <- xml2::xml_attr(node, "ModelType")
  bstruct <- xml2::xml_attr(node, "BrainStructure")

  nvert <- as.integer(xml2::xml_attr(node, "SurfaceNumberOfVertices") %||% "0")

  vertex_indices <- integer(0)
  voxel_ijk <- matrix(integer(0), ncol = 3)

  if (mtype == "CIFTI_MODEL_TYPE_SURFACE") {
    vi_node <- xml2::xml_find_first(node, "VertexIndices")
    if (!is.na(vi_node)) {
      vi_text <- trimws(xml2::xml_text(vi_node))
      if (nchar(vi_text) > 0) {
        vertex_indices <- as.integer(strsplit(vi_text, "\\s+")[[1]])
      }
    }
  } else {
    vij_node <- xml2::xml_find_first(node, "VoxelIndicesIJK")
    if (!is.na(vij_node)) {
      vij_text <- trimws(xml2::xml_text(vij_node))
      if (nchar(vij_text) > 0) {
        vals <- as.integer(strsplit(vij_text, "\\s+")[[1]])
        voxel_ijk <- matrix(vals, ncol = 3, byrow = TRUE)
      }
    }
  }

  new("CiftiBrainModel",
    index_offset = offset,
    index_count = count,
    model_type = mtype,
    brain_structure = bstruct,
    surface_number_of_vertices = nvert,
    vertex_indices = vertex_indices,
    voxel_indices_ijk = voxel_ijk
  )
}


#' Parse <Parcel>
#' @keywords internal
.parse_parcel <- function(node) {
  name <- xml2::xml_attr(node, "Name")

  # Surface vertices
  vert_nodes <- xml2::xml_find_all(node, "Vertices")
  surface_vertices <- list()
  for (vn in vert_nodes) {
    struct <- xml2::xml_attr(vn, "BrainStructure")
    vtxt <- trimws(xml2::xml_text(vn))
    if (nchar(vtxt) > 0) {
      surface_vertices[[struct]] <- as.integer(strsplit(vtxt, "\\s+")[[1]])
    } else {
      surface_vertices[[struct]] <- integer(0)
    }
  }

  # Voxel indices
  vij_node <- xml2::xml_find_first(node, "VoxelIndicesIJK")
  voxel_ijk <- matrix(integer(0), ncol = 3)
  if (!is.na(vij_node)) {
    vij_text <- trimws(xml2::xml_text(vij_node))
    if (nchar(vij_text) > 0) {
      vals <- as.integer(strsplit(vij_text, "\\s+")[[1]])
      voxel_ijk <- matrix(vals, ncol = 3, byrow = TRUE)
    }
  }

  new("CiftiParcel",
    name = name,
    surface_vertices = surface_vertices,
    voxel_indices_ijk = voxel_ijk
  )
}


#' Parse <Surface>
#' @keywords internal
.parse_surface <- function(node) {
  new("CiftiSurface",
    brain_structure = xml2::xml_attr(node, "BrainStructure"),
    number_of_vertices = as.integer(xml2::xml_attr(node, "NumberOfVertices"))
  )
}


#' Parse <NamedMap>
#' @keywords internal
.parse_named_map <- function(node) {
  mn_node <- xml2::xml_find_first(node, "MapName")
  map_name <- if (!is.na(mn_node)) xml2::xml_text(mn_node) else ""

  metadata <- .parse_metadata(node)

  # Parse LabelTable if present
  lt_node <- xml2::xml_find_first(node, "LabelTable")
  label_table <- new("CiftiLabelTable")
  if (!is.na(lt_node)) {
    lab_nodes <- xml2::xml_find_all(lt_node, "Label")
    labels <- lapply(lab_nodes, function(ln) {
      new("CiftiLabel",
        key   = as.integer(xml2::xml_attr(ln, "Key")),
        label = xml2::xml_text(ln),
        red   = as.numeric(xml2::xml_attr(ln, "Red") %||% "1"),
        green = as.numeric(xml2::xml_attr(ln, "Green") %||% "1"),
        blue  = as.numeric(xml2::xml_attr(ln, "Blue") %||% "1"),
        alpha = as.numeric(xml2::xml_attr(ln, "Alpha") %||% "1")
      )
    })
    label_table <- new("CiftiLabelTable", labels = labels)
  }

  new("CiftiNamedMap",
    map_name = map_name,
    label_table = label_table,
    metadata = metadata
  )
}


# ============================================================================
# Internal XML Building Helpers
# ============================================================================

#' Build <MatrixIndicesMap> XML node
#' @keywords internal
.build_matrix_indices_map <- function(parent, mim) {
  dim_str <- paste(mim@applies_to_dimension, collapse = ",")

  attrs <- list(
    AppliesToMatrixDimension = dim_str,
    IndicesMapToDataType = mim@index_type
  )

  if (mim@index_type == "CIFTI_INDEX_TYPE_SERIES") {
    attrs$NumberOfSeriesPoints <- as.character(mim@number_of_series_points)
    attrs$SeriesExponent <- "0"
    attrs$SeriesStart <- as.character(mim@series_start)
    attrs$SeriesStep <- as.character(mim@series_step)
    attrs$SeriesUnit <- mim@series_unit
  }

  mim_node <- xml2::xml_add_child(parent, "MatrixIndicesMap")
  for (nm in names(attrs)) {
    xml2::xml_set_attr(mim_node, nm, attrs[[nm]])
  }

  # Volume
  if (!is.null(mim@volume)) {
    .build_volume(mim_node, mim@volume)
  }

  # Type-specific children
  if (mim@index_type == "CIFTI_INDEX_TYPE_BRAIN_MODELS") {
    for (bm in mim@brain_models) {
      .build_brain_model(mim_node, bm)
    }

  } else if (mim@index_type == "CIFTI_INDEX_TYPE_PARCELS") {
    for (surf in mim@surfaces) {
      sn <- xml2::xml_add_child(mim_node, "Surface")
      xml2::xml_set_attr(sn, "BrainStructure", surf@brain_structure)
      xml2::xml_set_attr(sn, "NumberOfVertices", as.character(surf@number_of_vertices))
    }
    for (p in mim@parcels) {
      .build_parcel(mim_node, p)
    }

  } else if (mim@index_type %in% c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_LABELS")) {
    for (nm_map in mim@named_maps) {
      .build_named_map(mim_node, nm_map)
    }
  }

  invisible(mim_node)
}


#' Build <Volume> XML node
#' @keywords internal
.build_volume <- function(parent, vol) {
  vol_node <- xml2::xml_add_child(parent, "Volume")
  xml2::xml_set_attr(vol_node, "VolumeDimensions",
    paste(vol@dimensions, collapse = ","))

  tf_node <- xml2::xml_add_child(vol_node,
    "TransformationMatrixVoxelIndicesIJKtoXYZ")
  xml2::xml_set_attr(tf_node, "MeterExponent", "-3")

  # Write transform row by row
  tf_text <- paste(apply(vol@transform, 1, function(row) {
    paste(sprintf("%.10f", row), collapse = " ")
  }), collapse = "\n")
  xml2::xml_set_text(tf_node, tf_text)

  invisible(vol_node)
}


#' Build <BrainModel> XML node
#' @keywords internal
.build_brain_model <- function(parent, bm) {
  bm_node <- xml2::xml_add_child(parent, "BrainModel")
  xml2::xml_set_attr(bm_node, "IndexOffset", as.character(bm@index_offset))
  xml2::xml_set_attr(bm_node, "IndexCount", as.character(bm@index_count))
  xml2::xml_set_attr(bm_node, "ModelType", bm@model_type)
  xml2::xml_set_attr(bm_node, "BrainStructure", bm@brain_structure)

  if (bm@model_type == "CIFTI_MODEL_TYPE_SURFACE") {
    xml2::xml_set_attr(bm_node, "SurfaceNumberOfVertices",
      as.character(bm@surface_number_of_vertices))
    if (length(bm@vertex_indices) > 0) {
      vi_node <- xml2::xml_add_child(bm_node, "VertexIndices")
      xml2::xml_set_text(vi_node, paste(bm@vertex_indices, collapse = " "))
    }
  } else {
    if (nrow(bm@voxel_indices_ijk) > 0) {
      vij_node <- xml2::xml_add_child(bm_node, "VoxelIndicesIJK")
      vij_text <- paste(apply(bm@voxel_indices_ijk, 1, function(row) {
        paste(row, collapse = " ")
      }), collapse = "\n")
      xml2::xml_set_text(vij_node, vij_text)
    }
  }

  invisible(bm_node)
}


#' Build <Parcel> XML node
#' @keywords internal
.build_parcel <- function(parent, parcel) {
  p_node <- xml2::xml_add_child(parent, "Parcel")
  xml2::xml_set_attr(p_node, "Name", parcel@name)

  # Voxels
  if (nrow(parcel@voxel_indices_ijk) > 0) {
    vij_node <- xml2::xml_add_child(p_node, "VoxelIndicesIJK")
    vij_text <- paste(apply(parcel@voxel_indices_ijk, 1, function(row) {
      paste(row, collapse = " ")
    }), collapse = "\n")
    xml2::xml_set_text(vij_node, vij_text)
  }

  # Surface vertices
  for (struct in names(parcel@surface_vertices)) {
    vn <- xml2::xml_add_child(p_node, "Vertices")
    xml2::xml_set_attr(vn, "BrainStructure", struct)
    xml2::xml_set_text(vn, paste(parcel@surface_vertices[[struct]], collapse = " "))
  }

  invisible(p_node)
}


#' Build <NamedMap> XML node
#' @keywords internal
.build_named_map <- function(parent, nm) {
  nm_node <- xml2::xml_add_child(parent, "NamedMap")

  # MetaData
  if (length(nm@metadata) > 0) {
    md_node <- xml2::xml_add_child(nm_node, "MetaData")
    for (k in names(nm@metadata)) {
      md_entry <- xml2::xml_add_child(md_node, "MD")
      xml2::xml_add_child(md_entry, "Name", k)
      xml2::xml_add_child(md_entry, "Value", as.character(nm@metadata[[k]]))
    }
  }

  # MapName
  mn_node <- xml2::xml_add_child(nm_node, "MapName")
  xml2::xml_set_text(mn_node, nm@map_name)

  # LabelTable
  if (length(nm@label_table@labels) > 0) {
    lt_node <- xml2::xml_add_child(nm_node, "LabelTable")
    for (lab in nm@label_table@labels) {
      ln <- xml2::xml_add_child(lt_node, "Label")
      xml2::xml_set_attr(ln, "Key", as.character(lab@key))
      xml2::xml_set_attr(ln, "Red", as.character(lab@red))
      xml2::xml_set_attr(ln, "Green", as.character(lab@green))
      xml2::xml_set_attr(ln, "Blue", as.character(lab@blue))
      xml2::xml_set_attr(ln, "Alpha", as.character(lab@alpha))
      xml2::xml_set_text(ln, lab@label)
    }
  }

  invisible(nm_node)
}
