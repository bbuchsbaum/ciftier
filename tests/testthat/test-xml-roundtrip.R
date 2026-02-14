test_that("parse and rebuild a BRAIN_MODELS XML fragment", {
  xml_str <- '<?xml version="1.0"?>
<CIFTI Version="2" NumberOfMatrices="1">
  <Matrix>
    <MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES"
                      NumberOfSeriesPoints="100" SeriesExponent="0"
                      SeriesStart="0" SeriesStep="0.72" SeriesUnit="SECOND"/>
    <MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS">
      <BrainModel IndexOffset="0" IndexCount="3" ModelType="CIFTI_MODEL_TYPE_SURFACE"
                  BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" SurfaceNumberOfVertices="32492">
        <VertexIndices>0 1 2</VertexIndices>
      </BrainModel>
      <BrainModel IndexOffset="3" IndexCount="2" ModelType="CIFTI_MODEL_TYPE_VOXELS"
                  BrainStructure="CIFTI_STRUCTURE_THALAMUS_LEFT">
        <VoxelIndicesIJK>10 20 30
40 50 60</VoxelIndicesIJK>
      </BrainModel>
      <Volume VolumeDimensions="91,109,91">
        <TransformationMatrixVoxelIndicesIJKtoXYZ MeterExponent="-3">
1.0 0.0 0.0 -90.0
0.0 1.0 0.0 -126.0
0.0 0.0 1.0 -72.0
0.0 0.0 0.0 1.0</TransformationMatrixVoxelIndicesIJKtoXYZ>
      </Volume>
    </MatrixIndicesMap>
  </Matrix>
</CIFTI>'

  header <- parse_cifti_xml(xml_str)
  expect_s4_class(header, "CiftiHeader")
  expect_equal(header@version, "2")

  maps <- header@matrix@maps
  expect_length(maps, 2)

  # Series map
  series_map <- maps[[1]]
  expect_equal(series_map@index_type, "CIFTI_INDEX_TYPE_SERIES")
  expect_equal(series_map@number_of_series_points, 100L)
  expect_equal(series_map@series_step, 0.72)
  expect_equal(series_map@series_unit, "SECOND")

  # Brain models map
  bm_map <- maps[[2]]
  expect_equal(bm_map@index_type, "CIFTI_INDEX_TYPE_BRAIN_MODELS")
  expect_length(bm_map@brain_models, 2)

  # Surface model
  surf <- bm_map@brain_models[[1]]
  expect_equal(surf@model_type, "CIFTI_MODEL_TYPE_SURFACE")
  expect_equal(surf@brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(surf@index_count, 3L)
  expect_equal(surf@vertex_indices, c(0L, 1L, 2L))
  expect_equal(surf@surface_number_of_vertices, 32492L)

  # Voxel model
  vox <- bm_map@brain_models[[2]]
  expect_equal(vox@model_type, "CIFTI_MODEL_TYPE_VOXELS")
  expect_equal(vox@brain_structure, "CIFTI_STRUCTURE_THALAMUS_LEFT")
  expect_equal(vox@index_count, 2L)
  expect_equal(nrow(vox@voxel_indices_ijk), 2)
  expect_equal(vox@voxel_indices_ijk[1, ], c(10L, 20L, 30L))

  # Volume
  vol <- bm_map@volume
  expect_s4_class(vol, "CiftiVolume")
  expect_equal(vol@dimensions, c(91L, 109L, 91L))
  expect_equal(vol@transform[1, 4], -90.0)

  # Round-trip: build XML and re-parse
  xml_out <- build_cifti_xml(header)
  header2 <- parse_cifti_xml(xml_out)

  # Verify structural equality
  maps2 <- header2@matrix@maps
  expect_length(maps2, 2)

  # Series preserved
  s2 <- maps2[[1]]
  expect_equal(s2@number_of_series_points, 100L)
  expect_equal(s2@series_step, 0.72)

  # Brain models preserved
  bm2 <- maps2[[2]]
  expect_length(bm2@brain_models, 2)
  expect_equal(bm2@brain_models[[1]]@vertex_indices, c(0L, 1L, 2L))
  expect_equal(bm2@brain_models[[2]]@voxel_indices_ijk[2, ], c(40L, 50L, 60L))
})


test_that("parse and rebuild SCALARS with named maps", {
  xml_str <- '<?xml version="1.0"?>
<CIFTI Version="2">
  <Matrix>
    <MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS">
      <NamedMap>
        <MapName>thickness</MapName>
      </NamedMap>
      <NamedMap>
        <MapName>curvature</MapName>
      </NamedMap>
    </MatrixIndicesMap>
    <MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS">
      <BrainModel IndexOffset="0" IndexCount="2" ModelType="CIFTI_MODEL_TYPE_SURFACE"
                  BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" SurfaceNumberOfVertices="100">
        <VertexIndices>10 20</VertexIndices>
      </BrainModel>
    </MatrixIndicesMap>
  </Matrix>
</CIFTI>'

  header <- parse_cifti_xml(xml_str)
  maps <- header@matrix@maps

  scalar_map <- maps[[1]]
  expect_equal(scalar_map@index_type, "CIFTI_INDEX_TYPE_SCALARS")
  expect_length(scalar_map@named_maps, 2)
  expect_equal(scalar_map@named_maps[[1]]@map_name, "thickness")
  expect_equal(scalar_map@named_maps[[2]]@map_name, "curvature")

  # Round-trip
  xml_out <- build_cifti_xml(header)
  header2 <- parse_cifti_xml(xml_out)
  expect_equal(header2@matrix@maps[[1]]@named_maps[[1]]@map_name, "thickness")
})


test_that("parse and rebuild LABELS with label tables", {
  xml_str <- '<?xml version="1.0"?>
<CIFTI Version="2">
  <Matrix>
    <MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_LABELS">
      <NamedMap>
        <MapName>parcellation</MapName>
        <LabelTable>
          <Label Key="0" Red="1" Green="1" Blue="1" Alpha="0">???</Label>
          <Label Key="1" Red="1" Green="0" Blue="0" Alpha="1">V1</Label>
          <Label Key="2" Red="0" Green="1" Blue="0" Alpha="1">V2</Label>
        </LabelTable>
      </NamedMap>
    </MatrixIndicesMap>
    <MatrixIndicesMap AppliesToMatrixDimension="1" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS">
      <BrainModel IndexOffset="0" IndexCount="5" ModelType="CIFTI_MODEL_TYPE_SURFACE"
                  BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" SurfaceNumberOfVertices="1000">
        <VertexIndices>0 1 2 3 4</VertexIndices>
      </BrainModel>
    </MatrixIndicesMap>
  </Matrix>
</CIFTI>'

  header <- parse_cifti_xml(xml_str)
  label_map <- header@matrix@maps[[1]]
  expect_equal(label_map@index_type, "CIFTI_INDEX_TYPE_LABELS")

  nm <- label_map@named_maps[[1]]
  expect_equal(nm@map_name, "parcellation")
  expect_length(nm@label_table@labels, 3)
  expect_equal(nm@label_table@labels[[2]]@label, "V1")
  expect_equal(nm@label_table@labels[[2]]@key, 1L)
  expect_equal(nm@label_table@labels[[2]]@red, 1)
  expect_equal(nm@label_table@labels[[2]]@green, 0)

  # Round-trip
  xml_out <- build_cifti_xml(header)
  header2 <- parse_cifti_xml(xml_out)
  nm2 <- header2@matrix@maps[[1]]@named_maps[[1]]
  expect_equal(nm2@label_table@labels[[2]]@label, "V1")
})


test_that("parse PARCELS with surface vertices and voxels", {
  xml_str <- '<?xml version="1.0"?>
<CIFTI Version="2">
  <Matrix>
    <MatrixIndicesMap AppliesToMatrixDimension="0" IndicesMapToDataType="CIFTI_INDEX_TYPE_PARCELS">
      <Surface BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" NumberOfVertices="32492"/>
      <Parcel Name="FrontalPole">
        <Vertices BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT">100 200 300</Vertices>
      </Parcel>
      <Parcel Name="Thalamus">
        <VoxelIndicesIJK>10 20 30
11 21 31</VoxelIndicesIJK>
      </Parcel>
      <Volume VolumeDimensions="91,109,91">
        <TransformationMatrixVoxelIndicesIJKtoXYZ MeterExponent="-3">
1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1</TransformationMatrixVoxelIndicesIJKtoXYZ>
      </Volume>
    </MatrixIndicesMap>
  </Matrix>
</CIFTI>'

  header <- parse_cifti_xml(xml_str)
  mim <- header@matrix@maps[[1]]
  expect_equal(mim@index_type, "CIFTI_INDEX_TYPE_PARCELS")

  expect_length(mim@surfaces, 1)
  expect_equal(mim@surfaces[[1]]@brain_structure, "CIFTI_STRUCTURE_CORTEX_LEFT")
  expect_equal(mim@surfaces[[1]]@number_of_vertices, 32492L)

  expect_length(mim@parcels, 2)
  expect_equal(mim@parcels[[1]]@name, "FrontalPole")
  expect_equal(mim@parcels[[1]]@surface_vertices[["CIFTI_STRUCTURE_CORTEX_LEFT"]],
               c(100L, 200L, 300L))

  expect_equal(mim@parcels[[2]]@name, "Thalamus")
  expect_equal(nrow(mim@parcels[[2]]@voxel_indices_ijk), 2)
})
