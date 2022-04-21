# README for `vtk-haskell`

Helpers and data-types for exporting meshes to VTK (VTU) files.

# Mesh Data Structures

```haskell
-- TODO:
data VTK = VTU | VTS

-- | Data type for an @UnstructuredGrid@.
--   TODO:
--    + non-default attributes;
--    + proper support for multiple pieces;
--    + 3D "meshes";
data VTU = VTU [Piece]
```

# Future Work

Support for:

+ '`Appended`' data-array format
+ VTK files with multiple _pieces_
+ 3D meshes
+ more polygon (`n`-gon) types than just `d`-cubes
+ structured-grid (VTS) formats and files
+ parallel mesh I/O
+ parallel mesh ghost-levels

Testbenches for:

+ accurate storage of `Float32` and `Float64` values
+ point coordinates
+ point data
+ cell connectivities
+ cell offsets
+ cell types
+ cell data
