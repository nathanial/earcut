# Earcut for Lean 4

A Lean 4 port of the [earcut](https://github.com/mapbox/earcut) polygon triangulation library.

## Overview

Earcut is a fast, small library for triangulating 2D polygons using the ear clipping algorithm. This port provides the same functionality in Lean 4, suitable for use in verified computational geometry applications.

## Features

- Triangulates simple polygons and polygons with holes
- Uses z-order curve hashing for optimization on large polygons
- Handles degenerate cases gracefully
- Provides deviation calculation for triangulation quality verification

## Usage

### Basic Triangulation

```lean
import Earcut

-- Triangulate a square
let data : Array Float := #[0, 0, 1, 0, 1, 1, 0, 1]
let triangles := Earcut.earcut data
-- Returns: #[1, 2, 3, 3, 0, 1] (indices into the vertex array)
```

### Polygon with Holes

```lean
import Earcut

-- Outer square with inner hole
let data : Array Float := #[
  0, 0, 4, 0, 4, 4, 0, 4,  -- outer ring (clockwise)
  1, 1, 1, 3, 3, 3, 3, 1   -- hole (counter-clockwise)
]
let holeIndices : Array Nat := #[4]  -- hole starts at vertex index 4
let triangles := Earcut.earcut data holeIndices
```

### GeoJSON-style Input

```lean
import Earcut

-- Nested array format (like GeoJSON coordinates)
let nested : Array (Array (Array Float)) := #[
  #[#[0, 0], #[1, 0], #[1, 1], #[0, 1]],  -- outer ring
  #[#[0.2, 0.2], #[0.8, 0.2], #[0.8, 0.8], #[0.2, 0.8]]  -- hole
]
let (vertices, holes, dims) := Earcut.flatten nested
let triangles := Earcut.earcut vertices holes dims
```

### Verify Triangulation Quality

```lean
import Earcut

let data : Array Float := #[0, 0, 1, 0, 1, 1, 0, 1]
let triangles := Earcut.earcut data
let dev := Earcut.deviation data #[] 2 triangles
-- Returns 0 for perfect triangulation
```

## API Reference

### `earcut`

```lean
def earcut (data : Array Float) (holeIndices : Array Nat := #[]) (dim : Nat := 2) : Array Nat
```

Main triangulation function.

- `data`: Flat array of vertex coordinates (x, y, x, y, ...)
- `holeIndices`: Array of vertex indices where each hole starts
- `dim`: Number of coordinates per vertex (default 2 for 2D)
- Returns: Array of triangle vertex indices

### `deviation`

```lean
def deviation (data : Array Float) (holeIndices : Array Nat) (dim : Nat) (triangles : Array Nat) : Float
```

Calculate the percentage difference between polygon area and triangulation area. Returns 0 for a perfect triangulation.

### `flatten`

```lean
def flatten (data : Array (Array (Array Float))) : Array Float × Array Nat × Nat
```

Convert nested polygon format (like GeoJSON) into flat format for `earcut`.

## Building

Requires Lean 4 (v4.3.0 or compatible).

```bash
cd lean4
lake build
```

## Running Demo

```bash
lake exe earcut
```

## Running Tests

The test suite includes 17+ fixture-based tests from the original JavaScript implementation, each tested at 4 rotations (0°, 90°, 180°, 270°):

```bash
lake exe test
```

Test fixtures include:
- Basic polygons (building, indices-2d, indices-3d)
- Polygons with holes (dude, issue16, issue17, steiner)
- Edge cases (degenerate, empty-square, hourglass)
- Complex cases (water3b, touching holes, bad-diagonals)
- Regression tests (issue142, issue149, infinite-loop)

## Algorithm

The algorithm is based on:

1. **Ear Clipping**: Iteratively removes "ears" (triangles) from the polygon
2. **Z-order Curve Hashing**: For large polygons, uses spatial hashing to speed up point-in-triangle checks
3. **Hole Elimination**: Connects holes to the outer polygon using bridge edges (David Eberly's algorithm)

## License

ISC License (same as original JavaScript implementation)

## Credits

- Original JavaScript implementation by Vladimir Agafonkin (Mapbox)
- Lean 4 port maintains the same algorithmic approach
