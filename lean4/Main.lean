import Earcut

open Earcut

/-- Test helper to print results -/
def printTriangles (name : String) (triangles : Array Nat) : IO Unit := do
  IO.println s!"{name}: {triangles.size / 3} triangles"
  IO.println s!"  Indices: {triangles.toList}"

/-- Simple square test -/
def testSquare : IO Unit := do
  -- Square: (0,0), (1,0), (1,1), (0,1)
  let data : Array Float := #[0, 0, 1, 0, 1, 1, 0, 1]
  let triangles := earcut data
  printTriangles "Square" triangles
  -- Expected: 2 triangles

/-- Triangle test -/
def testTriangle : IO Unit := do
  -- Simple triangle
  let data : Array Float := #[0, 0, 1, 0, 0.5, 1]
  let triangles := earcut data
  printTriangles "Triangle" triangles
  -- Expected: 1 triangle

/-- Pentagon test -/
def testPentagon : IO Unit := do
  -- Regular pentagon (approximate)
  let data : Array Float := #[0, 0.5, 0.2, 0, 0.8, 0, 1, 0.5, 0.5, 1]
  let triangles := earcut data
  printTriangles "Pentagon" triangles
  -- Expected: 3 triangles

/-- L-shape test -/
def testLShape : IO Unit := do
  -- L-shaped polygon
  let data : Array Float := #[0, 0, 2, 0, 2, 1, 1, 1, 1, 2, 0, 2]
  let triangles := earcut data
  printTriangles "L-shape" triangles
  -- Expected: 4 triangles

/-- Square with hole test -/
def testSquareWithHole : IO Unit := do
  -- Outer square: (0,0), (4,0), (4,4), (0,4)
  -- Inner hole (square): (1,1), (3,1), (3,3), (1,3)
  let data : Array Float := #[
    0, 0, 4, 0, 4, 4, 0, 4,  -- outer ring
    1, 1, 1, 3, 3, 3, 3, 1   -- hole (counter-clockwise)
  ]
  let holeIndices : Array Nat := #[4]  -- hole starts at index 4
  let triangles := earcut data holeIndices
  printTriangles "Square with hole" triangles
  -- Expected: 8 triangles

/-- Test deviation calculation -/
def testDeviation : IO Unit := do
  let data : Array Float := #[0, 0, 1, 0, 1, 1, 0, 1]
  let triangles := earcut data
  let dev := deviation data #[] 2 triangles
  IO.println s!"Deviation for square: {dev}"
  -- Expected: 0 or very close to 0

/-- Test flatten function -/
def testFlatten : IO Unit := do
  let nested : Array (Array (Array Float)) := #[
    #[#[0, 0], #[1, 0], #[1, 1], #[0, 1]],  -- outer ring
    #[#[0.2, 0.2], #[0.8, 0.2], #[0.8, 0.8], #[0.2, 0.8]]  -- hole
  ]
  let (vertices, holes, dims) := flatten nested
  IO.println s!"Flatten test:"
  IO.println s!"  Vertices: {vertices.toList}"
  IO.println s!"  Holes: {holes.toList}"
  IO.println s!"  Dimensions: {dims}"

def main : IO Unit := do
  IO.println "=== Earcut Lean 4 Port Tests ==="
  IO.println ""

  IO.println "--- Basic Polygon Tests ---"
  testTriangle
  IO.println ""
  testSquare
  IO.println ""
  testPentagon
  IO.println ""
  testLShape
  IO.println ""

  IO.println "--- Polygon with Hole Test ---"
  testSquareWithHole
  IO.println ""

  IO.println "--- Deviation Test ---"
  testDeviation
  IO.println ""

  IO.println "--- Flatten Test ---"
  testFlatten
  IO.println ""

  IO.println "=== All tests completed ==="
