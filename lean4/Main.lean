/-
  Earcut Demo - Simple usage examples
-/

import Earcut

open Earcut

def main : IO Unit := do
  IO.println "=== Earcut Lean 4 Demo ==="
  IO.println ""

  -- Simple square
  IO.println "Triangulating a square (0,0), (1,0), (1,1), (0,1):"
  let square : Array Float := #[0, 0, 1, 0, 1, 1, 0, 1]
  let result := earcut square
  IO.println s!"  Triangle indices: {result.toList}"
  IO.println s!"  Number of triangles: {result.size / 3}"
  IO.println ""

  -- Pentagon
  IO.println "Triangulating a pentagon:"
  let pentagon : Array Float := #[0, 0.5, 0.2, 0, 0.8, 0, 1, 0.5, 0.5, 1]
  let result2 := earcut pentagon
  IO.println s!"  Triangle indices: {result2.toList}"
  IO.println s!"  Number of triangles: {result2.size / 3}"
  IO.println ""

  -- Square with hole
  IO.println "Triangulating a square with a hole:"
  let withHole : Array Float := #[
    0, 0, 4, 0, 4, 4, 0, 4,  -- outer ring
    1, 1, 1, 3, 3, 3, 3, 1   -- hole (counter-clockwise)
  ]
  let holes : Array Nat := #[4]  -- hole starts at vertex index 4
  let result3 := earcut withHole holes
  IO.println s!"  Triangle indices: {result3.toList}"
  IO.println s!"  Number of triangles: {result3.size / 3}"
  IO.println ""

  -- Deviation check
  IO.println "Verifying triangulation quality:"
  let dev := deviation square #[] 2 result
  IO.println s!"  Deviation for square: {dev}"
  IO.println s!"  (0 = perfect triangulation)"
  IO.println ""

  IO.println "Run `lake exe test` to run the full test suite."
