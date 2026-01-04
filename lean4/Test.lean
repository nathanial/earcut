/-
  Earcut Test Suite - Lean 4 port of the JavaScript test suite
  Tests polygon triangulation with various fixtures and edge cases
-/

import Earcut

open Earcut

/-- Test result type -/
structure TestResult where
  name : String
  passed : Bool
  message : String
  deriving Repr

/-- Expected test values -/
structure Expected where
  triangles : Nat
  maxDeviation : Float := 0
  deriving Repr

/-- Flatten nested coordinates to flat array with hole indices -/
def flattenCoords (rings : List (List (Float × Float))) : Array Float × Array Nat := Id.run do
  let mut vertices : Array Float := #[]
  let mut holes : Array Nat := #[]
  let mut idx : Nat := 0

  for (i, ring) in rings.enum do
    if i > 0 then
      holes := holes.push idx
    for (x, y) in ring do
      vertices := vertices.push x
      vertices := vertices.push y
      idx := idx + 1

  (vertices, holes)

/-- Rotate coordinates by angle (in degrees) -/
def rotateCoords (rings : List (List (Float × Float))) (degrees : Float)
    : List (List (Float × Float)) := Id.run do
  let theta := degrees * Float.pi / 180
  let cosT := Float.cos theta
  let sinT := Float.sin theta
  -- Round to avoid floating point issues
  let xx := Float.round cosT
  let xy := Float.round (-sinT)
  let yx := Float.round sinT
  let yy := Float.round cosT

  rings.map fun ring =>
    ring.map fun (x, y) =>
      (xx * x + xy * y, yx * x + yy * y)

/-- Run a single test case -/
def runTest (name : String) (coords : List (List (Float × Float)))
    (expected : Expected) (rotation : Float := 0) : TestResult := Id.run do
  let rotated := if rotation == 0 then coords else rotateCoords coords rotation
  let (vertices, holes) := flattenCoords rotated
  let triangles := earcut vertices holes

  let numTriangles := triangles.size / 3

  -- Check triangle count (only for rotation 0)
  if rotation == 0 && numTriangles != expected.triangles then
    return {
      name := s!"{name} (rotation {rotation})"
      passed := false
      message := s!"Expected {expected.triangles} triangles, got {numTriangles}"
    }

  -- Check deviation if we have triangles
  if expected.triangles > 0 then
    let dev := deviation vertices holes 2 triangles
    if dev > expected.maxDeviation then
      return {
        name := s!"{name} (rotation {rotation})"
        passed := false
        message := s!"Deviation {dev} > {expected.maxDeviation}"
      }

  return {
    name := s!"{name} (rotation {rotation})"
    passed := true
    message := "OK"
  }

/-- Run test with all rotations -/
def runTestWithRotations (name : String) (coords : List (List (Float × Float)))
    (expected : Expected) (rotatedDeviation : Option Float := none) : List TestResult :=
  let rotations : List Float := [0, 90, 180, 270]
  rotations.map fun r =>
    let exp := if r != 0 then
      match rotatedDeviation with
      | some d => { expected with maxDeviation := d }
      | none => expected
    else expected
    runTest name coords exp r

-- ============================================================================
-- Test Fixtures
-- ============================================================================

/-- Simple 2D indices test -/
def fixtureIndices2D : List (List (Float × Float)) :=
  [[(10, 0), (0, 50), (60, 60), (70, 10)]]

/-- Building fixture -/
def fixtureBuilding : List (List (Float × Float)) :=
  [[(661,112),(661,96),(666,96),(666,87),(743,87),(771,87),(771,114),(750,114),(750,113),(742,113),(742,106),(710,106),(710,113),(666,113),(666,112)]]

/-- Dude fixture (with holes) -/
def fixtureDude : List (List (Float × Float)) :=
  [-- outer ring
   [(280.35714, 648.79075),(286.78571, 662.8979),(263.28607, 661.17871),(262.31092, 671.41548),(250.53571, 677.00504),(250.53571, 683.43361),(256.42857, 685.21933),(297.14286, 669.50504),(289.28571, 649.50504),(285, 631.6479),(285, 608.79075),(292.85714, 585.21932),(306.42857, 563.79075),(323.57143, 548.79075),(339.28571, 545.21932),(357.85714, 547.36218),(375, 550.21932),(391.42857, 568.07647),(404.28571, 588.79075),(413.57143, 612.36218),(417.14286, 628.07647),(438.57143, 619.1479),(438.03572, 618.96932),(437.5, 609.50504),(426.96429, 609.86218),(424.64286, 615.57647),(419.82143, 615.04075),(420.35714, 605.04075),(428.39286, 598.43361),(437.85714, 599.68361),(443.57143, 613.79075),(450.71429, 610.21933),(431.42857, 575.21932),(405.71429, 550.21932),(372.85714, 534.50504),(349.28571, 531.6479),(346.42857, 521.6479),(346.42857, 511.6479),(350.71429, 496.6479),(367.85714, 476.6479),(377.14286, 460.93361),(385.71429, 445.21932),(388.57143, 404.50504),(360, 352.36218),(337.14286, 325.93361),(330.71429, 334.50504),(347.14286, 354.50504),(337.85714, 370.21932),(333.57143, 359.50504),(319.28571, 353.07647),(312.85714, 366.6479),(350.71429, 387.36218),(368.57143, 408.07647),(375.71429, 431.6479),(372.14286, 454.50504),(366.42857, 462.36218),(352.85714, 462.36218),(336.42857, 456.6479),(332.85714, 438.79075),(338.57143, 423.79075),(338.57143, 411.6479),(327.85714, 405.93361),(320.71429, 407.36218),(315.71429, 423.07647),(314.28571, 440.21932),(325, 447.71932),(324.82143, 460.93361),(317.85714, 470.57647),(304.28571, 483.79075),(287.14286, 491.29075),(263.03571, 498.61218),(251.60714, 503.07647),(251.25, 533.61218),(260.71429, 533.61218),(272.85714, 528.43361),(286.07143, 518.61218),(297.32143, 508.25504),(297.85714, 507.36218),(298.39286, 506.46932),(307.14286, 496.6479),(312.67857, 491.6479),(317.32143, 503.07647),(322.5, 514.1479),(325.53571, 521.11218),(327.14286, 525.75504),(326.96429, 535.04075),(311.78571, 540.04075),(291.07143, 552.71932),(274.82143, 568.43361),(259.10714, 592.8979),(254.28571, 604.50504),(251.07143, 621.11218),(250.53571, 649.1479),(268.1955, 654.36208)],
   -- hole 1
   [(325, 437), (320, 423), (329, 413), (332, 423)],
   -- hole 2
   [(320.72342, 480), (338.90617, 465.96863), (347.99754, 480.61584), (329.8148, 510.41534), (339.91632, 480.11077), (334.86556, 478.09046)]]

/-- Steiner points fixture -/
def fixtureSteiner : List (List (Float × Float)) :=
  [[(0,0),(100,0),(100,100),(0,100)],
   [(50,50)],
   [(30,40)],
   [(70,60)],
   [(20,70)]]

/-- Issue 16 fixture (polygon with hole) -/
def fixtureIssue16 : List (List (Float × Float)) :=
  [[(143.129527283745121, 61.240160826593640),
    (147.399527283763751, 74.780160826630892),
    (154.049527283757931, 90.260160827077932),
    (174.429527283762581, 81.710160826332872),
    (168.03952728374861, 67.040160826407372),
    (159.099527283746281, 53.590160826221112)],
   [(156.85952728375561, 67.430160827003422),
    (157.489527283760251, 67.160160826519132),
    (159.969527283741631, 68.350160826928912),
    (161.339527283766071, 67.640160826966172),
    (159.649527283763751, 63.310160826891662),
    (155.759527283749781, 64.880160826258362)]]

/-- Issue 17 fixture -/
def fixtureIssue17 : List (List (Float × Float)) :=
  [[(185.926452684519706, 64.357497608498498),
    (185.346452684528922, 54.847497608553798),
    (174.856452684509758, 50.527497609378606),
    (169.556452684521526, 58.807497608653895),
    (178.936452684529708, 64.317497609016706),
    (179.146452684500916, 67.587497608561206)],
   [(176.936452684505928, 58.297497608611502),
    (177.746452684531326, 58.827497608656702),
    (177.356452684501722, 56.657497608519104)]]

/-- Hourglass fixture -/
def fixtureHourglass : List (List (Float × Float)) :=
  [[(7,18),(7,15),(5,15),(7,13),(7,15),(17,17)]]

/-- Empty square fixture (hole covers entire outer ring) -/
def fixtureEmptySquare : List (List (Float × Float)) :=
  [[(0,0),(4000,0),(4000,4000),(0,4000)],
   [(0,0),(4000,0),(4000,4000),(0,4000)]]

/-- Degenerate fixture -/
def fixtureDegenerate : List (List (Float × Float)) :=
  [[(100,100),(100,100),(200,100),(200,200),(200,100),(0,100)]]

/-- Water3b fixture (with holes) -/
def fixtureWater3b : List (List (Float × Float)) :=
  [[(-128,4224),(-128,-128),(4224,-128),(4224,4224),(-128,4224)],
   [(3832,-21),(3840,-17),(3877,21),(3895,39),(3961,-21),(3893,-98),(3855,-128),(3688,-128),(3742,-81),(3793,-41),(3832,-21),(3832,-21)],
   [(4205,596),(4224,572),(4224,248),(4166,163),(4119,50),(4020,36),(4004,21),(3969,21),(3936,62),(3982,117),(4088,293),(4152,419),(4185,544),(4205,596),(4205,596)]]

/-- Shared points fixture -/
def fixtureSharedPoints : List (List (Float × Float)) :=
  [[(0, 0), (0, 1), (1, 1)],
   [(1, 1), (1, 2), (2, 2)]]

/-- Bad diagonals fixture -/
def fixtureBadDiagonals : List (List (Float × Float)) :=
  [[(102,102.125),(102,100.5),(100,98),(99,99.5),(100,102.375),(100,103),(100.5,103.5),(102,102.125)],
   [(100,100),(101,100),(101.5,100.25),(101,101),(100,101)]]

/-- Touching 2 fixture -/
def fixtureTouching2 : List (List (Float × Float)) :=
  [[(0, 0), (0, 4), (4, 4), (4, 0)],
   [(2, 2), (2, 3), (3, 3), (3, 2)],
   [(1, 1), (1, 2), (2, 2), (2, 1)]]

/-- Touching 3 fixture -/
def fixtureTouching3 : List (List (Float × Float)) :=
  [[(0, 0), (0, 6), (6, 6), (6, 0)],
   [(4, 4), (4, 5), (5, 5), (5, 4)],
   [(2, 2), (2, 4), (4, 4), (4, 2)],
   [(1, 1), (1, 2), (2, 2), (2, 1)]]

/-- Touching 4 fixture -/
def fixtureTouching4 : List (List (Float × Float)) :=
  [[(0, 0), (0, 7), (7, 7), (7, 0)],
   [(5, 5), (5, 6), (6, 6), (6, 5)],
   [(3, 3), (3, 5), (5, 5), (5, 3)],
   [(1, 1), (1, 3), (3, 3), (3, 1)]]

/-- Infinite loop test case -/
def fixtureInfiniteLoop : List (List (Float × Float)) :=
  [[(1, 2), (2, 2), (1, 2), (1, 1), (1, 2), (4, 1), (5, 1), (3, 2), (4, 2), (4, 1)]]

/-- Issue 149 fixture -/
def fixtureIssue149 : List (List (Float × Float)) :=
  [[(100, 100), (100, 300), (150, 150), (300, 300), (300, 100)]]

/-- Issue 142 fixture -/
def fixtureIssue142 : List (List (Float × Float)) :=
  [[(3, 0), (2, 0), (0, 2), (0, 3), (3, 3), (3, 0)],
   [(2, 2), (2, 1), (1, 2)]]

-- ============================================================================
-- Test Runner
-- ============================================================================

/-- All tests with expected values -/
def allTests : List (String × List (List (Float × Float)) × Expected × Option Float) := [
  ("indices-2d", fixtureIndices2D, ⟨2, 0⟩, none),
  ("building", fixtureBuilding, ⟨13, 0⟩, none),
  ("dude", fixtureDude, ⟨106, 2e-15⟩, none),
  ("steiner", fixtureSteiner, ⟨9, 0⟩, none),
  ("issue16", fixtureIssue16, ⟨12, 4e-16⟩, some 8e-16),
  ("issue17", fixtureIssue17, ⟨11, 2e-16⟩, none),
  ("hourglass", fixtureHourglass, ⟨2, 0⟩, none),
  ("empty-square", fixtureEmptySquare, ⟨0, 0⟩, none),
  ("degenerate", fixtureDegenerate, ⟨0, 0⟩, none),
  ("water3b", fixtureWater3b, ⟨25, 0⟩, none),
  ("shared-points", fixtureSharedPoints, ⟨4, 0⟩, none),
  ("bad-diagonals", fixtureBadDiagonals, ⟨7, 0⟩, none),
  ("touching2", fixtureTouching2, ⟨8, 0⟩, none),
  ("touching3", fixtureTouching3, ⟨15, 0⟩, none),
  ("touching4", fixtureTouching4, ⟨19, 0⟩, none),
  ("issue149", fixtureIssue149, ⟨2, 0⟩, none),
  ("issue142", fixtureIssue142, ⟨4, 0.13⟩, none)
]

/-- Run all tests -/
def runAllTests : IO Unit := do
  IO.println "=== Earcut Lean 4 Test Suite ==="
  IO.println ""

  let mut passed := 0
  let mut failed := 0
  let mut failedTests : List String := []

  -- Run basic tests
  IO.println "--- Basic Tests ---"

  -- indices-2d test
  let data1 : Array Float := #[10, 0, 0, 50, 60, 60, 70, 10]
  let result1 := earcut data1
  if result1.toList == [1, 0, 3, 3, 2, 1] then
    IO.println "✓ indices-2d"
    passed := passed + 1
  else
    IO.println s!"✗ indices-2d: expected [1, 0, 3, 3, 2, 1], got {result1.toList}"
    failed := failed + 1
    failedTests := failedTests ++ ["indices-2d"]

  -- indices-3d test
  let data2 : Array Float := #[10, 0, 0, 0, 50, 0, 60, 60, 0, 70, 10, 0]
  let result2 := earcut data2 #[] 3
  if result2.toList == [1, 0, 3, 3, 2, 1] then
    IO.println "✓ indices-3d"
    passed := passed + 1
  else
    IO.println s!"✗ indices-3d: expected [1, 0, 3, 3, 2, 1], got {result2.toList}"
    failed := failed + 1
    failedTests := failedTests ++ ["indices-3d"]

  -- empty test
  let data3 : Array Float := #[]
  let result3 := earcut data3
  if result3.isEmpty then
    IO.println "✓ empty"
    passed := passed + 1
  else
    IO.println s!"✗ empty: expected [], got {result3.toList}"
    failed := failed + 1
    failedTests := failedTests ++ ["empty"]

  IO.println ""
  IO.println "--- Fixture Tests ---"

  -- Run fixture tests
  for (name, coords, expected, rotDev) in allTests do
    let results := runTestWithRotations name coords expected rotDev
    for result in results do
      if result.passed then
        IO.println s!"✓ {result.name}"
        passed := passed + 1
      else
        IO.println s!"✗ {result.name}: {result.message}"
        failed := failed + 1
        failedTests := failedTests ++ [result.name]

  IO.println ""
  IO.println "--- Special Tests ---"

  -- Infinite loop test (should not hang)
  let (infVerts, infHoles) := flattenCoords fixtureInfiniteLoop
  -- This should complete without hanging
  let _ := earcut infVerts infHoles
  IO.println "✓ infinite-loop (completed without hanging)"
  passed := passed + 1

  IO.println ""
  IO.println "=== Summary ==="
  IO.println s!"Passed: {passed}"
  IO.println s!"Failed: {failed}"

  if failed > 0 then
    IO.println ""
    IO.println "Failed tests:"
    for t in failedTests do
      IO.println s!"  - {t}"

/-- Verify flatten function -/
def testFlatten : IO Unit := do
  IO.println "--- Flatten Tests ---"

  let nested : Array (Array (Array Float)) := #[
    #[#[0, 0], #[1, 0], #[1, 1], #[0, 1]],
    #[#[0.2, 0.2], #[0.8, 0.2], #[0.8, 0.8], #[0.2, 0.8]]
  ]
  let (vertices, holes, dims) := flatten nested

  if dims == 2 &&
     holes.toList == [4] &&
     vertices.size == 16 then
    IO.println "✓ flatten"
  else
    IO.println s!"✗ flatten: dims={dims}, holes={holes.toList}, vertices.size={vertices.size}"

/-- Verify deviation function -/
def testDeviation : IO Unit := do
  IO.println "--- Deviation Tests ---"

  -- Perfect triangulation of a unit square
  let data : Array Float := #[0, 0, 1, 0, 1, 1, 0, 1]
  let triangles := earcut data
  let dev := deviation data #[] 2 triangles

  -- Deviation should be very close to 0
  if dev < 0.0001 then
    IO.println s!"✓ deviation (value: {dev})"
  else
    IO.println s!"✗ deviation: expected ~0, got {dev}"

def main : IO Unit := do
  runAllTests
  IO.println ""
  testFlatten
  IO.println ""
  testDeviation
  IO.println ""
  IO.println "=== All tests completed ==="
