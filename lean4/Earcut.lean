/-
  Earcut - A Lean 4 port of the earcut polygon triangulation library
  Original JavaScript implementation by Vladimir Agafonkin (Mapbox)

  This library triangulates polygons using the ear clipping algorithm
  with z-order curve hashing optimization for large polygons.
-/

namespace Earcut

/-- A node in the doubly-linked circular list representing a polygon vertex -/
structure Node where
  id : Nat                -- unique identifier for reference equality
  i : Nat                 -- vertex index in the flat coordinates array
  x : Float               -- x coordinate
  y : Float               -- y coordinate
  prevId : Nat            -- previous node id
  nextId : Nat            -- next node id
  z : Int                 -- z-order curve value
  prevZ : Option Nat      -- previous node id in z-order
  nextZ : Option Nat      -- next node id in z-order
  steiner : Bool          -- whether this is a steiner point
  deriving Repr, Inhabited

/-- State for managing the linked list of nodes -/
structure NodeStore where
  nodes : Array Node
  nextId : Nat
  deriving Repr, Inhabited

/-- Initialize an empty node store -/
def NodeStore.empty : NodeStore := { nodes := #[], nextId := 0 }

/-- Get a node by its id -/
def NodeStore.get (store : NodeStore) (id : Nat) : Option Node :=
  store.nodes.find? fun n => n.id == id

/-- Get a node, with a default if not found -/
def NodeStore.get! (store : NodeStore) (id : Nat) : Node :=
  match store.get id with
  | some n => n
  | none => default

/-- Update a node in the store -/
def NodeStore.update (store : NodeStore) (node : Node) : NodeStore :=
  let nodes := store.nodes.map fun n => if n.id == node.id then node else n
  { store with nodes := nodes }

/-- Add a new node to the store -/
def NodeStore.add (store : NodeStore) (node : Node) : NodeStore :=
  { nodes := store.nodes.push node, nextId := store.nextId + 1 }

/-- Create a new node with a unique id -/
def NodeStore.createNode (store : NodeStore) (i : Nat) (x y : Float) : Node × NodeStore :=
  let node : Node := {
    id := store.nextId
    i := i
    x := x
    y := y
    prevId := store.nextId  -- self-referencing initially
    nextId := store.nextId
    z := 0
    prevZ := none
    nextZ := none
    steiner := false
  }
  (node, store.add node)

/-- Signed area of a triangle (positive if counter-clockwise) -/
def signedTriangleArea (px py qx qy rx ry : Float) : Float :=
  (qy - py) * (rx - qx) - (qx - px) * (ry - qy)

/-- Signed area using Node coordinates -/
def area (store : NodeStore) (pId qId rId : Nat) : Float :=
  let p := store.get! pId
  let q := store.get! qId
  let r := store.get! rId
  signedTriangleArea p.x p.y q.x q.y r.x r.y

/-- Check if two nodes have equal coordinates -/
def equals (store : NodeStore) (p1Id p2Id : Nat) : Bool :=
  let p1 := store.get! p1Id
  let p2 := store.get! p2Id
  p1.x == p2.x && p1.y == p2.y

/-- Sign of a number: 1 if positive, -1 if negative, 0 if zero -/
def sign (num : Float) : Int :=
  if num > 0 then 1
  else if num < 0 then -1
  else 0

/-- Check if point q lies on segment pr (for collinear points) -/
def onSegment (store : NodeStore) (pId qId rId : Nat) : Bool :=
  let p := store.get! pId
  let q := store.get! qId
  let r := store.get! rId
  q.x <= Float.max p.x r.x && q.x >= Float.min p.x r.x &&
  q.y <= Float.max p.y r.y && q.y >= Float.min p.y r.y

/-- Check if two segments intersect -/
def segmentsIntersect (store : NodeStore) (p1Id q1Id p2Id q2Id : Nat) : Bool :=
  let o1 := sign (area store p1Id q1Id p2Id)
  let o2 := sign (area store p1Id q1Id q2Id)
  let o3 := sign (area store p2Id q2Id p1Id)
  let o4 := sign (area store p2Id q2Id q1Id)

  if o1 != o2 && o3 != o4 then true  -- general case
  else if o1 == 0 && onSegment store p1Id p2Id q1Id then true
  else if o2 == 0 && onSegment store p1Id q2Id q1Id then true
  else if o3 == 0 && onSegment store p2Id p1Id q2Id then true
  else if o4 == 0 && onSegment store p2Id q1Id q2Id then true
  else false

/-- Check if a point lies within a convex triangle -/
def pointInTriangle (ax ay bx by cx cy px py : Float) : Bool :=
  (cx - px) * (ay - py) >= (ax - px) * (cy - py) &&
  (ax - px) * (by - py) >= (bx - px) * (ay - py) &&
  (bx - px) * (cy - py) >= (cx - px) * (by - py)

/-- Check if a point lies within triangle but false if equal to first point -/
def pointInTriangleExceptFirst (ax ay bx by cx cy px py : Float) : Bool :=
  not (ax == px && ay == py) && pointInTriangle ax ay bx by cx cy px py

/-- Calculate signed area of a polygon ring in flat array format -/
def signedArea (data : Array Float) (start finish dim : Nat) : Float := Id.run do
  let mut sum : Float := 0
  let mut j := finish - dim
  let mut i := start
  while i < finish do
    let di := data.get! i
    let di1 := data.get! (i + 1)
    let dj := data.get! j
    let dj1 := data.get! (j + 1)
    sum := sum + (dj - di) * (di1 + dj1)
    j := i
    i := i + dim
  return sum

/-- Remove a node from the linked list -/
def removeNode (store : NodeStore) (pId : Nat) : NodeStore := Id.run do
  let p := store.get! pId
  let pNext := store.get! p.nextId
  let pPrev := store.get! p.prevId

  let pNext' := { pNext with prevId := p.prevId }
  let pPrev' := { pPrev with nextId := p.nextId }

  let mut store' := store.update pNext'
  store' := store'.update pPrev'

  -- Update z-order links
  match p.prevZ with
  | some prevZId =>
    let prevZ := store'.get! prevZId
    store' := store'.update { prevZ with nextZ := p.nextZ }
  | none => pure ()

  match p.nextZ with
  | some nextZId =>
    let nextZ := store'.get! nextZId
    store' := store'.update { nextZ with prevZ := p.prevZ }
  | none => pure ()

  return store'

/-- Insert a node after 'last' in the circular list -/
def insertNode (store : NodeStore) (i : Nat) (x y : Float) (lastId : Option Nat) : Nat × NodeStore := Id.run do
  let (node, store') := store.createNode i x y

  match lastId with
  | none =>
    let node' := { node with prevId := node.id, nextId := node.id }
    return (node.id, store'.update node')
  | some lastIdVal =>
    let last := store'.get! lastIdVal
    let lastNext := store'.get! last.nextId

    let node' := { node with nextId := last.nextId, prevId := lastIdVal }
    let last' := { last with nextId := node.id }
    let lastNext' := { lastNext with prevId := node.id }

    let store'' := store'.update node'
    let store''' := store''.update last'
    let store'''' := store'''.update lastNext'
    return (node.id, store'''')

/-- Create a circular doubly linked list from polygon points -/
def linkedList (data : Array Float) (start finish dim : Nat) (clockwise : Bool) (store : NodeStore)
    : Option Nat × NodeStore := Id.run do
  let mut lastId : Option Nat := none
  let mut store' := store

  let shouldReverse := clockwise == (signedArea data start finish dim > 0)

  if shouldReverse then
    let mut i := start
    while i < finish do
      let idx := i / dim
      let x := data.get! i
      let y := data.get! (i + 1)
      let (newId, newStore) := insertNode store' idx x y lastId
      lastId := some newId
      store' := newStore
      i := i + dim
  else
    let mut i : Int := finish - dim
    while i >= start do
      let idx := i.toNat / dim
      let x := data.get! i.toNat
      let y := data.get! (i.toNat + 1)
      let (newId, newStore) := insertNode store' idx x y lastId
      lastId := some newId
      store' := newStore
      i := i - dim

  -- Remove duplicate last point if it equals first
  match lastId with
  | some lid =>
    let last := store'.get! lid
    if equals store' lid last.nextId then
      store' := removeNode store' lid
      lastId := some last.nextId
  | none => pure ()

  return (lastId, store')

/-- Check if a diagonal is locally inside the polygon -/
def locallyInside (store : NodeStore) (aId bId : Nat) : Bool :=
  let a := store.get! aId
  let areaVal := area store a.prevId aId a.nextId
  if areaVal < 0 then
    area store aId bId a.nextId >= 0 && area store aId a.prevId bId >= 0
  else
    area store aId bId a.prevId < 0 || area store aId a.nextId bId < 0

/-- Check if a polygon diagonal intersects any polygon segments -/
def intersectsPolygon (store : NodeStore) (aId bId : Nat) : Bool := Id.run do
  let a := store.get! aId
  let mut pId := aId
  let mut p := a

  repeat
    if p.i != a.i && (store.get! p.nextId).i != a.i &&
       p.i != (store.get! bId).i && (store.get! p.nextId).i != (store.get! bId).i &&
       segmentsIntersect store pId p.nextId aId bId then
      return true
    pId := p.nextId
    p := store.get! pId
    if pId == aId then break

  return false

/-- Check if the middle point of a diagonal is inside the polygon -/
def middleInside (store : NodeStore) (aId bId : Nat) : Bool := Id.run do
  let a := store.get! aId
  let b := store.get! bId
  let px := (a.x + b.x) / 2
  let py := (a.y + b.y) / 2

  let mut inside := false
  let mut pId := aId
  let mut p := a

  repeat
    let pNext := store.get! p.nextId
    if ((p.y > py) != (pNext.y > py)) && pNext.y != p.y &&
        (px < (pNext.x - p.x) * (py - p.y) / (pNext.y - p.y) + p.x) then
      inside := !inside
    pId := p.nextId
    p := store.get! pId
    if pId == aId then break

  return inside

/-- Check if a diagonal between two polygon nodes is valid -/
def isValidDiagonal (store : NodeStore) (aId bId : Nat) : Bool :=
  let a := store.get! aId
  let b := store.get! bId
  a.nextId != bId && a.prevId != bId &&
  not (intersectsPolygon store aId bId) &&
  ((locallyInside store aId bId && locallyInside store bId aId &&
    middleInside store aId bId &&
    (area store a.prevId aId b.prevId != 0 || area store aId b.prevId bId != 0)) ||
   (equals store aId bId &&
    area store a.prevId aId a.nextId > 0 &&
    area store b.prevId bId b.nextId > 0))

/-- Link two polygon vertices with a bridge -/
def splitPolygon (store : NodeStore) (aId bId : Nat) : Nat × NodeStore := Id.run do
  let a := store.get! aId
  let b := store.get! bId

  -- Create a2 (copy of a)
  let (a2, store1) := store.createNode a.i a.x a.y
  -- Create b2 (copy of b)
  let (b2, store2) := store1.createNode b.i b.x b.y

  let an := store2.get! a.nextId
  let bp := store2.get! b.prevId

  -- a.next = b
  let a' := { store2.get! aId with nextId := bId }
  let store3 := store2.update a'

  -- b.prev = a
  let b' := { store3.get! bId with prevId := aId }
  let store4 := store3.update b'

  -- a2.next = an
  let a2' := { store4.get! a2.id with nextId := an.id }
  let store5 := store4.update a2'

  -- an.prev = a2
  let an' := { store5.get! an.id with prevId := a2.id }
  let store6 := store5.update an'

  -- b2.next = a2
  let b2' := { store6.get! b2.id with nextId := a2.id }
  let store7 := store6.update b2'

  -- a2.prev = b2
  let a2'' := { store7.get! a2.id with prevId := b2.id }
  let store8 := store7.update a2''

  -- bp.next = b2
  let bp' := { store8.get! bp.id with nextId := b2.id }
  let store9 := store8.update bp'

  -- b2.prev = bp
  let b2'' := { store9.get! b2.id with prevId := bp.id }
  let store10 := store9.update b2''

  return (b2.id, store10)

/-- Z-order of a point given coords and inverse of the longer side of data bbox -/
def zOrder (x y minX minY invSize : Float) : Int := Id.run do
  -- Transform coords into non-negative 15-bit integer range
  let mut xi := Float.floor ((x - minX) * invSize) |>.toUInt32.toNat
  let mut yi := Float.floor ((y - minY) * invSize) |>.toUInt32.toNat

  -- Interleave bits using bit manipulation
  xi := (xi ||| (xi <<< 8)) &&& 0x00FF00FF
  xi := (xi ||| (xi <<< 4)) &&& 0x0F0F0F0F
  xi := (xi ||| (xi <<< 2)) &&& 0x33333333
  xi := (xi ||| (xi <<< 1)) &&& 0x55555555

  yi := (yi ||| (yi <<< 8)) &&& 0x00FF00FF
  yi := (yi ||| (yi <<< 4)) &&& 0x0F0F0F0F
  yi := (yi ||| (yi <<< 2)) &&& 0x33333333
  yi := (yi ||| (yi <<< 1)) &&& 0x55555555

  return (xi ||| (yi <<< 1)).toInt

/-- Merge sort for z-order linked list (Simon Tatham's algorithm) -/
partial def sortLinked (store : NodeStore) (listId : Option Nat) : Option Nat × NodeStore := Id.run do
  match listId with
  | none => return (none, store)
  | some lid =>
    let mut list := some lid
    let mut store' := store
    let mut inSize : Nat := 1
    let mut numMerges : Nat := 1

    while numMerges > 0 do
      let mut p := list
      let mut tail : Option Nat := none
      list := none
      numMerges := 0

      while p.isSome do
        numMerges := numMerges + 1
        let mut q := p
        let mut pSize : Nat := 0

        for _ in [0:inSize] do
          pSize := pSize + 1
          match q with
          | some qId =>
            let qNode := store'.get! qId
            q := qNode.nextZ
          | none => break

        let mut qSize := inSize

        while pSize > 0 || (qSize > 0 && q.isSome) do
          let mut e : Option Nat := none

          if pSize != 0 && (qSize == 0 || q.isNone ||
             (store'.get! p.get!).z <= (store'.get! q.get!).z) then
            e := p
            let pNode := store'.get! p.get!
            p := pNode.nextZ
            pSize := pSize - 1
          else
            e := q
            let qNode := store'.get! q.get!
            q := qNode.nextZ
            qSize := qSize - 1

          match tail with
          | some tailId =>
            let tailNode := store'.get! tailId
            store' := store'.update { tailNode with nextZ := e }
          | none =>
            list := e

          let eNode := store'.get! e.get!
          store' := store'.update { eNode with prevZ := tail }
          tail := e

        p := q

      match tail with
      | some tailId =>
        let tailNode := store'.get! tailId
        store' := store'.update { tailNode with nextZ := none }
      | none => pure ()

      inSize := inSize * 2

    return (list, store')

/-- Index curve - interlink polygon nodes in z-order -/
def indexCurve (store : NodeStore) (startId : Nat) (minX minY invSize : Float)
    : NodeStore := Id.run do
  let mut store' := store
  let mut pId := startId

  -- Calculate z-order for each node and set up z-links
  repeat
    let p := store'.get! pId
    let z := if p.z == 0 then zOrder p.x p.y minX minY invSize else p.z
    let p' := { p with z := z, prevZ := some p.prevId, nextZ := some p.nextId }
    store' := store'.update p'
    pId := p.nextId
    if pId == startId then break

  -- Break circular z-links
  let pFirst := store'.get! startId
  match pFirst.prevZ with
  | some prevZId =>
    let prevZ := store'.get! prevZId
    store' := store'.update { prevZ with nextZ := none }
    let pFirst' := store'.get! startId
    store' := store'.update { pFirst' with prevZ := none }
  | none => pure ()

  -- Sort by z-order
  let (_, store'') := sortLinked store' (some startId)
  return store''

/-- Find the leftmost node of a polygon ring -/
def getLeftmost (store : NodeStore) (startId : Nat) : Nat := Id.run do
  let mut pId := startId
  let mut leftmostId := startId

  repeat
    let p := store.get! pId
    let leftmost := store.get! leftmostId
    if p.x < leftmost.x || (p.x == leftmost.x && p.y < leftmost.y) then
      leftmostId := pId
    pId := p.nextId
    if pId == startId then break

  return leftmostId

/-- Check whether sector in vertex m contains sector in vertex p -/
def sectorContainsSector (store : NodeStore) (mId pId : Nat) : Bool :=
  let m := store.get! mId
  let p := store.get! pId
  area store m.prevId mId p.prevId < 0 && area store p.nextId mId m.nextId < 0

/-- Find a bridge between hole and outer polygon (David Eberly's algorithm) -/
def findHoleBridge (store : NodeStore) (holeId outerNodeId : Nat) : Option Nat := Id.run do
  let hole := store.get! holeId
  let hx := hole.x
  let hy := hole.y
  let mut qx : Float := -1e100  -- Approximation of -Infinity
  let mut mId : Option Nat := none
  let mut pId := outerNodeId

  -- Check if hole equals the starting point
  if equals store holeId pId then
    return some pId

  repeat
    let p := store.get! pId
    -- Check if hole equals next
    if equals store holeId p.nextId then
      return some p.nextId

    let pNext := store.get! p.nextId
    if hy <= p.y && hy >= pNext.y && pNext.y != p.y then
      let x := p.x + (hy - p.y) * (pNext.x - p.x) / (pNext.y - p.y)
      if x <= hx && x > qx then
        qx := x
        mId := if p.x < pNext.x then some pId else some p.nextId
        if x == hx then
          return mId

    pId := p.nextId
    if pId == outerNodeId then break

  match mId with
  | none => return none
  | some mid =>
    let stop := mid
    let m := store.get! mid
    let mx := m.x
    let my := m.y
    let mut tanMin : Float := 1e100  -- Approximation of Infinity
    let mut bestM := mid
    let mut pId' := mid

    repeat
      let p := store.get! pId'
      if hx >= p.x && p.x >= mx && hx != p.x then
        let inTriangle := if hy < my then
          pointInTriangle hx hy mx my qx hy p.x p.y
        else
          pointInTriangle qx hy mx my hx hy p.x p.y

        if inTriangle then
          let tanVal := Float.abs (hy - p.y) / (hx - p.x)
          if locallyInside store pId' holeId &&
             (tanVal < tanMin || (tanVal == tanMin &&
              (p.x > (store.get! bestM).x ||
               (p.x == (store.get! bestM).x && sectorContainsSector store bestM pId')))) then
            bestM := pId'
            tanMin := tanVal

      pId' := p.nextId
      if pId' == stop then break

    return some bestM

/-- Eliminate a hole by linking it to the outer ring -/
def eliminateHole (store : NodeStore) (holeId outerNodeId : Nat) : Nat × NodeStore := Id.run do
  match findHoleBridge store holeId outerNodeId with
  | none => return (outerNodeId, store)
  | some bridge =>
    let (bridgeReverse, store') := splitPolygon store bridge holeId
    -- Filter collinear points (simplified - actual filtering would be more complex)
    return (bridge, store')

/-- Compare function for sorting holes by x, y, and slope -/
def compareXYSlope (store : NodeStore) (aId bId : Nat) : Ordering :=
  let a := store.get! aId
  let b := store.get! bId
  let result := a.x.compare b.x
  if result != Ordering.eq then result
  else
    let result2 := a.y.compare b.y
    if result2 != Ordering.eq then result2
    else
      let aNext := store.get! a.nextId
      let bNext := store.get! b.nextId
      let aSlope := (aNext.y - a.y) / (aNext.x - a.x)
      let bSlope := (bNext.y - b.y) / (bNext.x - b.x)
      aSlope.compare bSlope

/-- Link every hole into the outer loop -/
def eliminateHoles (data : Array Float) (holeIndices : Array Nat) (outerNodeId : Nat)
    (dim : Nat) (store : NodeStore) : Nat × NodeStore := Id.run do
  let mut store' := store
  let mut queue : Array Nat := #[]

  for i in [0:holeIndices.size] do
    let start := holeIndices.get! i * dim
    let finish := if i < holeIndices.size - 1
                  then holeIndices.get! (i + 1) * dim
                  else data.size
    let (listOpt, newStore) := linkedList data start finish dim false store'
    store' := newStore
    match listOpt with
    | some listId =>
      let list := store'.get! listId
      if list.nextId == listId then
        store' := store'.update { list with steiner := true }
      queue := queue.push (getLeftmost store' listId)
    | none => pure ()

  -- Sort holes by x (simplified sort)
  queue := queue.qsort fun a b =>
    match compareXYSlope store' a b with
    | Ordering.lt => true
    | _ => false

  -- Process holes from left to right
  let mut outerNode := outerNodeId
  for i in [0:queue.size] do
    let holeId := queue.get! i
    let (newOuter, newStore) := eliminateHole store' holeId outerNode
    outerNode := newOuter
    store' := newStore

  return (outerNode, store')

/-- Filter out colinear or duplicate points -/
partial def filterPoints (store : NodeStore) (startId : Nat) (endOpt : Option Nat)
    : Nat × NodeStore := Id.run do
  let endId := endOpt.getD startId
  let mut store' := store
  let mut pId := startId
  let mut endId' := endId
  let mut again := true

  while again do
    again := false
    let p := store'.get! pId

    if not p.steiner && (equals store' pId p.nextId || area store' p.prevId pId p.nextId == 0) then
      store' := removeNode store' pId
      pId := p.prevId
      endId' := p.prevId
      if pId == (store'.get! pId).nextId then
        break
      again := true
    else
      pId := p.nextId

    if not again && pId != endId' then
      again := true

  return (endId', store')

/-- Check whether a polygon node forms a valid ear -/
def isEar (store : NodeStore) (earId : Nat) : Bool := Id.run do
  let ear := store.get! earId
  let a := store.get! ear.prevId
  let c := store.get! ear.nextId

  -- Reflex check
  if area store ear.prevId earId ear.nextId >= 0 then
    return false

  let ax := a.x; let bx := ear.x; let cx := c.x
  let ay := a.y; let by := ear.y; let cy := c.y

  -- Triangle bbox
  let x0 := Float.min ax (Float.min bx cx)
  let y0 := Float.min ay (Float.min by cy)
  let x1 := Float.max ax (Float.max bx cx)
  let y1 := Float.max ay (Float.max by cy)

  -- Check for points inside the potential ear
  let mut pId := c.nextId
  while pId != ear.prevId do
    let p := store.get! pId
    if p.x >= x0 && p.x <= x1 && p.y >= y0 && p.y <= y1 &&
       pointInTriangleExceptFirst ax ay bx by cx cy p.x p.y &&
       area store p.prevId pId p.nextId >= 0 then
      return false
    pId := p.nextId

  return true

/-- Check whether a polygon node forms a valid ear (hashed version) -/
def isEarHashed (store : NodeStore) (earId : Nat) (minX minY invSize : Float) : Bool := Id.run do
  let ear := store.get! earId
  let a := store.get! ear.prevId
  let c := store.get! ear.nextId

  if area store ear.prevId earId ear.nextId >= 0 then
    return false

  let ax := a.x; let bx := ear.x; let cx := c.x
  let ay := a.y; let by := ear.y; let cy := c.y

  let x0 := Float.min ax (Float.min bx cx)
  let y0 := Float.min ay (Float.min by cy)
  let x1 := Float.max ax (Float.max bx cx)
  let y1 := Float.max ay (Float.max by cy)

  let minZ := zOrder x0 y0 minX minY invSize
  let maxZ := zOrder x1 y1 minX minY invSize

  let mut pOpt := ear.prevZ
  let mut nOpt := ear.nextZ

  -- Look for points inside the triangle in both directions
  while pOpt.isSome && (store.get! pOpt.get!).z >= minZ &&
        nOpt.isSome && (store.get! nOpt.get!).z <= maxZ do
    let pId := pOpt.get!
    let p := store.get! pId
    if p.x >= x0 && p.x <= x1 && p.y >= y0 && p.y <= y1 &&
       pId != ear.prevId && pId != ear.nextId &&
       pointInTriangleExceptFirst ax ay bx by cx cy p.x p.y &&
       area store p.prevId pId p.nextId >= 0 then
      return false
    pOpt := p.prevZ

    let nId := nOpt.get!
    let n := store.get! nId
    if n.x >= x0 && n.x <= x1 && n.y >= y0 && n.y <= y1 &&
       nId != ear.prevId && nId != ear.nextId &&
       pointInTriangleExceptFirst ax ay bx by cx cy n.x n.y &&
       area store n.prevId nId n.nextId >= 0 then
      return false
    nOpt := n.nextZ

  -- Look for remaining points in decreasing z-order
  while pOpt.isSome && (store.get! pOpt.get!).z >= minZ do
    let pId := pOpt.get!
    let p := store.get! pId
    if p.x >= x0 && p.x <= x1 && p.y >= y0 && p.y <= y1 &&
       pId != ear.prevId && pId != ear.nextId &&
       pointInTriangleExceptFirst ax ay bx by cx cy p.x p.y &&
       area store p.prevId pId p.nextId >= 0 then
      return false
    pOpt := p.prevZ

  -- Look for remaining points in increasing z-order
  while nOpt.isSome && (store.get! nOpt.get!).z <= maxZ do
    let nId := nOpt.get!
    let n := store.get! nId
    if n.x >= x0 && n.x <= x1 && n.y >= y0 && n.y <= y1 &&
       nId != ear.prevId && nId != ear.nextId &&
       pointInTriangleExceptFirst ax ay bx by cx cy n.x n.y &&
       area store n.prevId nId n.nextId >= 0 then
      return false
    nOpt := n.nextZ

  return true

/-- Cure small local self-intersections -/
def cureLocalIntersections (store : NodeStore) (startId : Nat) (triangles : Array Nat)
    : Nat × Array Nat × NodeStore := Id.run do
  let mut store' := store
  let mut triangles' := triangles
  let mut pId := startId
  let mut startId' := startId

  repeat
    let p := store'.get! pId
    let a := store'.get! p.prevId
    let bId := (store'.get! p.nextId).nextId
    let b := store'.get! bId

    if not (equals store' p.prevId bId) &&
       segmentsIntersect store' p.prevId pId p.nextId bId &&
       locallyInside store' p.prevId bId &&
       locallyInside store' bId p.prevId then

      triangles' := triangles'.push a.i
      triangles' := triangles'.push p.i
      triangles' := triangles'.push b.i

      store' := removeNode store' pId
      store' := removeNode store' p.nextId

      pId := bId
      startId' := bId

    pId := (store'.get! pId).nextId
    if pId == startId' then break

  let (filteredId, store'') := filterPoints store' pId none
  return (filteredId, triangles', store'')

/-- Split and triangulate each half of the polygon -/
partial def splitEarcut (store : NodeStore) (startId : Nat) (triangles : Array Nat)
    (dim : Nat) (minX minY : Float) (invSize : Option Float)
    : Array Nat × NodeStore := Id.run do
  let mut aId := startId
  let mut store' := store
  let mut triangles' := triangles

  repeat
    let a := store'.get! aId
    let mut bId := (store'.get! a.nextId).nextId

    while bId != a.prevId do
      let b := store'.get! bId
      if a.i != b.i && isValidDiagonal store' aId bId then
        let (cId, newStore) := splitPolygon store' aId bId
        store' := newStore

        let (aFiltered, store2) := filterPoints store' aId (some a.nextId)
        store' := store2
        let (cFiltered, store3) := filterPoints store' cId (some (store'.get! cId).nextId)
        store' := store3

        let (tris1, store4) := earcutLinked store' aFiltered triangles' dim minX minY invSize 0
        triangles' := tris1
        store' := store4
        let (tris2, store5) := earcutLinked store' cFiltered triangles' dim minX minY invSize 0
        return (tris2, store5)

      bId := (store'.get! bId).nextId

    aId := a.nextId
    if aId == startId then break

  return (triangles', store')

/-- Main ear slicing loop which triangulates a polygon -/
partial def earcutLinked (store : NodeStore) (earId : Nat) (triangles : Array Nat)
    (dim : Nat) (minX minY : Float) (invSize : Option Float) (pass : Nat)
    : Array Nat × NodeStore := Id.run do
  let mut store' := store
  let mut triangles' := triangles
  let mut earId' := earId

  -- Index curve on first pass if using hashing
  if pass == 0 then
    match invSize with
    | some inv =>
      store' := indexCurve store' earId' minX minY inv
    | none => pure ()

  let mut stopId := earId'

  while (store'.get! earId').prevId != (store'.get! earId').nextId do
    let ear := store'.get! earId'
    let prev := store'.get! ear.prevId
    let next := store'.get! ear.nextId

    let isValidEar := match invSize with
      | some inv => isEarHashed store' earId' minX minY inv
      | none => isEar store' earId'

    if isValidEar then
      triangles' := triangles'.push prev.i
      triangles' := triangles'.push ear.i
      triangles' := triangles'.push next.i

      store' := removeNode store' earId'

      earId' := next.nextId
      stopId := next.nextId
      continue

    earId' := ear.nextId

    if earId' == stopId then
      if pass == 0 then
        let (filtered, newStore) := filterPoints store' earId' none
        store' := newStore
        return earcutLinked store' filtered triangles' dim minX minY invSize 1
      else if pass == 1 then
        let (cured, tris, newStore) := cureLocalIntersections store' earId' triangles'
        triangles' := tris
        store' := newStore
        return earcutLinked store' cured triangles' dim minX minY invSize 2
      else if pass == 2 then
        return splitEarcut store' earId' triangles' dim minX minY invSize
      break

  return (triangles', store')

/-- Main earcut function - triangulates a polygon

    @param data - flat array of vertex coordinates (x, y, x, y, ...)
    @param holeIndices - array of indices where each hole starts
    @param dim - number of coordinates per vertex (default 2)
    @return array of triangle indices
-/
def earcut (data : Array Float) (holeIndices : Array Nat := #[]) (dim : Nat := 2)
    : Array Nat := Id.run do
  let hasHoles := holeIndices.size > 0
  let outerLen := if hasHoles then holeIndices.get! 0 * dim else data.size

  let mut store := NodeStore.empty
  let (outerNodeOpt, store') := linkedList data 0 outerLen dim true store
  store := store'

  match outerNodeOpt with
  | none => return #[]
  | some outerNodeId =>
    let outer := store.get! outerNodeId
    if outer.nextId == outer.prevId then
      return #[]

    let mut outerNode := outerNodeId
    let mut minX : Float := 0
    let mut minY : Float := 0
    let mut invSize : Option Float := none

    -- Eliminate holes
    if hasHoles then
      let (newOuter, newStore) := eliminateHoles data holeIndices outerNode dim store
      outerNode := newOuter
      store := newStore

    -- Calculate bbox for z-order optimization on large polygons
    if data.size > 80 * dim then
      minX := data.get! 0
      minY := data.get! 1
      let mut maxX := minX
      let mut maxY := minY

      let mut i := dim
      while i < outerLen do
        let x := data.get! i
        let y := data.get! (i + 1)
        if x < minX then minX := x
        if y < minY then minY := y
        if x > maxX then maxX := x
        if y > maxY then maxY := y
        i := i + dim

      let size := Float.max (maxX - minX) (maxY - minY)
      invSize := if size != 0 then some (32767 / size) else some 0

    let (triangles, _) := earcutLinked store outerNode #[] dim minX minY invSize 0
    return triangles

/-- Calculate the deviation between polygon area and triangulation area -/
def deviation (data : Array Float) (holeIndices : Array Nat) (dim : Nat)
    (triangles : Array Nat) : Float := Id.run do
  let hasHoles := holeIndices.size > 0
  let outerLen := if hasHoles then holeIndices.get! 0 * dim else data.size

  let mut polygonArea := Float.abs (signedArea data 0 outerLen dim)

  if hasHoles then
    for i in [0:holeIndices.size] do
      let start := holeIndices.get! i * dim
      let finish := if i < holeIndices.size - 1
                    then holeIndices.get! (i + 1) * dim
                    else data.size
      polygonArea := polygonArea - Float.abs (signedArea data start finish dim)

  let mut trianglesArea : Float := 0
  let mut i := 0
  while i < triangles.size do
    let a := triangles.get! i * dim
    let b := triangles.get! (i + 1) * dim
    let c := triangles.get! (i + 2) * dim
    trianglesArea := trianglesArea + Float.abs (
      (data.get! a - data.get! c) * (data.get! (b + 1) - data.get! (a + 1)) -
      (data.get! a - data.get! b) * (data.get! (c + 1) - data.get! (a + 1))
    )
    i := i + 3

  if polygonArea == 0 && trianglesArea == 0 then
    return 0
  else
    return Float.abs ((trianglesArea - polygonArea) / polygonArea)

/-- Flatten a nested polygon array (like GeoJSON) into flat format -/
def flatten (data : Array (Array (Array Float))) : Array Float × Array Nat × Nat := Id.run do
  if data.isEmpty || data.get! 0 |>.isEmpty || (data.get! 0).get! 0 |>.isEmpty then
    return (#[], #[], 2)

  let dimensions := ((data.get! 0).get! 0).size
  let mut vertices : Array Float := #[]
  let mut holes : Array Nat := #[]
  let mut holeIndex : Nat := 0
  let mut prevLen : Nat := 0

  for ring in data do
    for p in ring do
      for d in [0:dimensions] do
        vertices := vertices.push (p.get! d)

    if prevLen > 0 then
      holeIndex := holeIndex + prevLen
      holes := holes.push holeIndex

    prevLen := ring.size

  return (vertices, holes, dimensions)

end Earcut
