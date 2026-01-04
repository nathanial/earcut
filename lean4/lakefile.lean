import Lake
open Lake DSL

package «earcut» where
  version := v!"0.1.0"

lean_lib «Earcut» where
  -- add library configuration options here

@[default_target]
lean_exe «earcut» where
  root := `Main

lean_exe «test» where
  root := `Test
