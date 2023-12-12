import Lake
open Lake DSL

require Parser from git "https://github.com/fgdorais/lean4-parser" @ "foldl"
require UnicodeBasic from git "https://github.com/fgdorais/lean4-unicode-basic" @ "main"
require std from git "https://github.com/leanprover/std4" @ "main"

package «advent-of-code» where
  -- add package configuration options here

lean_lib «AdventOfCode» where
  -- add library configuration options here

@[default_target]
lean_exe «advent-of-code» where
  root := `Main
  buildType := BuildType.debug
