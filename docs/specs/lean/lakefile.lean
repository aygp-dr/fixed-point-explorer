import Lake
open Lake DSL

package «fixedPoint» where
  -- Package configuration

lean_lib «FixedPoint» where
  roots := #[`Fibonacci, `YCombinatorTheory, `Properties, 
             `SchemeCorrespondence, `Performance]

@[default_target]
lean_exe «tests» where
  root := `TestSuite
  supportInterpreter := true
