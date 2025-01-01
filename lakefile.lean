import Lake
open Lake DSL

def execName := "riscv"

package «BitVMX-CPI» {
  binDir := "../../bin" ++ execName ++ "/"
}

lean_lib Soundness

@[default_target]
lean_lib RiscV {
  srcDir := "src"

}

@[default_target]
lean_exe emulator {
  root := `src
  exeName := execName
}
