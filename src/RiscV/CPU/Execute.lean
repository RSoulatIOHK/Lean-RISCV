import RiscV.Instr.Decode
import RiscV.CPU.CPU

namespace Riscv.CPU.Execute
open RiscV.CPU.CPU
open RiscV.Instr.Decode



-- def executeInstruction (state : CPUState) (instr: Instr.DecodedInstr) : CPU.CPUState :=
--   match instr with
--   | Instr.DecodedInstr.RType instr => executeRType state instr
--   | Instr.DecodedInstr.IType instr => executeIType state instr
--   | Instr.DecodedInstr.SType _ => state -- Placeholder for S-Type
--   | Instr.DecodedInstr.BType _ => state -- Placeholder for B-Type
--   | Instr.DecodedInstr.JType _ => state -- Placeholder for J-Type
--   | Instr.DecodedInstr.UType _ => state -- Placeholder for U-Type
--   | Instr.DecodedInstr.Invalid => { state with halted := true } -- Halt the CPU


end Riscv.CPU.Execute
