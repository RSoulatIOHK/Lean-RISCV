namespace RiscV.CPU.CPU

-- RISCV RV32IM
-- 32 registers on 32-bit
def XLEN := 32

abbrev RegNum := BitVec 5
abbrev Reg := BitVec XLEN
abbrev Regs := Array Reg
abbrev Mem := Array (BitVec 8)
abbrev Instr := BitVec XLEN

instance : Repr Regs where
  --split the registers into 4 rows of 8 registers
  reprPrec regs _ :=
    let rows := List.range 4 |>.map (λ i =>
      List.range 8 |>.map (λ j => toString (repr (regs.get! (i*8 + j)))) |> String.intercalate " ")
    String.intercalate "\n" rows

structure CPUState where
  registers : Regs
  pc : Reg
  memory : Array UInt8
  --flags
  halted : Bool
  --interrupts

instance : Repr CPUState where
  reprPrec st _ :=
    "VirtualCPUState(\n\t registers = \n" ++ repr st.registers ++
    ",\n\t pc = " ++ repr st.pc ++
    ",\n\t memory = " ++ repr st.memory ++
    ",\n\t halted = " ++ repr st.halted ++
    "\n)"

instance : ToString CPUState where
  toString st := toString (repr st)

def defaultState : CPUState :=
  { registers := Array.mkArray 32 0
  , pc := 0
  , memory := Array.mkArray 40 0
  , halted := false
  }

abbrev CPUExecM := StateM CPU.CPUState


end RiscV.CPU.CPU
