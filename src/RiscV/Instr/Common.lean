import RiscV.CPU.CPU

namespace RiscV.Instr.Common

def extractRs2 (instr : CPU.CPU.Instr) : CPU.CPU.RegNum :=
  instr.extractLsb 24 20

def extractRs1 (instr : CPU.CPU.Instr) : CPU.CPU.RegNum :=
  instr.extractLsb 19 15

def extractRd (instr : CPU.CPU.Instr) : CPU.CPU.RegNum :=
  instr.extractLsb 11 7

def extractOpcode (instr : CPU.CPU.Instr) : BitVec 7 :=
  instr.extractLsb 6 0

def extractFunct3 (instr : CPU.CPU.Instr) : BitVec 3 :=
  instr.extractLsb 14 12

def extractFunct7 (instr : CPU.CPU.Instr) : BitVec 7 :=
  instr.extractLsb 31 25

def toRegType (imm : BitVec n) : CPU.CPU.Reg :=
  imm.signExtend CPU.CPU.XLEN

def getRegisterValue (state : CPU.CPU.CPUState) (reg : CPU.CPU.RegNum) : CPU.CPU.Reg :=
  if reg.toNat == 0 then 0 else state.registers.get! reg.toNat

def setRegisterValue (state : CPU.CPU.CPUState) (reg : CPU.CPU.RegNum) (value : CPU.CPU.Reg) : CPU.CPU.CPUState :=
  if reg.toNat == 0 then state
  else { state with registers := state.registers.set! reg.toNat value }

end RiscV.Instr.Common
