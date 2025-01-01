import RiscV.Instr.Common
import RiscV.CPU.CPU

namespace RiscV.Instr.SType
open RiscV.Instr.Common
open RiscV.CPU.CPU

inductive Mnemonic
  | SW | Invalid
deriving BEq

instance : Repr Mnemonic where
  reprPrec m _ := match m with
  | Mnemonic.SW => "SW"
  | Mnemonic.Invalid => "Invalid"

structure STypeDecoded where
  mnemonic : Mnemonic
  rs1 : RegNum
  rs2 : RegNum
  imm : BitVec 12
deriving BEq

instance : Repr STypeDecoded where
  reprPrec instr _ :=
    "( " ++ repr instr.mnemonic ++
    ", rs1 = x" ++ repr instr.rs1.toNat ++
    ", rs2 = x" ++ repr instr.rs2.toNat ++
    ", imm = " ++ repr instr.imm.toNat ++ " )"

instance : ToString STypeDecoded where
  toString instr := toString (repr instr)

def decodeMnemonic (instr : Instr) : Mnemonic :=
  let funct3 := extractFunct3 instr
  match funct3 with
  | 0b010 => Mnemonic.SW
  | _ => Mnemonic.Invalid

def extractImm1 (instr : Instr) : BitVec 7 :=
  instr.extractLsb 31 25

def extractImm2 (instr : Instr) : BitVec 5 :=
  instr.extractLsb 11 7

def combineImms (imm1 : BitVec 7) (imm2 : BitVec 5) : BitVec 12 :=
  imm1.append imm2

def extractOperands (instr : Instr) : (RegNum × RegNum × BitVec 7 × BitVec 5) :=
  (extractRs1 instr, extractRs2 instr, extractImm1 instr, extractImm2 instr)

def decodeInstr (instr : Instr) : STypeDecoded :=
  let mnemonic := decodeMnemonic instr
  let (rs1, rs2, imm1, imm2) := extractOperands instr
  { mnemonic := mnemonic, rs1 := rs1, rs2 := rs2, imm := combineImms imm1 imm2 }

end RiscV.Instr.SType
