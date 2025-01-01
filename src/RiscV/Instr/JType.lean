import RiscV.Instr.Common
import RiscV.CPU.CPU

namespace RiscV.Instr.JType
open RiscV.Instr.Common
open RiscV.CPU.CPU

inductive Mnemonic
  | JAL | Invalid
deriving BEq

instance : Repr Mnemonic where
  reprPrec m _ := match m with
  | Mnemonic.JAL => "JAL"
  | Mnemonic.Invalid => "Invalid"

structure JTypeDecoded where
  mnemonic : Mnemonic
  rd : RegNum
  imm : BitVec 21

instance : Repr JTypeDecoded where
  reprPrec instr _ :=
    "( " ++ repr instr.mnemonic ++
    ", rd = x" ++ repr instr.rd.toNat ++
    ", imm = " ++ repr instr.imm.toNat ++ " )"

instance : ToString JTypeDecoded where
  toString instr := toString (repr instr)

def decodeMnemonic (instr : Instr) : Mnemonic :=
  let opcode := extractOpcode instr
  match opcode with
  | 0b1101111 => Mnemonic.JAL
  | _ => Mnemonic.Invalid

def extractImm (instr : Instr) : BitVec 21 :=
  let imm20 := instr.extractLsb 31 31
  let imm10_1 := instr.extractLsb 30 21
  let imm11 := instr.extractLsb 20 20
  let imm19_12 := instr.extractLsb 19 12
  imm20.append (imm19_12.append (imm11.append (imm10_1.append (0 : BitVec 1))))

def extractOperands (instr : Instr) : (RegNum × BitVec 21) :=
  (extractRd instr, extractImm instr)

def decodeInstr (instr : Instr) : JTypeDecoded :=
  let mnemonic := decodeMnemonic instr
  let (rd, imm) := extractOperands instr
  { mnemonic := mnemonic, rd := rd, imm := imm }

end RiscV.Instr.JType
