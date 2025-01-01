import RiscV.Instr.Common
import RiscV.CPU.CPU

namespace RiscV.Instr.UType
open RiscV.Instr.Common
open RiscV.CPU.CPU

inductive Mnemonic
  | LUI | AUIPC | Invalid
deriving BEq

instance : Repr Mnemonic where
  reprPrec m _ := match m with
  | Mnemonic.LUI => "LUI"
  | Mnemonic.AUIPC => "AUIPC"
  | Mnemonic.Invalid => "Invalid"

structure UTypeDecoded where
  mnemonic : Mnemonic
  rd : RegNum
  imm : BitVec 20

instance : Repr UTypeDecoded where
  reprPrec instr _ :=
    "( " ++ repr instr.mnemonic ++
    ", rd = x" ++ repr instr.rd.toNat ++
    ", imm = " ++ repr instr.imm.toNat ++ " )"

instance : ToString UTypeDecoded where
  toString instr := toString (repr instr)

def decodeMnemonic (instr : Instr) : Mnemonic :=
  let opcode := extractOpcode instr
  match opcode with
  | 0b0110111 => Mnemonic.LUI
  | 0b0010111 => Mnemonic.AUIPC
  | _ => Mnemonic.Invalid

def extractOperands (instr : Instr) : (RegNum × BitVec 20) :=
  (extractRd instr, instr.extractLsb 31 12)

def decodeInstr (instr : Instr) : UTypeDecoded :=
  let mnemonic := decodeMnemonic instr
  let (rd, imm) := extractOperands instr
  { mnemonic := mnemonic, rd := rd, imm := imm }


end RiscV.Instr.UType
