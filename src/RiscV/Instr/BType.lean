import RiscV.Instr.Common
import RiscV.CPU.CPU

namespace RiscV.Instr.BType
open RiscV.Instr.Common
open RiscV.CPU.CPU


inductive Mnemonic
  | BEQ     -- Branch if equal
  | BNE     -- Branch if not equal
  | BLT     -- Branch if less than
  | BGE     -- Branch if greater or equal
  | BLTU    -- Branch if less than unsigned
  | BGEU    -- Branch if greater or equal unsigned
  | Invalid -- Invalid instruction
deriving BEq

instance : Repr Mnemonic where
  reprPrec m _ := match m with
  | Mnemonic.BEQ => "BEQ"
  | Mnemonic.BNE => "BNE"
  | Mnemonic.BLT => "BLT"
  | Mnemonic.BGE => "BGE"
  | Mnemonic.BLTU => "BLTU"
  | Mnemonic.BGEU => "BGEU"
  | Mnemonic.Invalid => "Invalid"

structure BTypeDecoded where
  mnemonic : Mnemonic
  rs1 : RegNum
  rs2 : RegNum
  imm : BitVec 13
deriving BEq

instance : Repr BTypeDecoded where
  reprPrec instr _ :=
    "( " ++ repr instr.mnemonic ++
    ", rs1 = x" ++ repr instr.rs1.toNat ++
    ", rs2 = x" ++ repr instr.rs2.toNat ++
    ", imm = " ++ repr instr.imm.toNat ++ " )"

instance : ToString BTypeDecoded where
  toString instr := toString (repr instr)

def decodeMnemonic (instr : Instr) : Mnemonic :=
  let funct3 := extractFunct3 instr
  match funct3 with
  | 0b000 => Mnemonic.BEQ
  | 0b001 => Mnemonic.BNE
  | 0b100 => Mnemonic.BLT
  | 0b101 => Mnemonic.BGE
  | 0b110 => Mnemonic.BLTU
  | 0b111 => Mnemonic.BGEU
  | _ => Mnemonic.Invalid

def extractImm (instr : Instr) : BitVec 13 :=
  let imm12 := instr.extractLsb 31 31       -- Extract bit 31 (sign bit)
  let imm10_5 := instr.extractLsb 30 25     -- Extract bits 30 to 25
  let imm4_1 := instr.extractLsb 11 8       -- Extract bits 11 to 8
  let imm11 := instr.extractLsb 7 7         -- Extract bit 7
  imm12.append (imm11.append (imm10_5.append (imm4_1.append (0 : BitVec 1))))

def extractOperands (instr : Instr) : (RegNum × RegNum × BitVec 13) :=
  (extractRs1 instr, extractRs2 instr, extractImm instr)

def decodeInstr (instr : Instr) : BTypeDecoded :=
  let mnemonic := decodeMnemonic instr
  let (rs1, rs2, imm) := extractOperands instr
  { mnemonic := mnemonic, rs1 := rs1, rs2 := rs2, imm := imm }

end RiscV.Instr.BType
