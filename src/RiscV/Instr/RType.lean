import RiscV.Instr.Common
import RiscV.CPU.CPU

namespace RiscV.Instr.RType
open RiscV.Instr.Common
open RiscV.CPU.CPU

inductive Mnemonic
  | ADD -- rd = rs1 + rs2
  | SUB -- rd = rs1 - rs2
  | SLL -- rd = rs1 << rs2
  | SLT -- rd = rs1 < rs2 ? 1 : 0
  | SLTU -- rd = rs1 < rs2 ? 1 : 0
  | XOR -- rd = rs1 ^ rs2
  | SRL -- rd = rs1 >> rs2
  | SRA -- rd = rs1 >> rs2
  | OR -- rd = rs1 | rs2
  | AND -- rd = rs1 & rs2
  | Invalid
deriving BEq

instance : Repr Mnemonic where
  reprPrec m _ := match m with
  | Mnemonic.ADD => "ADD"
  | Mnemonic.SUB => "SUB"
  | Mnemonic.SLL => "SLL"
  | Mnemonic.SLT => "SLT"
  | Mnemonic.SLTU => "SLTU"
  | Mnemonic.XOR => "XOR"
  | Mnemonic.SRL => "SRL"
  | Mnemonic.SRA => "SRA"
  | Mnemonic.OR => "OR"
  | Mnemonic.AND => "AND"
  | Mnemonic.Invalid => "Invalid"

structure RTypeDecoded where
  mnemonic : Mnemonic
  rd : RegNum
  rs1 : RegNum
  rs2 : RegNum

instance : Repr RTypeDecoded where
  reprPrec instr _ :=
    "( " ++ repr instr.mnemonic ++
    ", rd = x" ++ repr instr.rd.toNat ++
    ", rs1 = x" ++ repr instr.rs1.toNat ++
    ", rs2 = x" ++ repr instr.rs2.toNat ++ " )"

instance : ToString RTypeDecoded where
  toString instr := toString (repr instr)

-- Decode the mnemonic from an R-Type instructions
-- Inputs: 32-bit instruction
-- Outputs: Mnemonic R-Type instruction
def decodeMnemonic (instr: Instr) : Mnemonic :=
  let funct7 := extractFunct7 instr
  let funct3 := extractFunct3 instr
  match funct7, funct3 with
  | 0b0000000, 0b000 => Mnemonic.ADD
  | 0b0100000, 0b000 => Mnemonic.SUB
  | 0b0000000, 0b001 => Mnemonic.SLL
  | 0b0000000, 0b010 => Mnemonic.SLT
  | 0b0000000, 0b011 => Mnemonic.SLTU
  | 0b0000000, 0b100 => Mnemonic.XOR
  | 0b0000000, 0b101 => Mnemonic.SRL
  | 0b0100000, 0b101 => Mnemonic.SRA
  | 0b0000000, 0b110 => Mnemonic.OR
  | 0b0000000, 0b111 => Mnemonic.AND
  | _, _ => Mnemonic.Invalid

-- Extract operands for R-Type instructions
def extractOperands (instr : Instr) : (BitVec 5 × BitVec 5 × BitVec 5) :=
  (extractRd instr,  -- rd
   extractRs1 instr, -- rs1
   extractRs2 instr) -- rs2

def decodeInstr (instr : Instr) : RTypeDecoded :=
  let mnemonic := decodeMnemonic instr
  let (rd, rs1, rs2) := extractOperands instr
  { mnemonic := mnemonic, rd := rd, rs1 := rs1, rs2 := rs2 }

-- Execute an R-Type instruction
def executeRType (state : CPU.CPU.CPUState) (instr : Instr.RType.RTypeDecoded) : CPU.CPU.CPUState :=
  let rs1Val := getRegisterValue state instr.rs1
  let rs2Val := getRegisterValue state instr.rs2
  let rd := instr.rd
  let result := match instr.mnemonic with
    | Instr.RType.Mnemonic.ADD => rs1Val + rs2Val
    | Instr.RType.Mnemonic.SUB => rs1Val - rs2Val
    | Instr.RType.Mnemonic.SLL => rs1Val <<< rs2Val
    | Instr.RType.Mnemonic.SLT => if rs1Val < rs2Val then 1 else 0
    | Instr.RType.Mnemonic.SLTU => if rs1Val < rs2Val then 1 else 0
    | Instr.RType.Mnemonic.XOR => rs1Val.xor rs2Val
    | Instr.RType.Mnemonic.SRL => rs1Val >>> rs2Val
    | Instr.RType.Mnemonic.SRA => rs1Val >>> rs2Val
    | Instr.RType.Mnemonic.OR  => rs1Val ||| rs2Val
    | Instr.RType.Mnemonic.AND => rs1Val &&& rs2Val
    | _ => rs1Val -- Default case, should not happen
  setRegisterValue state rd result

end RiscV.Instr.RType
