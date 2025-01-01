import RiscV.Instr.Common
import RiscV.CPU.CPU

namespace RiscV.Instr.IType
open RiscV.Instr.Common
open RiscV.CPU.CPU

def extractIImm (instr : Instr) : BitVec 12 :=
  instr.extractLsb 31 20

inductive Mnemonic
  | ADDI -- rd = rs1 + imm
  | SLTI -- rd = rs1 < imm ? 1 : 0
  | SLTIU -- rd = rs1 < imm ? 1 : 0
  | XORI -- rd = rs1 ^ imm
  | ORI -- rd = rs1 | imm
  | ANDI -- rd = rs1 & imm
  | Invalid
deriving BEq

instance : Repr Mnemonic where
  reprPrec m _ := match m with
  | Mnemonic.ADDI => "ADDI"
  | Mnemonic.SLTI => "SLTI"
  | Mnemonic.SLTIU => "SLTIU"
  | Mnemonic.XORI => "XORI"
  | Mnemonic.ORI => "ORI"
  | Mnemonic.ANDI => "ANDI"
  | Mnemonic.Invalid => "Invalid"

structure ITypeDecoded where
  mnemonic : Mnemonic
  rd : RegNum
  rs1 : RegNum
  imm : BitVec 12

instance : Repr ITypeDecoded where
  reprPrec instr _ :=
    "( " ++ repr instr.mnemonic ++
    ", rd = x" ++ repr instr.rd.toNat ++
    ", rs1 = x" ++ repr instr.rs1.toNat ++
    ", imm = " ++ repr instr.imm ++ " )"

instance : ToString ITypeDecoded where
  toString instr := toString (repr instr)

-- Decode I-Type mnemonic
def decodeMnemonic (instr : BitVec 32) : Mnemonic :=
  let funct3 := extractFunct3 instr
  match funct3 with
  | 0b000 => Mnemonic.ADDI
  | 0b010 => Mnemonic.SLTI
  | 0b011 => Mnemonic.SLTIU
  | 0b100 => Mnemonic.XORI
  | 0b110 => Mnemonic.ORI
  | 0b111 => Mnemonic.ANDI
  | _ => Mnemonic.Invalid

-- Extract operands for I-Type instructions
def extractOperands (instr : BitVec 32) : (RegNum × RegNum × BitVec 12) :=
  (extractRd instr,  -- rd
   extractRs1 instr, -- rs1
   extractIImm instr)       -- imm

def decodeInstr (instr : Instr) : ITypeDecoded :=
  let mnemonic := decodeMnemonic instr
  let (rd, rs1, imm) := extractOperands instr
  { mnemonic := mnemonic, rd := rd, rs1 := rs1, imm := imm }

def executeIType (state : CPU.CPU.CPUState) (instr : Instr.IType.ITypeDecoded) : CPU.CPU.CPUState :=
  let rs1Val := getRegisterValue state instr.rs1
  let immVal := toRegType instr.imm
  let rd := instr.rd
  let result := match instr.mnemonic with
    | Instr.IType.Mnemonic.ADDI => rs1Val + immVal
    | Instr.IType.Mnemonic.SLTI => if rs1Val < immVal then 1 else 0
    | Instr.IType.Mnemonic.SLTIU => if rs1Val < immVal then 1 else 0
    | Instr.IType.Mnemonic.XORI => rs1Val.xor immVal
    | Instr.IType.Mnemonic.ORI => rs1Val ||| immVal
    | Instr.IType.Mnemonic.ANDI => rs1Val &&& immVal
    | _ => rs1Val -- Default case, should not happen
  setRegisterValue state rd result

end RiscV.Instr.IType
