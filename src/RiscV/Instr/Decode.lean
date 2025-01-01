import RiscV.Instr.Common
import RiscV.Instr.RType
import RiscV.Instr.IType
import RiscV.Instr.SType
import RiscV.Instr.UType
import RiscV.Instr.BType
import RiscV.Instr.JType
import RiscV.CPU.CPU

namespace RiscV.Instr.Decode
open RiscV.Instr.Common

inductive DecodedInstr
  | RType : Instr.RType.RTypeDecoded → DecodedInstr
  | IType : Instr.IType.ITypeDecoded → DecodedInstr
  | SType : Instr.SType.STypeDecoded → DecodedInstr
  | BType : Instr.BType.BTypeDecoded → DecodedInstr
  | JType : Instr.JType.JTypeDecoded → DecodedInstr
  | UType : Instr.UType.UTypeDecoded → DecodedInstr
  | Invalid -- For unrecognized or invalid instructions
deriving Repr

instance : ToString DecodedInstr where
  toString
    | DecodedInstr.RType instr => toString instr
    | DecodedInstr.IType instr => toString instr
    | DecodedInstr.SType instr => toString instr
    | DecodedInstr.BType instr => toString instr
    | DecodedInstr.JType instr => toString instr
    | DecodedInstr.UType instr => toString instr
    | DecodedInstr.Invalid => "Invalid"

inductive InstrType
  | RType
  | IType
  | SType
  | BType
  | JType
  | UType
  | Unknown
deriving Repr

def classifyInstr (instr : CPU.CPU.Instr) : InstrType :=
  let opcode := extractOpcode instr
  match opcode with
  | 0b0110011 => InstrType.RType  -- R-Type
  | 0b0010011 => InstrType.IType  -- I-Type
  | 0b0100011 => InstrType.SType  -- S-Type
  | 0b1100011 => InstrType.BType  -- B-Type
  | 0b1101111 => InstrType.JType  -- J-Type
  | 0b0110111 => InstrType.UType  -- U-Type
  | _ => InstrType.Unknown

def decodeInstruction (instr : CPU.CPU.Instr) : DecodedInstr :=
  match classifyInstr instr with
  | InstrType.RType => DecodedInstr.RType (Instr.RType.decodeInstr instr)
  | InstrType.IType => DecodedInstr.IType (Instr.IType.decodeInstr instr)
  | InstrType.SType => DecodedInstr.SType (Instr.SType.decodeInstr instr)
  | InstrType.BType => DecodedInstr.BType (Instr.BType.decodeInstr instr)
  | InstrType.JType => DecodedInstr.JType (Instr.JType.decodeInstr instr)
  | InstrType.UType => DecodedInstr.UType (Instr.UType.decodeInstr instr)
  | _ => DecodedInstr.Invalid

end RiscV.Instr.Decode
