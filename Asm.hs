module Asm where

import Data.Word


type Address = Word16
type SmallAddress = Word8

data InterruptVector = InterruptVector {
     nmiAddress :: Address,
     resetAddress :: Address,
     brkAddress :: Address
                     } deriving Show

data AsmInstruction = Adc | And | Asl | Bcc | Bcs | Beq | Bit | Bmi | Bne | Bpl |
     Brk | Bvc | Bvs | Clc | Cld | Cli | Clv | Cmp | Cpx | Cpy | Dec | Dex |
     Dey | Eor | Inc | Inx | Iny | Jmp | Jsr | Lda | Ldx | Ldy | Lsr | Nop |
     Ora | Pha | Php | Pla | Plp | Rol | Ror | Rti | Rts | Sbc | Sec | Sed |
     Sei | Sta | Stx | Sty | Tax | Tay | Tsx | Txa | Txs | Tya | Unknown deriving Show

data AsmOperand = Implied | Immediate Address | Absolute Address |
     ZeroPage SmallAddress | Relative Int | AbsIndexedX Address |
     AbsIndexedY Address | ZeroPageIndexedX SmallAddress |
     ZeroPageIndexedY SmallAddress | Indirect Address |
     ZeroPageIndirectIndexedX SmallAddress |
     ZeroPageIndirectIndexedY SmallAddress |
     Labeled Label deriving Show

data AsmLine = AsmLine {
     offset :: Int,
     instruction :: AsmInstruction,
     operand :: AsmOperand,
     opcodes :: [Word8]
                    } deriving Show

data Label = Label {
     lblName :: String,
     lblOffset :: Address
     } deriving Show

type OutLine = Either AsmLine Label
