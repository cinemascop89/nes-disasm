module DisAsm where

import Control.Monad.State
import Data.Word
import Data.Char
import Data.List
import Data.Bits
import Data.Maybe
import Data.Array
import Numeric

import Asm

type PrgBank = Array Int Word8

data DisasmState = DisasmState {
     position :: Int,
     currOpcodes :: [Word8],
     disassembled :: [Bool],
     prgBank :: PrgBank
                 }

type Disassembler = StateT DisasmState IO

initDisassembler :: [Word8] -> DisasmState
initDisassembler rom = DisasmState {
                 position=0,
                 disassembled=take (length rom) (repeat False),
                 prgBank=array (0, length rom) (zip [0..length rom] rom),
                 currOpcodes=[]}

getPrgBank :: Disassembler PrgBank
getPrgBank = liftM prgBank get

incrPosition :: Disassembler ()
incrPosition = do
             st <- get
             put st {position = position st + 1}

setPosition :: Int -> Disassembler ()
setPosition p = do
            st <- get
            put st {position = p}

getPosition :: Disassembler Int
getPosition = do
            st <- get
            return $ position st

getCurrOpcodes :: Disassembler [Word8]
getCurrOpcodes = liftM currOpcodes get

appendOpcode :: Word8 -> Disassembler ()
appendOpcode b = do
             st <- get
             let opcodes = currOpcodes st
             put st {currOpcodes = opcodes ++ [b]}

clearCurrOpcodes :: Disassembler ()
clearCurrOpcodes = do
             st <- get
             put st {currOpcodes = []}

bytesLeft :: Disassembler Int
bytesLeft = do
          bank <- getPrgBank
          return $ length bank

isByteDisassembled :: Int -> Disassembler Bool
isByteDisassembled b = do
                   st <- get
                   return $ disassembled st !! b

setByteDisassembled :: Int -> Disassembler ()
setByteDisassembled b = do
                    st <- get
                    let bts = disassembled st
                    let (firstPart, _:lastPart) = splitAt b bts
                    put st {disassembled = firstPart ++ True:lastPart}

popByte :: Disassembler Word8
popByte = do
        bank <- getPrgBank
        pos <- getPosition
        let byte = bank ! pos
        setByteDisassembled pos
        appendOpcode byte
        incrPosition
        return $ byte

makeLine :: AsmInstruction -> Disassembler AsmOperand -> Disassembler AsmLine
makeLine instr getOper = do
         oper <- getOper
         return AsmLine {instruction = instr,
                         operand = oper,
                         opcodes=[]}

addressFromBytes :: Word8 -> Word8 -> Word16
addressFromBytes b1 b2 = (fromIntegral b1) + 0x100 * fromIntegral b2

paramIndirect :: Disassembler AsmOperand
paramIndirect = do
                  addr1 <- popByte
                  addr2 <- popByte
                  return $ Indirect (addressFromBytes addr1 addr2)

paramIndirectX :: Disassembler AsmOperand
paramIndirectX = do
                  addr <- popByte
                  return $ ZeroPageIndirectIndexedX addr

paramIndirectY :: Disassembler AsmOperand
paramIndirectY = do
                  addr <- popByte
                  return $ ZeroPageIndirectIndexedY addr

paramRelative :: Disassembler AsmOperand
paramRelative = do
                 addr <- popByte
                 let sign = if addr .&. (1 `shift` 7) > 0 then -1 else 1
                 let absVal = fromIntegral $ complement addr + 1
                 return $ Relative $ sign * absVal

paramAbsolute :: Disassembler AsmOperand
paramAbsolute = do
                  addr1 <- popByte
                  addr2 <- popByte
                  return $ Absolute (addressFromBytes addr1 addr2)

paramAbsoluteX :: Disassembler AsmOperand
paramAbsoluteX = do
                  addr1 <- popByte
                  addr2 <- popByte
                  return $ AbsIndexedX (addressFromBytes addr1 addr2)

paramAbsoluteY :: Disassembler AsmOperand
paramAbsoluteY = do
                  addr1 <- popByte
                  addr2 <- popByte
                  return $ AbsIndexedY (addressFromBytes addr1 addr2)

paramZeroPage :: Disassembler AsmOperand
paramZeroPage = do
                 addr <- popByte
                 return $ ZeroPage addr

paramZeroPageX :: Disassembler AsmOperand
paramZeroPageX = do
                  addr <- popByte
                  return $ ZeroPageIndexedX addr

paramZeroPageY :: Disassembler AsmOperand
paramZeroPageY = do
                  addr <- popByte
                  return $ ZeroPageIndexedY addr

paramImmediate :: Disassembler AsmOperand
paramImmediate = do
                  val <- popByte
                  return $ Immediate (fromIntegral val :: Word16)

paramImplied :: Disassembler AsmOperand
paramImplied = return Implied

disassembleInstruction :: Disassembler AsmLine
disassembleInstruction = do
            clearCurrOpcodes
            offset <- getPosition
            opcode <- popByte

            parsed <- case opcode of

                  -- ADC
                  0x69 -> makeLine Adc paramImmediate
                  0x65 -> makeLine Adc paramZeroPage
                  0x75 -> makeLine Adc paramZeroPageX
                  0x6d -> makeLine Adc paramAbsolute
                  0x7d -> makeLine Adc paramAbsoluteX
                  0x79 -> makeLine Adc paramAbsoluteY
                  -- 0x61 -> makeLine adc fetchParamIndirectX 2
                  0x71 -> makeLine Adc paramIndirectY

                  -- AND
                  0x29 -> makeLine And paramImmediate
                  0x25 -> makeLine And paramZeroPage
                  0x35 -> makeLine And paramZeroPageX
                  0x2d -> makeLine And paramAbsolute
                  0x3d -> makeLine And paramAbsoluteX
                  0x39 -> makeLine And paramAbsoluteY
                  -- 0x21 -> makeLine nd_ paramImmediate 6
                  0x31 -> makeLine And paramIndirectY

                  -- ASL
                  0x0a -> makeLine Asl paramImplied
                  0x06 -> makeLine Asl paramZeroPage
                  0x16 -> makeLine Asl paramZeroPageX
                  0x0e -> makeLine Asl paramAbsolute
                  0x1e -> makeLine Asl paramAbsoluteX

                  -- BCC
                  0x90 -> makeLine Bcc paramRelative

                  -- BCS
                  0xb0 -> makeLine Bcs paramRelative

                  -- BEQ
                  0xf0 -> makeLine Beq paramRelative

                  0x24 -> makeLine Bit paramZeroPage
                  0x2c -> makeLine Bit paramAbsolute

                  -- BNE
                  0xd0 -> makeLine Bne paramRelative

                  -- BPL
                  0x10 -> makeLine Bpl paramRelative

                  -- BRK
                  0x00 -> makeLine Brk paramImplied

                  0x70 -> makeLine Bvs paramRelative

                  -- CLC
                  0x18 -> makeLine Clc paramImplied

                  -- CLD
                  0xd8 -> makeLine Cld paramImplied

                  -- CMP
                  0xc9 -> makeLine Cmp paramImmediate
                  0xc5 -> makeLine Cmp paramZeroPage
                  0xd5 -> makeLine Cmp paramZeroPageX
                  0xcd -> makeLine Cmp paramAbsolute
                  0xdd -> makeLine Cmp paramAbsoluteX
                  0xd9 -> makeLine Cmp paramAbsoluteY
                  0xc1 -> makeLine Cmp paramImmediate
                  0xd1 -> makeLine Cmp paramIndirectY

                  -- CPX
                  0xe0 -> makeLine Cpx paramImmediate
                  0xe4 -> makeLine Cpx paramZeroPage
                  0xec -> makeLine Cpx paramAbsolute

                  -- CPY
                  0xc0 -> makeLine Cpy paramImmediate
                  0xc4 -> makeLine Cpy paramZeroPage
                  0xcc -> makeLine Cpy paramAbsolute

                  -- DEC
                  0xc6 -> makeLine Dec paramZeroPage
                  0xd6 -> makeLine Dec paramZeroPageX
                  0xce -> makeLine Dec paramAbsolute
                  0xde -> makeLine Dec paramAbsoluteX

                  -- DEX
                  0xca -> makeLine Dex paramImplied

                  -- DEY
                  0x88 -> makeLine Dey paramImplied

                  -- EOR
                  0x49 -> makeLine Eor paramImmediate
                  0x45 -> makeLine Eor paramZeroPage
                  0x55 -> makeLine Eor paramZeroPageX
                  0x4d -> makeLine Eor paramAbsolute
                  0x5d -> makeLine Eor paramAbsoluteX
                  0x59 -> makeLine Eor paramAbsoluteY
                  0x41 -> makeLine Eor paramImmediate
                  0x51 -> makeLine Eor paramIndirectY

                  -- INC
                  0xe6 -> makeLine Inc paramZeroPage
                  0xf6 -> makeLine Inc paramZeroPageX
                  0xee -> makeLine Inc paramAbsolute
                  0xfe -> makeLine Inc paramAbsoluteX

                  -- INX
                  0xe8 -> makeLine Inx paramImplied

                  -- INY
                  0xc8 -> makeLine Iny paramImplied

                  -- LSR
                  0x4a -> makeLine Lsr paramImplied
                  0x46 -> makeLine Lsr paramZeroPage
                  0x56 -> makeLine Lsr paramZeroPageX
                  0x4e -> makeLine Lsr paramAbsolute
                  0x5e -> makeLine Lsr paramAbsoluteX

                  -- ORA
                  0x09 -> makeLine Ora paramImmediate
                  0x05 -> makeLine Ora paramZeroPage
                  0x15 -> makeLine Ora paramZeroPageX
                  0x0d -> makeLine Ora paramAbsolute
                  0x1d -> makeLine Ora paramAbsoluteX
                  0x19 -> makeLine Ora paramAbsoluteY
                  0x01 -> makeLine Ora paramIndirectX
                  0x11 -> makeLine Ora paramIndirectY

                  -- PHA
                  0x48 -> makeLine Pha paramImplied

                  -- PLA
                  0x68 -> makeLine Pla paramImplied

                  -- ROR
                  0x6a -> makeLine Ror paramImplied
                  0x66 -> makeLine Ror paramZeroPage
                  0x76 -> makeLine Ror paramZeroPageX
                  0x6e -> makeLine Ror paramAbsolute
                  0x7e -> makeLine Ror paramAbsoluteX

                  -- ROL
                  0x2a -> makeLine Rol paramImplied
                  0x26 -> makeLine Rol paramZeroPage
                  0x36 -> makeLine Rol paramZeroPageX
                  0x2e -> makeLine Rol paramAbsolute
                  0x3e -> makeLine Rol paramAbsoluteX

                  -- RTS
                  0x60 -> makeLine Rts paramImplied

                  -- RTI
                  0x40 -> makeLine Rti paramImplied

                  -- SBC
                  0xe9 -> makeLine Sbc paramImmediate
                  0xe5 -> makeLine Sbc paramZeroPage
                  0xf5 -> makeLine Sbc paramZeroPageX
                  0xed -> makeLine Sbc paramAbsolute
                  0xfd -> makeLine Sbc paramAbsoluteX
                  0xf9 -> makeLine Sbc paramAbsoluteY
                  0xe1 -> makeLine Sbc paramZeroPageX
                  0xf1 -> makeLine Sbc paramIndirectY

                  -- SEC
                  0x38 -> makeLine Sec paramImplied

                  -- SED
                  0xf8 -> makeLine Sed paramImplied

                  -- SEI
                  0x78 -> makeLine Sei paramImplied

                  -- STA
                  0x85 -> makeLine Sta paramZeroPage
                  0x95 -> makeLine Sta paramZeroPageX
                  0x8d -> makeLine Sta paramAbsolute
                  0x9d -> makeLine Sta paramAbsoluteX
                  0x99 -> makeLine Sta paramAbsoluteY
                  -- 0x81 -> makeLine sta paramIndirectX 6
                  0x91 -> makeLine Sta paramIndirectY

                  -- STX
                  0x86 -> makeLine Stx paramZeroPage
                  0x96 -> makeLine Stx paramZeroPageY
                  0x8e -> makeLine Stx paramAbsolute

                  -- STY
                  0x8c -> makeLine Sty paramAbsolute

                  -- TAX
                  0xaa -> makeLine Tax paramImplied

                  -- TAY
                  0xa8 -> makeLine Tay paramImplied

                  -- TXA
                  0x8a -> makeLine Txa paramImplied

                  -- TXS
                  0x9a -> makeLine Txs paramImplied

                  -- TYA
                  0x98 -> makeLine Tya paramImplied

                  -- JMP
                  0x4c -> makeLine Jmp paramAbsolute
                  0x6c -> makeLine Jmp paramIndirect

                  -- JSR
                  0x20 -> makeLine Jsr paramAbsolute

                  -- LDA
                  0xa9 -> makeLine Lda paramImmediate
                  0xa5 -> makeLine Lda paramZeroPage
                  0xb5 -> makeLine Lda paramZeroPageX
                  0xad -> makeLine Lda paramAbsolute
                  0xbd -> makeLine Lda paramAbsoluteX
                  0xb9 -> makeLine Lda paramAbsoluteY
                  -- 0xa1 -> makeLine lda paramIndirectX 6
                  0xb1 -> makeLine Lda paramIndirectY

                  -- LDX
                  0xa2 -> makeLine Ldx paramImmediate
                  0xa6 -> makeLine Ldx paramZeroPage
                  0xb6 -> makeLine Ldx paramZeroPageY
                  0xae -> makeLine Ldx paramAbsolute
                  0xbe -> makeLine Ldx paramAbsoluteX

                  -- LDY
                  0xa0 -> makeLine Ldy paramImmediate
                  0xa4 -> makeLine Ldy paramZeroPage
                  0xb4 -> makeLine Ldy paramZeroPageX
                  0xac -> makeLine Ldy paramAbsolute
                  0xbc -> makeLine Ldy paramAbsoluteX

                  _ -> makeLine (Unknown) paramImplied --do fail $ "ERROR: unknown opcode 0x" ++ (showHex opcode " at offset ") ++ showHex (offset + 0x10) ""

            ops <- getCurrOpcodes
            return  parsed {opcodes = ops, offset = offset}

getInterruptVector :: PrgBank -> InterruptVector
getInterruptVector rom = InterruptVector {nmiAddress=nmi - 0x8000,
                                          resetAddress=reset - 0x8000,
                                          brkAddress=brk - 0x8000}
                   where
                     nmi = addressFromBytes (rom ! 0x7ffa) (rom ! 0x7ffb)
                     reset = addressFromBytes (rom ! 0x7ffc) (rom ! 0x7ffd)
                     brk = addressFromBytes (rom ! 0x7ffe) (rom ! 0x7fff)


disassembleAll :: Disassembler [AsmLine]
disassembleAll = do
               prgRom <- getPrgBank
               let interrupts = getInterruptVector prgRom

               setPosition $ fromIntegral $ nmiAddress interrupts
               lsNmi <- disassembleInterrupt

               setPosition $ fromIntegral $ resetAddress interrupts
               lsReset <- disassembleInterrupt

               setPosition $ fromIntegral $ brkAddress interrupts
               lsBrk <- disassembleInterrupt

               return $ joinLinesOrdered lsNmi (joinLinesOrdered lsReset lsBrk)

joinLinesOrdered :: [AsmLine] -> [AsmLine] -> [AsmLine]
joinLinesOrdered (l:l1) l2 = insertLineOrdered (joinLinesOrdered l1 l2) l
joinLinesOrdered [] l2 = l2

insertLineOrdered :: [AsmLine] -> AsmLine -> [AsmLine]
insertLineOrdered (l:ls) line = if smaller then
                                   line:l:ls
                                else
                                   l:(insertLineOrdered ls line)
                  where smaller = offset line < offset l
insertLineOrdered [] line = [line]

disassembleInterrupt :: Disassembler [AsmLine]
disassembleInterrupt = do
                     l <- disassembleInstruction
                     let isJump = case instruction l of
                                    Jmp -> case operand l of
                                            Absolute _ -> True
                                            _          -> False
                                    _   -> False
                     let isJsr = case instruction l of
                                   Jsr -> True
                                   _   -> False
                     let isRet = case instruction l of
                                   Rts -> True
                                   Rti -> True
                                   _   -> False
                     let isUnknown = case instruction l of
                                       Unknown -> True
                                       _       -> False
                     let instrOffs = fromIntegral $ instrOffset l
                     ls <- if isJump then
                              disassembleJump instrOffs
                           else
                              if isRet then
                                 return []
                              else if isJsr then
                                 disassembleJsr instrOffs
                              else if isUnknown then
                                 return []
                              else do
                                 pos <- getPosition
                                 isDisasm <- isByteDisassembled pos
                                 if isDisasm then
                                    return []
                                 else
                                    disassembleInterrupt
                     return $ insertLineOrdered ls l

disassembleJsr :: Int -> Disassembler [AsmLine]
disassembleJsr offs = do
               currPos <- getPosition
               setPosition offs
               branchDisasm <- disassembleInterrupt
               setPosition currPos
               nextDisasm <- disassembleInterrupt
               return $ nextDisasm ++ branchDisasm

disassembleJump :: Int -> Disassembler [AsmLine]
disassembleJump offs = do
                  isDisasm <- isByteDisassembled offs
                  if isDisasm then
                     return []
                  else do
                     setPosition offs
                     disassembleInterrupt

disassembleInstructions :: Int -> Disassembler [AsmLine]
disassembleInstructions n = do
                        l <- disassembleInstruction
                        ls <- if n == 0 then
                                return []
                              else
                                disassembleInstructions (n-1)
                        return (l:ls)

isBranch :: AsmLine -> Bool
isBranch l = case instruction l of
         Bcc -> True
         Bcs -> True
         Beq -> True
         Bmi -> True
         Bne -> True
         Bpl -> True
         Bvc -> True
         Bvs -> True
         Jmp -> case operand l of
                      Absolute _ -> True
                      _          -> False
         Jsr -> case operand l of
                      Absolute _ -> True
                      _          -> False
         _   -> False

filterBranching :: [AsmLine] -> [AsmLine]
filterBranching = filter isBranch

instrOffset :: AsmLine -> Word16
instrOffset line = case operand line of
                     Relative relOffset -> fromIntegral (offset line + relOffset + 2) :: Word16
                     Absolute absOffset -> absOffset - 0x8000


generateLabel :: Int -> AsmLine -> Label
generateLabel num line = Label {lblName = name, lblOffset = address}
              where name = case instruction line of
                             Jsr -> "subroutine" ++ show num
                             _   -> "label" ++ show num
                    address = instrOffset line

enumerateBranches :: [AsmLine] -> [(Int, AsmLine)]
enumerateBranches ls = zip (iterate (+1) 0) ls

generateLabels :: [AsmLine] -> [Label]
generateLabels ls =  map (uncurry generateLabel) (enumerateBranches (filterBranching ls))

getLabelByOffset :: Word16 -> [Label] -> Maybe Label
getLabelByOffset off (lbl:lbls) = if off == lblOffset lbl then
                                    Just lbl
                                  else
                                    getLabelByOffset off lbls
getLabelByOffset _ [] = Nothing

getLabel :: AsmLine -> [Label] -> Maybe Label
getLabel line = getLabelByOffset (fromIntegral $ offset line)


addLabels :: [Label] -> [AsmLine] -> [OutLine]
addLabels lbls (l:ls) = case getLabel l lbls of
                          Just lbl -> (Right lbl):(Left l'):(addLabels lbls ls)
                          Nothing  -> (Left l'):(addLabels lbls ls)
          where instrLabel = getLabelByOffset (instrOffset l) lbls
                l' = if isBranch l && isJust instrLabel then
                        l {operand = Labeled (fromJust instrLabel)}
                     else
                        l
addLabels _ [] = []

showInstruction :: AsmInstruction -> String
showInstruction = map toLower . show

showRelative :: Int -> String
showRelative n = sign ++ (showHex unsigned "")
             where unsigned = abs n
                   sign = if n < 0 then "-" else ""

showOperand :: AsmOperand -> String
showOperand (Immediate addr) = '#':'$':(showHex addr "")
showOperand (Absolute addr) = '$':(showHex addr "")
showOperand (ZeroPage addr) = '$':(showHex addr "")
showOperand (Relative addr) = '$':(showRelative addr)
showOperand (AbsIndexedX addr) = '$':(showHex addr ",X")
showOperand (AbsIndexedY addr) = '$':(showHex addr ",Y")
showOperand (ZeroPageIndexedX addr) = '$':(showHex addr ",X")
showOperand (ZeroPageIndexedY addr) = '$':(showHex addr ",Y")
showOperand (Indirect addr) = '(':'$':(showHex addr ")")
showOperand (ZeroPageIndirectIndexedX addr) = '(':'$':(showHex addr ",X)")
showOperand (ZeroPageIndirectIndexedY addr) = "(" ++ '$':(showHex addr ",Y") ++ ")"
showOperand (Labeled lbl) = lblName lbl
showOperand Implied = ""

showRawBytes :: [Word8] -> String
showRawBytes = (intercalate " ") . map (\b -> "0x" ++ showHex b "")

showOffset :: Int -> String
showOffset o
           | o < 0x10 = "0x000" ++ showHex o ""
           | o < 0x100 = "0x00" ++ showHex o ""
           | o < 0x1000 = "0x0" ++ showHex o ""
           | o < 0x10000 = "0x" ++ showHex o ""

spaceOffset :: Int -> String
spaceOffset n = take n $ repeat ' '

showAsmLine :: AsmLine -> String
showAsmLine line =  offsetPart ++ " " ++ instrPart ++ " " ++ operPart ++ spaceOffset numSpaces ++ commentPart
            where instrPart = showInstruction $ instruction line
                  operPart = showOperand $ operand line
                  offsetPart = "      "
                  commentPart = "; " ++ (showOffset $ 0x8000 + offset line) ++ "  " ++ (showRawBytes $ opcodes line)
                  numSpaces = 18 - length instrPart - length operPart - 2

showOutLine :: OutLine -> String
showOutLine (Left line) = showAsmLine line
showOutLine (Right label) = lblName label ++ ":"

generateOutput :: [AsmLine] -> String
generateOutput ls = intercalate "\n" $ map showOutLine (addLabels (generateLabels ls) ls)
