module StackPU.CPU where

import Clash.Prelude
import Clash.Num.Overflowing
import Debug.Trace

opcode :: BitVector 8 -> BitVector 5
opcode = slice d7 d3

operand :: BitVector 8 -> BitVector 3
operand = slice d2 d0

data Op = Nop | Add | Sub | Xor | And | Ior | Inc | Rsh | Dec | Clr | Rst | Rld | Jmp | Pst | Mld | Mst | Pld
  deriving (Eq, Show)

branchOp :: Op -> Bool
branchOp = (== Jmp)

decodeOp :: BitVector 8 -> Op
decodeOp ins = case opcode ins of
  0b00000 -> Nop
  0b00001 -> Add
  0b00010 -> Sub
  0b00011 -> Xor
  0b00100 -> And
  0b00101 -> Ior
  0b00110 -> Inc
  0b00111 -> Rsh
  0b01000 -> Dec
  0b01001 -> Clr
  0b01010 -> Rst
  0b01011 -> Rld
  0b01100 -> Jmp
  0b01101 -> Pst
  0b01110 -> Mld
  0b01111 -> Mst
  0b10000 -> Pld
  _ -> Nop

alu :: Op -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Bool)
-- alu op a b | traceShow (op, a, b) False = undefined
alu Add a b = let out = toOverflowing a + toOverflowing b in (fromOverflowing out, hasOverflowed out)
alu Sub a b = let out = toOverflowing a - toOverflowing b in (fromOverflowing out, not $ hasOverflowed out)
alu Xor a b = (a `xor` b, False)
alu And a b = (a .&. b, False)
alu Ior a b = (a .|. b, False)
alu Inc a _ = (a + 1, False)
alu Dec a _ = (a - 1, False)
alu Rld _ b = (b, False)
alu _ _ _ = (0, False)

decodeAccEn :: Op -> Bool
-- decodeAccEn op | traceShow op False = undefined
decodeAccEn Nop = False
decodeAccEn Rst = False
decodeAccEn Jmp = False
decodeAccEn Pst = False
decodeAccEn Mst = False
decodeAccEn _ = True

regfile
  :: (HiddenClockResetEnable dom)
  => Signal dom (Index 8) -- ^ Write index
  -> Signal dom (Unsigned 8) -- ^ Write data
  -> Signal dom Bool -- ^ Write enable
  -> Signal dom (Vec 8 (Unsigned 8))
regfile addr wrData en = file
  where
    file = regEn (repeat 0) en file'
    file' = replace <$> addr <*> wrData <*> file

(.!) :: (BitPack a) => Signal dom a -> Int -> Signal dom Bit
(.!) bits idx = liftA2 (!) bits $ pure idx

stackpu :: forall (dom :: Domain). (HiddenClockResetEnable dom) => (Signal dom (Unsigned 8) -> Signal dom (BitVector 8)) -> Signal dom (Unsigned 8)
stackpu insMem = acc
  where
    condChk = not <$> (coutChk .||. underflowChk .||. zeroChk)
      where
        coutChk = not <$> cout .&&. (bitToBool <$> oper .! 0)
        underflowChk = bitToBool <$> ((.&.) <$> aluOut .! 7 <*> oper .! 1)
        zeroChk = not <$> zero .&&. (bitToBool <$> oper .! 2)
    branching = (op .==. pure Jmp) .&&. condChk
    pc = register 0 $ mux disableDecode (unpack <$> insOrImm) (pc + 1)

    immDisable = (accEn .&&. (oper .==. pure 0)) .&&. (not <$> useImm)

    useImm = register False immDisable
    disableDecode = register False branching

    lastIns = register 0 insOrImm
    insOrImm = insMem pc
    ins = mux useImm lastIns ins'
      where ins' = mux disableDecode 0 insOrImm
    op = decodeOp <$> ins
    oper = operand <$> ins

    regIdx = unpack <$> oper
    regWrEn = op .==. pure Rst
    regData = regfile regIdx acc regWrEn

    accEn = decodeAccEn <$> op
    acc = regEn 0 (accEn .&&. not <$> immDisable) aluOut

    aluA = acc
    aluB = mux useImm (unpack <$> insOrImm) $ (!!) <$> regData <*> regIdx
    (aluOut, cout) = unbundle $ alu <$> op <*> aluA <*> aluB
    zero = (== 0) <$> aluOut
