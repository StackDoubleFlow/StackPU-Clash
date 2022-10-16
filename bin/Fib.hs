import Clash.Prelude
import StackPU.CPU

fib :: Unsigned 8 -> BitVector 8
-- fib addr | traceShow addr False = undefined
fib 0 = 0b01011000
fib 1 = 0b00000000
fib 2 = 0b01010001
fib 3 = 0b01011000
fib 4 = 0b00000001
fib 5 = 0b01010010
fib 6 = 0b00001001
fib 7 = 0b01010001
fib 8 = 0b00001010
fib 9 = 0b01010010
fib 10 = 0b01100000
fib 11 = 0b00000110
fib _ = 0

main :: IO ()
main = putStrLn $ show $ sampleN @System 100 $ stackpu $ liftA fib
