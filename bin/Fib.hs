import Clash.Prelude
import StackPU.CPU

fib :: Unsigned 8 -> BitVector 8
fib = asyncRomFile d12 "programs/fib"

main :: IO ()
main = print $ sampleN @System 100 $ stackpu $ fmap fib
