import Clash.Prelude
import StackPU.CPU

fib :: Unsigned 8 -> BitVector 8
fib = asyncRomFile d12 "programs/fib"

main :: IO ()
main = putStrLn $ show $ sampleN @System 100 $ stackpu $ liftA fib
