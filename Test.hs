import Data.Compact
import qualified Data.Compact.Serialize as C

main :: IO ()
main = do
    let v = (5 :: Int)
    c <- compact v
    C.writeFile "tmp" c
    Right v' <- C.readFile "tmp"
    print $ v == getCompact v'
