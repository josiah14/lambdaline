import System.Directory
import Data.Text as T
import Git

main :: IO ()
main = do
    getCurrentDirectory >>= print

