module Parser (
  fileToWordList,
  printGameRes
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import GHC.IO (catchAny)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import System.Exit (die)

-- Process File Input
fileToWordList :: FilePath -> IO [String]
fileToWordList path = do
  handle <- catchAny (openFile path ReadMode) $ \_ -> do
    die $ "Could not open file: " ++ path
  contents <- B.hGetContents handle
  return $ Prelude.map BC.unpack $ BC.lines contents

-- Prints out the result of wordle games
-- Example output for one of the results:
-- "gorge"[4]: ["soare","rurus","forge","gorge"]
printGameRes :: [[String]] -> [String] -> IO ()
printGameRes [] _ = return ()
printGameRes _ [] = return ()
printGameRes (x:xs) (y:ys) = do
  putStr $ (show y) ++ "[" ++ (show $ length x) ++ "]: " ++ (show x) ++ "\n"
  printGameRes xs ys