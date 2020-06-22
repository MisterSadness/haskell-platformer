module HighScore(updateScores) where

import System.IO
import Text.Read(readMaybe)
import Data.Maybe
import System.Directory
import Control.Monad
import Control.Arrow

scoreFile :: String
scoreFile = "scores.txt"

-- | Inserts a new score into a list stored in a predefined text file
updateScores :: Int -> IO ()
updateScores score = do
    fileExists <- doesFileExist scoreFile
    fileAccesible <- checkPermission $ readable &&& writable >>> uncurry (&&)

    if fileExists && fileAccesible
        then do
            oldScores <- readScores
            writeScores $ insertScore score oldScores
        else unless fileExists $ writeScores [score]

checkPermission :: (Permissions -> Bool)-> IO Bool
checkPermission predicate = doesFileExist scoreFile >>= \e -> if e then predicate <$> getPermissions scoreFile else return False

readFileWithoutClosing :: Handle -> IO [String]
readFileWithoutClosing h = (hIsEOF h) >>= \b -> if not b then do
    l <- hGetLine h
    fmap (l:) $ readFileWithoutClosing h
    else return []

readScores :: IO [Int]
readScores = do
    handle <- openFile scoreFile ReadMode
    scores <- fromMaybe [] . mapM readMaybe <$> readFileWithoutClosing handle
    hClose handle
    return scores

insertScore :: Int -> [Int] -> [Int]
insertScore s [] = [s]
insertScore s (x:xs)
    | x > s = x:insertScore s xs
    | otherwise = s:x:xs

writeScores :: [Int] -> IO ()
writeScores scores = withFile scoreFile WriteMode $ \handle -> mapM_ (hPrint handle) scores
