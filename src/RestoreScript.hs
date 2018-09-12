module RestoreScript (
  makeRestoreScript
) where

import           Data.Bifunctor
import           Data.List
import qualified Data.Set       as S

import           Lib
import           Lodree
import           Text.Printf
import           TurboWare
import           Types

data Cmd = MkDir | CpFile FilePath deriving (Show, Eq, Ord)

slashni = replaceBacklashesToSlashes . replaceVerticalToSlashes

makeRestoreScript :: Lodree -> [String]
makeRestoreScript lodree =
    let cmdList =  S.toList . S.fromList $ (first namesToPath)  <$> (
         takeRestoreTuples lodree >>= (\ (rp, originalPath) ->
                (rp, CpFile originalPath) :
                zip (tails . tail $ rp) (repeat MkDir)
             ))
      in  fmap (replaceVerticalToSlashes . toShallCommand) cmdList

          --   let restoreTuples = (. bimap (slashni . namesToPath) slashni) <$> takeRestoreTuples lodreeBackupCurrent

toShallCommand :: (FilePath, Cmd) -> String
toShallCommand (path, MkDir)     = printf "ymkdir \"%s\""  (dropSlash path)
toShallCommand (path, CpFile op) = printf "ycopy \"%s\" \"%s\"" (dropSlash op)  (dropSlash path)

dropSlash :: String -> String
dropSlash []        = []
dropSlash ('/' : x) = x
dropSlash x         = x
