module Yaba.Process.RestoreScriptMaker (
  makeRestoreScript
)

where

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import qualified Data.Map         as M
import qualified Data.Set         as S
import           Text.Printf

import           Lib
import           TurboWare
import           Types
import           Yaba.Data.Lodree


data Cmd = MkDir | CpFile FilePath (Maybe UTCTime) deriving (Show, Eq, Ord)

slashni = replaceBacklashesToSlashes . replaceVerticalToSlashes

makeRestoreScript :: M.Map FilePath UTCTime -> Lodree -> [String]
makeRestoreScript modificationTimes lodree =
    let cmdList =  S.toList . S.fromList $ (first namesToPath)  <$> (
         takeRestoreTuples lodree >>= (\ (rp, originalPath) ->
                (rp, CpFile originalPath (lookupModificationTime rp)) :
                zip (tails . tail $ rp) (repeat MkDir)
             ))
      in  fmap (replaceVerticalToSlashes . toShallCommand) cmdList
  where
     lookupModificationTime :: RevPath -> Maybe UTCTime
     lookupModificationTime rp = (M.lookup (dropSlash . namesToPath $ rp) modificationTimes)
          --   let restoreTuples = (. bimap (slashni . namesToPath) slashni) <$> takeRestoreTuples lodreeBackupCurrent

toShallCommand :: (FilePath, Cmd) -> String
toShallCommand (path, MkDir)     = printf "ymkdir \"%s\""  (dropSlash path)
toShallCommand (path, CpFile op maybeTime) =
    printf "ycp \"%s\" \"%s\"" (dropSlash op)  (dropSlash path)
    ++ case maybeTime of
         Nothing -> ""
         Just time -> printf "\nymodtime \"%s\" %s" (dropSlash path) (show time)


dropSlash :: String -> String
dropSlash []        = []
dropSlash ('/' : x) = x
dropSlash x         = x
