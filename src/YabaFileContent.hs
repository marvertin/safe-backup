module YabaFileContent (
  YabaFileContent,
  YabaFileKind,
  parseYabaFile,
  getLinkTarget,
  isYabaRemove,
  isYabaLink,
  jj
) where

import           System.FilePath

data YabaFileContent = YabaFileContent YabaFileKind FilePath
  deriving (Show, Read)

data YabaFileKind
  = JfcDelete
  | JfcRenameFrom FilePath
  | JfcRenameTo FilePath
  | JfcMoveFrom FilePath
  | JfcMoveTo FilePath
  | JfcCopyFrom FilePath
  deriving (Show, Read)

xx = show $ YabaFileContent (JfcMoveFrom "/maintree/A/M/OO") "xxx/yyy/zzzz"

yy :: YabaFileContent
yy = read "YabaFileContent (JfcRenameFrom \"aaa/bbb/ccc\") \"xxx/yyy/zzzz\""

parseYabaFile :: String -> YabaFileContent
parseYabaFile fileContent = (parse . lines) fileContent
  where
    parse :: [String] -> YabaFileContent
    parse []                   = error "yaba file is empty"
    parse ("#yaba1" : line : _) = read line
    parse _ = error $ "Bad version of Yaba file, probably old version of yaba tool: " ++ fileContent

isYabaRemove :: YabaFileContent -> Bool
isYabaRemove (YabaFileContent JfcDelete _)       = True
isYabaRemove (YabaFileContent (JfcRenameTo _) _) = True
isYabaRemove (YabaFileContent (JfcMoveTo _) _)   = True
isYabaRemove _                                   = False

isYabaLink :: YabaFileContent -> Bool
isYabaLink (YabaFileContent (JfcCopyFrom _) _)   = True
isYabaLink (YabaFileContent (JfcRenameFrom _) _) = True
isYabaLink (YabaFileContent (JfcMoveFrom _) _)   = True
isYabaLink _                                     = False

getLinkTarget :: YabaFileContent -> FilePath
getLinkTarget (YabaFileContent (JfcCopyFrom target) _)   = target
getLinkTarget (YabaFileContent (JfcRenameFrom target) _) = target
getLinkTarget (YabaFileContent (JfcMoveFrom target) _)   = target



jj = do
  putStrLn xx
  print yy
