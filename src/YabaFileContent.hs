module YabaFileContent (
  YabaFileContent,
  parseYabaFile,
  getLinkTarget,
  isYabaRemove,
  isYabaLink,
  isYabaLogicalLink,
  isYabaPhysicalLink,
) where

import           System.FilePath
import           Types

data YabaFileContent = Delete | LogicalLink FilePath | PhysicalLink FilePath
  deriving (Show, Read)

parseYabaFile :: JabaContent -> YabaFileContent
parseYabaFile fileContent = (parse . lines . unJabaContent) fileContent
  where
    parse :: [String] -> YabaFileContent
    parse []                   = error "yaba file is empty"
    parse ("#yaba1" : line : _) = read line
    parse _ = error $ "Bad version of Yaba file, probably old version of yaba tool: " ++ show fileContent

isYabaRemove :: YabaFileContent -> Bool
isYabaRemove Delete = True
isYabaRemove _      = False

isYabaLogicalLink :: YabaFileContent -> Bool
isYabaLogicalLink (LogicalLink _) = True
isYabaLogicalLink _               = False

isYabaPhysicalLink :: YabaFileContent -> Bool
isYabaPhysicalLink (PhysicalLink _) = True
isYabaPhysicalLink _                = False

isYabaLink x = isYabaPhysicalLink x || isYabaLogicalLink x

getLinkTarget :: YabaFileContent -> FilePath
getLinkTarget (LogicalLink target)  = target
getLinkTarget (PhysicalLink target) = target
