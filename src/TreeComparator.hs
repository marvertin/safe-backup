module TreeComparator (

) where


import           LogicalDirTree

data DirCompare = QDir FileName [(FileName, DirCompare)]
 | QLeft Lodree
 | QCRight Lodree
 | QBoth Lodree Lodree

compareTrees :: Lodree -> Lodree -> DirCompare
compareTrees _ = QLeft

-- data LfInfo = LfInfo { physPath :: FilePath}
--data Ree = Ree { physPathx :: FilePath, size :: FileSize, hash :: Hash } deriving (Show)
--data DRee = DRee { dsize :: FileSize, dcount :: Int, dhash :: Hash } deriving (Show)

--data Lodree = LFile Ree
  --          | LDir DRee [(FileName, Lodree)]
    --        deriving (Show)
