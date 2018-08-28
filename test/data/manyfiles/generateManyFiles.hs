
import System.FilePath
import System.Directory

toStrList = map (tail . show . (+10000))   

writeFileBundle dir numberList = do 
  createDirectoryIfMissing True dir
  mapM (\x -> writeFile  (dir </> x) x) $ toStrList numberList

main = do    
  writeFileBundle  "backup/0000-many-files.yaba-slice/root" [0..4999]
  writeFileBundle  "source-of-root" [5000..9999]
  