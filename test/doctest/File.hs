module File (getFilesRecursive) where

import           Control.Monad       (filterM)
import           Data.Foldable       (fold)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      listDirectory)
import           System.FilePath     ((</>))
import           System.IO.Unsafe    (unsafeInterleaveIO)

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive fp = getDirFiltered (const (pure True)) fp >>= filterM doesFileExist

getDirFiltered :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
getDirFiltered p fp = do
    all' <- listDirectory fp
    all'' <- filterM p (mkRel <$> all')
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure all''
        ds -> do
            next <- unsafeInterleaveIO $ foldMapA (getDirFiltered p) ds
            pure $ all'' ++ next
    where mkRel = (fp </>)
          foldMapA = (fmap fold .) . traverse