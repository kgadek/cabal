{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Simple.Build.Globbing (expandExtraFileGlobs) where
import Distribution.Simple.Utils ( matchDirFileGlob )

import Data.List ( intercalate )
import Data.List.Split (splitOn)
import System.FilePath (normalise)
import System.FilePath.Posix (dropExtension)

import Distribution.ModuleName (ModuleName)
import Distribution.PackageDescription ( PackageDescription(..), Library(..))
import Distribution.ModuleName (ModuleName(..))


expandExtraFileGlobs :: PackageDescription -> IO PackageDescription
expandExtraFileGlobs pkgDesc =
  case library pkgDesc of
    Nothing -> return pkgDesc
    Just thelib@(exposedModules -> expMod) -> do
      fpaths <- mapM expandGlobs expMod 
      return pkgDesc{ 
        library = Just thelib{ 
          exposedModules = ModuleName <$> (filter (not.null) . concat $ fpaths)
        }
      }
  where expandGlobs :: ModuleName -> IO [[String]]
        expandGlobs (ModuleName modBlocks) 
          | last modBlocks == "*" =
              (fmap.fmap) 
                (splitOn "/" . normalise . (dirName ++) . dropExtension)
                (matchDirFileGlob dirName "*.hs")
            where dirName = takeWhile (/= '*') . intercalate "/" $ modBlocks
        expandGlobs (ModuleName modBlocks) = 
              return . return $ modBlocks
