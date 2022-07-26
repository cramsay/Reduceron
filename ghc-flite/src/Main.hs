module Main where

import System.Environment
import GHC (runGhc, compileToCoreSimplified, CoreModule, setSessionDynFlags, getSessionDynFlags, cm_binds)
import GHC.Paths (libdir)

import GhcMonad
import DynFlags
import CoreSyn
-- for printing
import GHC.IO.Handle.FD (stdout)
import Outputable (ppr, printForUser, neverQualify)
import GHCFlite.Translate (translate)

import Flite.Pretty

data Opt = O2 | NoOpt

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["-o2", filename] -> go O2 filename
      ["-noopt", filename] -> go NoOpt filename
      _ -> putStrLn "Expecting optimisation flag (-o2 | -noopt) and filename"
  where go opt filename =
          do core <- runGhc (Just libdir) (coreModule opt filename)
             let prg = translate $ cm_binds core
             putStrLn $ pretty prg
             return ()

-- | Sets up a session in the GhcMonad and
-- compiles the given file to a CoreModule
coreModule :: (GhcMonad m) => Opt -> String -> m CoreModule
coreModule opt fileName =
  do dflags <- getSessionDynFlags
     setSessionDynFlags $ case opt of
                            NoOpt -> dflags
                            O2    -> updOptLevel 2 dflags
     core <- compileToCoreSimplified fileName
     --liftIO . printForUser dflags stdout neverQualify . ppr $ cm_binds core
     return core

