module Main where

import GHC (runGhc, compileToCoreSimplified, CoreModule, setSessionDynFlags, getSessionDynFlags, cm_binds)
import GHC.Paths (libdir)

import GhcMonad
import DynFlags
import CoreSyn
-- for printing
import GHC.IO.Handle.FD (stdout)
import Outputable (ppr, printForUser, neverQualify)

import GHCFlite.Translate (translate, pretty)

-- import Flite.Interp (interp)
-- import Flite.InterpFrontend (frontend)
-- import Flite.Inline (InlineFlag(..))
-- import Flite.Pretty

main :: IO ()
main = do
    core <- runGhc (Just libdir) (coreModule "../test/test_main.hs")
    let prog = translate $ cm_binds core
    putStrLn $ pretty prog
    --let prog = frontend (NoInline,NoInline) prog
    --print $ prog
    --print $ interp (NoInline,NoInline) prog
    return ()

-- | Sets up a session in the GhcMonad and
-- compiles the given file to a CoreModule
coreModule :: (GhcMonad m) => String -> m CoreModule
coreModule fileName = do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    -- setSessionDynFlags $ updOptLevel 2 dflags
    core <- compileToCoreSimplified fileName
    liftIO . printForUser dflags stdout neverQualify . ppr $ cm_binds core
    return core
