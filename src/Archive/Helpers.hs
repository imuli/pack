{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

module Archive.Helpers ( runCommand, newArchiveType ) where

import System.FilePath
import System.IO
import System.Process hiding (runCommand)
import System.Exit

import Language.Haskell.TH
import qualified Text.ParserCombinators.ReadPrec as RPrec
import qualified Text.ParserCombinators.ReadP as RP

class Handlable a where
    toReadH :: a -> IO Handle
    toWriteH :: a -> IO Handle

instance Handlable Handle where
    toReadH x = return x
    toWriteH x = return x

instance Handlable FilePath where
    toReadH file = openBinaryFile file ReadMode
    toWriteH file = openBinaryFile file WriteMode

runCommand :: (Handlable a, Handlable b) => FilePath -> a -> b -> String -> [String] -> IO (Either Int String)
runCommand dir input output cmd args = do
    iH <- toReadH input
    oH <- toWriteH output
    (_,_,_,p) <- createProcess (proc cmd args){ cwd = Just dir
                                              , close_fds = True
                                              , std_in = UseHandle iH
                                              , std_out = UseHandle oH
                                              }
    exit <- waitForProcess p
    case exit of
        ExitSuccess -> return $ Left 0
        ExitFailure n -> return $ Right $ show n

-- | This mess turns 
newArchiveType :: String -> ExpQ -> ExpQ -> ExpQ -> [String] -> Q [Dec]
newArchiveType n buildQ extractQ idQ exts = do
    build <- funD (mkName "builder") [clause [] (normalB buildQ) []]
    extract <- funD (mkName "extracter") [clause [] (normalB extractQ) []]
    identify <- funD (mkName "ident") [clause [] (normalB $ returnQ idQ) []]
    let ai = InstanceD [] (insT "Archive") [build, extract, identify]
    let readB = returnQ $ appE [| RPrec.lift |] $ appE [| RP.string |] $ extE
    read <- funD (mkName "readPrec") [clause [] (normalB $ readB) []]
    let ri = InstanceD [] (insT "Read") [read]
    show <- funD (mkName "show") [clause [wildP] (normalB extE) []]
    let si = InstanceD [] (insT "Show") [show]
    return [d, ai, ri, si]
  where
    extE = litE $ StringL $ head exts
    insT t = AppT (ConT (mkName t)) (ConT name)
    returnQ q = uInfixE q [| (>>) |] $ appE [| return |] $ conE name
    name = mkName n
    d = DataD [] name [] [NormalC name []] []
