{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad (forM_, when)
import qualified Data.ByteString.UTF8 as BS8
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(Proxy))
import qualified System.Exit as IO
import qualified System.Environment as IO
import           System.Console.GetOpt

import           Data.Parameterized.Some (Some(..))

import qualified SemMC.Architecture as A
import qualified SemMC.Architecture.PPC32 as PPCS
import qualified SemMC.Architecture.PPC32.Opcodes as PPCS

data Arg =
    Help
    | Arch String
    deriving (Eq, Show)

arguments :: [OptDescr Arg]
arguments =
    [ Option "h" ["help"] (NoArg Help)
      "Show this help"

    , Option "a" ["arch"] (ReqArg Arch "ARCHNAME")
      ("The name of the architecture to test (choices: " <>
      intercalate ", " allArchNames <> ")")
    ]

data Config =
    Config { configShowHelp   :: Bool
           , configArchName   :: Maybe String
           }

defaultConfig :: Config
defaultConfig =
    Config { configShowHelp   = False
           , configArchName   = Nothing
           }

data ArchImpl where
    ArchImpl :: forall proxy arch .  (A.Architecture arch)
             => String
             -> proxy arch
             -> [Some ((A.Opcode arch) (A.Operand arch))]
             -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
             -> ArchImpl

archImplName :: ArchImpl -> String
archImplName (ArchImpl n _ _ _) = n

ppc32Arch :: ArchImpl
ppc32Arch =
    ArchImpl "ppc32" (Proxy @PPCS.PPC) PPCS.allOpcodes PPCS.allSemantics

knownArchs :: [ArchImpl]
knownArchs =
    [ ppc32Arch
    ]

allArchNames :: [String]
allArchNames = archImplName <$> knownArchs

usage :: IO ()
usage = do
    pn <- IO.getProgName
    putStrLn $ "Usage: " <> pn <> " [options]"
    let msg = unlines [ "\nAt a minimum, an architecture must be specified."
                      ]
    putStrLn $ usageInfo msg arguments

configFromArgs :: IO Config
configFromArgs = do
    stringArgs <- IO.getArgs
    let (args, rest, errs) = getOpt Permute arguments stringArgs

    when (not $ null $ errs <> rest) $ do
        usage
        IO.exitFailure

    let processArg _ Nothing = Nothing
        processArg arg (Just c) =
            case arg of
                Help ->
                    return $ c { configShowHelp = True }
                Arch a ->
                    return $ c { configArchName = Just a }

    case foldr processArg (Just defaultConfig) args of
        Nothing -> usage >> IO.exitFailure
        Just c -> return c

findArch :: String -> Maybe ArchImpl
findArch n =
    case filter ((== n) . archImplName) knownArchs of
        [a] -> return a
        _ -> Nothing

main :: IO ()
main = do
    cfg <- configFromArgs

    when (configShowHelp cfg) $
        usage >> IO.exitFailure

    case configArchName cfg of
        Nothing -> usage >> IO.exitFailure
        Just an -> case findArch an of
            Nothing -> usage >> IO.exitFailure
            Just (ArchImpl _ _ opcodes semantics) -> do
                -- Get list of all opcodes with no semantics
                forM_ opcodes $ \opc ->
                    case lookup opc semantics of
                        Nothing -> print opc
                        Just _ -> return ()
