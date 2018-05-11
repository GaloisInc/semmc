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
import           System.FilePath ( (<.>) )
import           System.Console.GetOpt

import           Data.Parameterized.Some (Some(..))

import qualified SemMC.Architecture as A
import qualified SemMC.TH as A

import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC32.Opcodes as PPC32

import qualified SemMC.Architecture.PPC64 as PPC64
import qualified SemMC.Architecture.PPC64.Opcodes as PPC64

import qualified SemMC.ARM as ARM
import qualified SemMC.Architecture.ARM.Opcodes as ARM

data Arg =
    Help
    | Arch String
    | DefinedOnly
    | UndefinedOnly
    | LoadPath FilePath
    deriving (Eq, Show)

arguments :: [OptDescr Arg]
arguments =
    [ Option "h" ["help"] (NoArg Help)
      "Show this help"

    , Option "a" ["arch"] (ReqArg Arch "ARCHNAME")
      ("The name of the architecture to test\n(choices: " <>
      intercalate ", " allArchNames <> ")")

    , Option "m" ["missing-only"] (NoArg UndefinedOnly)
      "Show only opcodes with no semantics (default behavior)"

    , Option "d" ["defined-only"] (NoArg DefinedOnly)
      "Show only opcodes with semantics"

    , Option "p" ["path"] (ReqArg LoadPath "PATH")
      ("Load semantics files from the specified directory\n(if " <>
       "omitted, use the compiled-in semantics; otherwise\nif " <>
       "specifies multiple times, load from specified paths in order)")
    ]

data Config =
    Config { configShowHelp   :: Bool
           , configArchName   :: Maybe String
           , configShowDefined :: Bool
           , configPaths :: [FilePath]
           }

defaultConfig :: Config
defaultConfig =
    Config { configShowHelp   = False
           , configArchName   = Nothing
           , configShowDefined = False
           , configPaths      = []
           }

data ArchImpl where
    ArchImpl :: forall proxy arch .  (A.Architecture arch)
             => String
             -> proxy arch
             -> [Some ((A.Opcode arch) (A.Operand arch))]
             -> [(Some ((A.Opcode arch) (A.Operand arch)), BS8.ByteString)]
             -> (Some ((A.Opcode arch) (A.Operand arch)) -> String)
             -> ArchImpl

archImplName :: ArchImpl -> String
archImplName (ArchImpl n _ _ _ _) = n

ppc32Arch :: ArchImpl
ppc32Arch =
    ArchImpl "ppc32" (Proxy @PPC32.PPC) PPC32.allOpcodes PPC32.allSemantics
             (\(Some x) -> show x)

ppc64Arch :: ArchImpl
ppc64Arch =
    ArchImpl "ppc64" (Proxy @PPC64.PPC) PPC64.allOpcodes PPC64.allSemantics
             (\(Some x) -> show x)

arm32Arch :: ArchImpl
arm32Arch =
    ArchImpl "arm32" (Proxy @ARM.ARM) ARM.allA32Opcodes ARM.allA32Semantics
             (\(Some x) -> show x)

thumb32Arch :: ArchImpl
thumb32Arch =
    ArchImpl "thumb32" (Proxy @ARM.ARM) ARM.allT32Opcodes ARM.allT32Semantics
             (\(Some x) -> show x)

knownArchs :: [ArchImpl]
knownArchs =
    [ ppc32Arch
    , ppc64Arch
    , arm32Arch
    , thumb32Arch
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
                UndefinedOnly ->
                    return $ c { configShowDefined = False }
                DefinedOnly ->
                    return $ c { configShowDefined = True }
                LoadPath p ->
                    return $ c { configPaths = configPaths c <> [p] }

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

    case configArchName cfg >>= findArch of
        Nothing -> usage >> IO.exitFailure
        Just (ArchImpl _ _ opcodes builtInSemantics showOpc) -> do
            semantics <- case configPaths cfg of
                [] -> return builtInSemantics
                paths -> do
                    let getPair (_, b, c) = (b, c)
                    fmap getPair <$> A.loadSemantics ((<.> "sem") . showOpc) opcodes paths

            forM_ opcodes $ \opc ->
                case lookup opc semantics of
                    Nothing -> when (not $ configShowDefined cfg) $ print opc
                    Just _ -> when (configShowDefined cfg) $ print opc
