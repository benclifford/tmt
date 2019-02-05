{-# OPTIONS_GHC -Wall -Werror #-}
{-# Language ApplicativeDo #-}

{-|
The command line options for tmt
-}

module Options where

import Control.Applicative ( some, (<**>), (<|>) )
import qualified Options.Applicative as OA

type Branch = String

data CommandLine
  = Init
  | Add Branch
  | Prepend Branch
  | Remove Branch
  | Materialise
  | MaterialiseAdhoc [Branch]
  | Status
  | On [String]

getCommandLine :: IO CommandLine
getCommandLine = OA.execParser opts

opts :: OA.ParserInfo CommandLine
opts = OA.info (optsParser <**> OA.helper) info

info :: OA.InfoMod CommandLine
info = OA.header "Temporary Merge Tool"
    <> OA.fullDesc

optsParser :: OA.Parser CommandLine
optsParser = OA.subparser (
    OA.command "init" (OA.info (pure Init) (OA.progDesc "Initialise an empty stack"))
 <> OA.command "add" (OA.info (Add <$> OA.strArgument (OA.metavar "BRANCH")) (OA.progDesc "Add a branch to the end of the stack"))
 <> OA.command "prepend" (OA.info (Prepend <$> OA.strArgument (OA.metavar "BRANCH")) (OA.progDesc "Add a branch to the start of the stack"))
 <> OA.command "remove" (OA.info (Remove <$> OA.strArgument (OA.metavar "BRANCH")) (OA.progDesc "Remove a branch from the stack"))
 <> OA.command "materialise" (OA.info (pure Materialise) (OA.progDesc "Materialise the current stack into the working directory"))
 <> OA.command "materialise-adhoc" (OA.info (MaterialiseAdhoc <$> remainingArguments "BRANCH") (OA.progDesc "Materialise an adhoc collection of branches without affecting stack state (EXPERIMENTAL)"))
 <> OA.command "status" (OA.info (pure Status) (OA.progDesc "Show status"))
 <> OA.command "on" (OA.info (On <$> onArguments) (OA.progDesc "Show status"))
  )
  <|>
  OA.subparser (
    OA.command "materialize-adhoc" (OA.info (MaterialiseAdhoc <$> remainingArguments "BRANCH") (OA.progDesc "Materialise an adhoc collection of branches without affecting stack state (EXPERIMENTAL)"))
 <> OA.command "materialize" (OA.info (pure Materialise) (OA.progDesc "Materialise the current stack into the working directory")) -- TODO: hide from help?
 <> OA.commandGroup "Transatlantic aliases:"
  )


remainingArguments :: String -> OA.Parser [String]
remainingArguments metastr = some (OA.strArgument (OA.metavar metastr))

onArguments :: OA.Parser [String]
onArguments = do
  branch <- OA.strArgument (OA.metavar "BRANCH")
  rest <- remainingArguments "COMMAND"
  pure (branch:rest)

