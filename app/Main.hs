{-# Language MultiWayIf #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Monad (void, when)
import Data.List (intersperse)
import Data.Traversable (for)
import qualified System.Console.ANSI as ANSI
import System.Environment (getArgs)
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified System.Process as Process

main :: IO ()
main = do
  logDebug "temporary merge tool"

  -- there should be a tmt context
  -- that knows which branches we've claimed to put in
  -- and how.

  -- that can be a serialised Haskell value for now, in pwd
  -- but should go in .git eventually.

  args <- getArgs

  when (length args == 0) $ error "Must supply a tmt subcommand"
  let cmd = args !! 0

  if
    | cmd == "init" -> initContext
    | cmd == "add" -> withContext $ \ctx -> addBranch ctx (args !! 1)
    | cmd == "prepend" -> withContext $ \ctx -> prependBranch ctx (args !! 1)
    | cmd == "remove" -> withContext $ \ctx -> removeBranch ctx (args !! 1)
    | cmd == "materialise" -> withContext $ \ctx -> materialiseContext ctx
    | cmd == "materialize" -> withContext $ \ctx -> materialiseContext ctx
    | cmd == "materialise-adhoc" -> materialiseAdhocContext (tail args)
    | cmd == "materialize-adhoc" -> materialiseAdhocContext (tail args)
    | cmd == "status" -> withContext $ \ctx -> showStatus ctx
    | cmd == "on" -> withContext $ \ctx -> runOn (tail args) ctx
    | True -> error $ "Unknown command: " ++ cmd


initContext :: IO ()
initContext = do
  -- TODO: should check that there isn't a context already
  writeContext []

withContext :: (Context -> IO Context) -> IO ()
withContext act = do
  ctx <- readContext
  logDebug $ "Loaded context is: " ++ formatContext ctx
  newCtx <- act ctx
  writeContext newCtx

runOn :: [String] -> Context -> IO Context
runOn args ctx = do
  logDebug $ "Context is: " ++ formatContext ctx

  -- TODO: we should check the HEAD commit is actually the last
  -- commit we made using materialise, so as to not lose track
  -- of commits that we've made

  -- TODO: check that 'on' target is actually in the materialised
  -- context (although this might be desirable to override in
  -- some cases?)

  -- hopefully this will keep any changes across the checkout
  stashOver $ run $ "git checkout " ++ (args !! 0)
  -- now run the command
  runArgs $ tail args
  -- now materialise again - potentially changed by the args command
  newCtx <- materialiseContext ctx

  return newCtx

-- | Does a git stash, runs the action, and pops the stash.
-- This is intended to allow switching to a branch (in the supplied
-- action) and preserving uncommitted changes.
-- TODO: error handling: if the action throws an error, we should
-- log that a stash has been made, before passing on the error, so
-- that the user gets told where their changes have disappeared to.
stashOver :: IO a -> IO a
stashOver act = do
  checkStashEmpty
  run "git stash save 'WIP from tmt stashOver'"
  r <- act
  em <- isStashEmpty
  when (not em) $ run "git stash pop"
  return r

checkStashEmpty :: IO ()
checkStashEmpty = do
  em <- isStashEmpty
  when (not em) $ error "This tmt mode cannot operate with a non-empty git stash"

isStashEmpty :: IO Bool
isStashEmpty = do
  s <- runRead "git stash list"
  return (s == "")

showStatus :: Context -> IO Context
showStatus ctx = do
  logInfo $ "Context is: "
  case ctx of
    [] -> void $ putStrLn ". empty"
    _ -> void $ for ctx $ \c -> putStrLn $ "> merge " ++ c
  return ctx

prependBranch :: Context -> BranchName -> IO Context
prependBranch ctx branchName = do
  logInfo $ "Adding branch " ++ branchName

  -- Adds onto the start of the stack - perhaps because branch
  -- is intended to be "closer" to master than the branches near
  -- the end. (closer = less divergant, intended to be merged
  -- sooner?)

  let newCtx = [branchName] ++ ctx

  materialiseContext newCtx


addBranch :: Context -> BranchName -> IO Context
addBranch ctx branchName = do
  logInfo $ "Adding branch " ++ branchName

  -- Additions should happen deliberately on the end so that the
  -- head of the context keeps a special position as the "main"
  -- branch being worked on.
  let newCtx = ctx ++ [branchName]

  materialiseContext newCtx

removeBranch :: Context -> BranchName -> IO Context
removeBranch ctx branchName = do
  logInfo $ "Removing branch " ++ branchName

  let newCtx = filter (/= branchName) ctx

  when (newCtx == ctx) $ error $ "No branch " ++ branchName ++ " was removed"

  materialiseContext newCtx


-- | Given a desired context, make the current checkout
--   look like that.
--   That means, for now, checking out a particular commit,
--   not a branch name, and then merging in other stuff.
materialiseContext :: Context -> IO Context
materialiseContext ctx = do
  logInfo $ "Materialising context: " ++ formatContext ctx

  -- Checkout commit ID of head of context, detached so that
  -- we can make new commits which are not changing branch refs.

  run $ "git checkout --detach " ++ head ctx

  -- TODO: for each entry in tail of context, merge in that
  --       entry - unlike the HEAD, it doesn't need to be resolved
  --       to a commit ID.

  void $ for (tail ctx) $ \branch -> do
    let msg = "tmt: merging in " ++ branch
    mergeRerere msg branch

  return ctx

-- | Given a list of branches to merge together, create an ad-hoc
--   merge of them without modifying the list of branches in the
--   context file. This allows a different combination of branches
--   to be merged on the fly, without needing to rearrange the
--   context.
materialiseAdhocContext :: [String] -> IO ()
materialiseAdhocContext branches = do
  logInfo $ "Materialising ad-hoc context: " ++ show branches
  void $ materialiseContext branches
  logInfo "Ad-hoc materialisation complete"

mergeRerere :: String -> String -> IO ()
mergeRerere msg branch = do
    rerere <- isRerereEnabled
    if not rerere

    -- a simple merge, throwing an exception if there is an error
    then run $ "git merge --no-ff -m '" ++ msg ++ "' " ++ branch

    else do
      (mergeExit,mergeStdout, mergeStderr) <- runReadRet $ "git merge --no-ff -m '" ++ msg ++ "' " ++ branch
      when (mergeExit /= Exit.ExitSuccess) $ do
        logError "Merge failed"
        logInfo "Merge stdout:"
        putStrLn mergeStdout
        logInfo "Merge stderr:"
        putStrLn mergeStderr
        (remainingExit,rerereStdout,_rerereStderr) <- runReadRet "git rerere remaining"
        if | remainingExit == Exit.ExitSuccess && rerereStdout == "" -> do
               logInfo "git rerere reports no remaining conflicts, so committing"
               run $ "git commit -a -m '" ++ msg ++ " -- attempted rerere fix'"
           | True -> error "rerere was not able to fix everything"

run :: String -> IO ()
run command = do
  putStrLn $ "+ " ++ command 
  Process.callCommand command

runArgs :: [String] -> IO ()
runArgs commands = do
  putStrLn $ "+ " ++ (concat $ intersperse " // " commands)
  Process.callProcess (head commands) (tail commands)

runRead :: String -> IO String
runRead command = do
  putStrLn $ "+ " ++ command
  Process.readCreateProcess (Process.shell command) ""

runReadRet :: String -> IO (Exit.ExitCode, String, String)
runReadRet command = do
  putStrLn $ "+ " ++ command
  Process.readCreateProcessWithExitCode (Process.shell command) ""

type BranchName = String

-- | Represents the stored context. At present, that is the list of
--   branches that we are using, but later might include commit
--   IDs for the branches merged in, and for the resulting commit.

type Context = [BranchName]

readContext :: IO Context
readContext = do
  logDebug "Reading context"
  ctxPath <- getContextPath
  read <$> readFile ctxPath

writeContext :: Context -> IO ()
writeContext ctx = do
  logDebug $ "Writing context: " ++ formatContext ctx
  ctxPath <- getContextPath
  ((writeFile ctxPath) . show) ctx
  return ()

getContextPath :: IO String
getContextPath = do
  p <- Process.readProcess "sh" ["-c",
    "GIT_DIR=$(readlink -f $(git rev-parse --git-dir)) git rev-parse --git-path tmt-context"] ""
  return (head $ lines p)

formatContext :: Context -> String
formatContext ctx = concat $ intersperse ", " $ ctx

getGitConfig :: String -> IO (Maybe String)
getGitConfig key = do
  (exit, stdout, _stderr) <- runReadRet ("git config " ++ key)
  case exit of
    Exit.ExitSuccess -> return (Just stdout)
    _ -> return Nothing

isRerereEnabled :: IO Bool
isRerereEnabled = do
  mc <- getGitConfig "rerere.enabled"
  return (mc == (Just "1\n"))

logError :: String -> IO ()
logError msg = 
  logWithSGR
    [ANSI.Reset, ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
    msg

logInfo :: String -> IO ()
logInfo msg = 
  logWithSGR
    [ANSI.Reset, ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    msg

logDebug :: String -> IO ()
logDebug msg = 
  logWithSGR
    [ANSI.Reset, ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
    msg

logWithSGR :: [ANSI.SGR] -> String -> IO ()
logWithSGR sgr msg = do
  setSGRIfAvailable sgr
  putStrLn ("tmt: " ++ msg)
  setSGRIfAvailable [ANSI.Reset]
  IO.hFlush IO.stdout

setSGRIfAvailable :: [ANSI.SGR] -> IO ()
setSGRIfAvailable sgr = do
  p <- ANSI.hSupportsANSI IO.stdout
  when p $ ANSI.setSGR sgr

