{-# Language MultiWayIf #-}

module Main where

import Data.List (intersperse)
import Data.Monoid (mempty)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Traversable (for)
import System.Environment (getArgs)
import qualified System.Process as Process

import Lib

main :: IO ()
main = do
  putStrLn "tmt: temporary merge tool"

  -- there should be a tmt context
  -- that knows which branches we've claimed to put in
  -- and how.

  -- that can be a serialised Haskell value for now, in pwd
  -- but should go in .git eventually.

  args <- getArgs

  let cmd = args !! 0

  putStrLn $ "Command is " ++ cmd

  if
    | cmd == "init" -> initContext
    | cmd == "add" -> withContext $ \ctx -> addBranch ctx (args !! 1)
    | cmd == "materialise" -> withContext $ \ctx -> materialiseContext ctx
    | cmd == "status" -> withContext $ \ctx -> showStatus ctx
    | cmd == "on" -> withContext $ \ctx -> runOn (tail args) ctx
    | True -> error "Unknown command"


initContext :: IO ()
initContext = do
  -- TODO: should check that there isn't a context already
  writeContext []

withContext :: (Context -> IO Context) -> IO ()
withContext act = do
  ctx <- readContext
  putStrLn $ "Loaded context is: " ++ show ctx
  newCtx <- act ctx
  writeContext newCtx

runOn :: [String] -> Context -> IO Context
runOn args ctx = do
  putStrLn $ "Context is: " ++ show ctx

  -- TODO: we should check the HEAD commit is actually the last
  -- commit we made using materialise, so as to not lose track
  -- of commits that we've made

  -- TODO: check that 'on' target is actually in the materialised
  -- context (although this might be desirable to override in
  -- some cases?)

  -- hopefully this will keep any changes across the checkout
  run $ "git checkout " ++ (args !! 0)
  -- now run the command
  runArgs $ tail args
  -- now materialise again - potentially changed by the args command
  newCtx <- materialiseContext ctx

  return newCtx

showStatus :: Context -> IO Context
showStatus ctx = do
  putStrLn $ "Context is: " ++ show ctx
  return ctx

addBranch :: Context -> BranchName -> IO Context
addBranch ctx branchName = do
  putStrLn $ "Adding branch " ++ branchName

  -- Additions should happen deliberately on the end so that the
  -- head of the context keeps a special position as the "main"
  -- branch being worked on.
  let newCtx = ctx ++ [branchName]

  materialiseContext newCtx
  return newCtx

-- | Given a desired context, make the current checkout
--   look like that.
--   That means, for now, checking out a particular commit,
--   not a branch name, and then merging in other stuff.
materialiseContext :: Context -> IO Context
materialiseContext ctx = do
  putStrLn $ "Materialising context: " ++ show ctx

  -- Checkout commit ID of head of context, detached so that
  -- we can make new commits which are not changing branch refs.

  run $ "git checkout --detach " ++ head ctx

  -- TODO: for each entry in tail of context, merge in that
  --       entry - unlike the HEAD, it doesn't need to be resolved
  --       to a commit ID.

  for (tail ctx) $ \branch -> do
    let msg = "tmt: merging in " ++ branch
    run $ "git merge --no-ff -m '" ++ msg ++ "' " ++ branch

  return ctx

run :: String -> IO ()
run command = do
  putStrLn $ "+ " ++ command 
  Process.callCommand command

runArgs :: [String] -> IO ()
runArgs commands = do
  putStrLn $ "+ " ++ (concat $ intersperse " // " commands)
  Process.callProcess (head commands) (tail commands)

type BranchName = String

-- | Represents the stored context. At present, that is the list of
--   branches that we are using, but later might include commit
--   IDs for the branches merged in, and for the resulting commit.

type Context = [BranchName]

readContext :: IO Context
readContext = do
  putStrLn "Reading context"
  read <$> readFile "tmt-context"

writeContext :: Context -> IO ()
writeContext ctx = do
  putStrLn $ "Write context: " ++ show ctx
  ((writeFile "tmt-context") . show) ctx
  return ()
