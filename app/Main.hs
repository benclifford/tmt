{-# Language MultiWayIf #-}

module Main where

import Data.Monoid (mempty)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Traversable (for)
import System.Environment (getArgs)
import qualified Turtle as Turtle

import Lib

main :: IO ()
main = do
  putStrLn "tmt: temporary merge tool"

  -- there should be a tmt context
  -- that knows which branches we've claimed to put in
  -- and how.

  -- that can be a serialised Haskell value for now, in pwd
  -- but should go in .git eventually.

  ctx <- readContext

  putStrLn $ "Loaded context is: " ++ show ctx

  args <- getArgs

  let cmd = args !! 0

  putStrLn $ "Command is " ++ cmd

  newCtx <- if
    | cmd == "add" -> addBranch ctx (args !! 1)
    | cmd == "materialise" -> materialiseContext ctx
    | cmd == "status" -> showStatus ctx
    | True -> error "Unknown command"

  writeContext newCtx

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
    run $ "git merge --no-ff " ++ branch

  return ctx

run :: String -> IO ()
run command = do
  putStrLn $ "+ " ++ command 
  Turtle.shells (Text.pack command) mempty

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
