module Tests where

import System.Exit (exitFailure)
import Control.Exception (try, SomeException, evaluate)

assert :: String -> Bool -> IO ()
assert msg cond =
  if cond
    then pure ()
    else do
        putStrLn ("ASSERT FAILED: " <> msg)
        exitFailure

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq msg expected actual =
    assert
        (msg <> "Expected: " <> show expected <> " but got:  " <> show actual)
        (expected == actual)

assertThrows :: String -> IO a -> IO ()
assertThrows msg action = do
    r <- try (action >> pure ()) :: IO (Either SomeException ())
    case r of
        Left _ ->
            pure ()
        Right _ -> do
            putStrLn ("Assert failed (expected exception): " <> msg)
            exitFailure
