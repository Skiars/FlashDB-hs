{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import FlashDB.KVDB

main :: IO ()
main = do
  db <- open "test" "test.db" 4096 (4096 * 4)
  set db "user" "person"
  (user :: Maybe String) <- get db "user"
  putStrLn $ "user: " ++ show (take 100 <$> user)
  remain <- remainSize db
  putStrLn $ "remain size: " ++ show remain
  return ()
