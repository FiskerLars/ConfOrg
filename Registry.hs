{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Yaml
import Data.ByteString.Char8
import GHC.Generics

data Registry = Registry {name :: String, titel:: String} deriving (Generic,Show)
instance FromJSON Registry

main:: IO ()
main = case decode $ pack "name: Lars\ntitel: Dr." of
        Just x -> print $ show (x ::Registry)
        Nothing -> print "Nothing"
