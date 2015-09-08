{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Yaml
import qualified Data.Text as T
import Data.Text.Encoding
--import Data.String.UTF8  hiding (lines, foldr)
--import qualified Data.ByteString.Char8 as BS
--import qualified Data.ByteString.UTF8 as UTF hiding (foldr)
-- import Codec.Binary.UTF8.String (isUTF8Encoded, decodeString)
import GHC.Generics
import Data.Char
import Data.List 
import Data.Maybe
import System.IO

import Debug.Hood.Observe

data Registry = Registry { anrede :: T.Text, vorname :: T.Text, nachname :: T.Text, titel:: Maybe T.Text
                         , affiliation :: T.Text, street :: T.Text
			 , city :: T.Text, postcode :: Integer
			 , email :: T.Text } deriving (Generic,Show)
instance FromJSON Registry

-- TestString:
-- name: Lars\ntitel: Dr.

main:: IO ()
main = runO $
       do
         str <- getContents >>= (return.prettyformat_input.(T.pack))
         case decodeEither' $ encodeUtf8 str of
          Right x -> hPutStrLn stdout $ registry_to_csv (x ::Registry)
          Left err -> hPutStrLn stderr $ (prettyPrintParseException err)
                      ++ "\n" ++ (T.unpack str)


prettyformat_input:: T.Text -> T.Text
prettyformat_input = T.unlines
                     .(map prettyformat_line)
                     .(filter (not.(T.null)))
                     .(T.lines) -- .(reverse).(foldl nextline [])
  where
    prettyformat_line:: T.Text -> T.Text
    prettyformat_line t = let (tag, value) = T.break ((==) ':') t
                          in T.concat [ (T.toLower tag)
                                       , T.singleton ':'
                                       , T.singleton ' '
                                       , (replace_block_like_strings
                                          $ replace_empty
                                          $ (T.strip)
                                          $ T.tail value)]
    replace_empty:: T.Text -> T.Text
    replace_empty t | all_in_emptychars t = T.singleton ' '
                    | otherwise = t
    all_in_emptychars :: T.Text -> Bool
    all_in_emptychars = T.all (\c -> any ((==) c) ("-—-/\\ \t"::String))
    replace_block_like_strings:: T.Text -> T.Text
    replace_block_like_strings t | ('"' /= T.head t)
                                   && (contains ',' t) = T.concat [ T.singleton '"'
                                                                     , t
                                                                     , T.singleton '"' ]
                                 | otherwise = t
    contains:: Char -> T.Text -> Bool
    contains c t = case T.find (== c) t of
      Just _ -> True
      Nothing -> False
                                             
{-
      | (T.head t) == ':'  = T.singleton (T.head t) (dropWhile isSpace $ T.tail t)
                   | otherwise = T.toLower (T.head t):tag_to_lower (T.tail t)
-}
    nextline:: [String] -> Char -> [String]
    nextline [] _     = []
    nextline (l:ls) c | c == '\n' = []:l:ls
                      | otherwise = (l++[c]):ls


registry_to_csv:: Registry -> String
registry_to_csv reg = let
  fields_csv = [anrede
               , (fromMaybe "").titel, vorname, nachname, affiliation, street, (T.pack).show.postcode, city, email] 
  in
   intercalate "; " $ map (\f -> T.unpack $ f reg) fields_csv



       
