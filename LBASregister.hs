
import RegistrationParser

import Data.Yaml
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8, decodeLatin1)
import qualified Data.ByteString as BS
import Numeric (showHex)
import System.IO
import System.Environment (getArgs)
import Data.List 
import Data.Maybe (listToMaybe, fromMaybe)
import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString as BS (pack)
import Debug.Hood.Observe

data Command = Command String (Registry -> IO T.Text)

cmdName (Command s _ ) = s
cmdFun (Command _ f ) = f

commands:: [Command]
commands = [ Command "email" (return.email)
           , Command "csv" registry_to_csv
           , Command "rechnung" (registry_apply_to_rechnung rechnung_template)]


rechnung_template = getArgs >>= readFile.(!! 1)

getCmd:: [Command] -> String -> Command
getCmd cmds str =  (fromMaybe undefined) $ first_prefix_match cmds str

first_prefix_match:: [Command] -> String -> Maybe Command
first_prefix_match cmds init = let
  in find (\cmd -> isPrefixOf init $ cmdName cmd) cmds 



main:: IO ()
main = runO $
       do
         cmd <- getArgs >>= return.(getCmd commands).head
         str <- getContents >>= (return.(T.pack))
         case parse_registration str of
          Right x -> hPutStrLn stdout.(T.unpack) =<< (cmdFun cmd) (x ::Registry)
          Left err -> hPutStrLn stderr $ (prettyPrintParseException err)
                      ++ "\n" ++ (T.unpack str)
            


registry_to_csv:: Registry -> IO T.Text
registry_to_csv reg = let
  fields_csv = [anrede
               , (fromMaybe (T.pack "")).titel, vorname, nachname, affiliation, street, (T.pack).show.postcode, city, email] 
  in
   return $ T.intercalate (T.pack "; ") $ map (\f -> f reg) fields_csv


registry_to_emails:: Registry -> IO T.Text 
registry_to_emails = return.email


registry_apply_to_rechnung:: IO String -> Registry -> IO T.Text
registry_apply_to_rechnung template r = let
  anredeTag = T.pack "<<anrede>>"
  affiliationTag = T.pack "<<postal>>"
  rechNrTag = T.pack "<<nr>>"
  affiliationLatexText r = T.intercalate (T.pack "\\\\") [ nameline r
                                                       , affiliation r
                                                       , street r 
                                                       , cityline r]
  anredeText r = T.intercalate (T.pack " ") [anrede r, nameline r]
  nameline r = (fromMaybe (T.pack "") (titel r))
               `T.append` (T.intercalate (T.pack " ") [ vorname r
                                                              , nachname r])
  cityline r = T.intercalate (T.pack " ") [ (T.pack "D-") `T.append` (((T.pack).show.postcode) r)
                                          ,  city r]
  rechNrText r =  genHashText
                  $ T.concat [ vorname r, nachname r
                             , (T.pack "LBAS15")
                             , affiliation r]
  genHashText = T.pack . take 7 . concat . map (flip showHex "") . BS.unpack . hash . encodeUtf8
  in template >>= return
     .(T.replace anredeTag (anredeText r))
     .(T.replace affiliationTag (affiliationLatexText r))
     .(T.replace rechNrTag (rechNrText r))
     .(T.pack)

open_db:: String -> IO [Registry]
open_db db_file = undefined



  
