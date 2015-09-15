
import RegistrationParser

import Data.Yaml
import qualified Data.Text as T
import System.IO
import System.Environment (getArgs)
import Data.List 
import Data.Maybe (listToMaybe, fromMaybe)

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
  affiliationTag = T.pack "<<postal>>"
  affiliationLatexText r = T.intercalate (T.pack "\\\\") [ nameline r
                                                       , affiliation r
                                                       , street r
                                                       , T.pack ""
                                                       , cityline r]
    where nameline r = (fromMaybe (T.pack "") (titel r))
                       `T.append` (T.intercalate (T.pack " ") [ vorname r
                                                              , nachname r])
          cityline r = T.intercalate (T.pack " ") [((T.pack).show.postcode) r, city r]
  in template >>= return.(T.replace affiliationTag (affiliationLatexText r)).(T.pack)

open_db:: String -> IO [Registry]
open_db db_file = undefined



  
