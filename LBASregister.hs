
import RegistrationParser
import CmdLineInterpreter

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

import System.Directory (getDirectoryContents)
import System.FilePath (combine)



basedirpath = "/home/lars/projects/lbas15/lbas15orga"
participants_dirpath = combine basedirpath "teilnehmer"
invoice_dirpath = combine basedirpath "rechnungen"

getDirectoryFiles:: FilePath -> IO [FilePath]
getDirectoryFiles dirPath = return.filter (\f -> not $ any (== f) [".", ".."]) =<< getDirectoryContents dirPath


participant_filenames:: IO [FilePath]
participant_filenames = getDirectoryFiles  participants_dirpath

participants_filepaths:: IO [FilePath]
participants_filepaths =  participant_filenames >>= return.map (combine participants_dirpath)

invoice_filepaths:: IO [FilePath]
invoice_filepaths = participant_filenames >>= return.map ((++ ".pdf").combine invoice_dirpath)




-- commands:: [Command]
commands = [ defCmd "email"
             (return.email)
             (cmdNoFileOut)
             intercalate_komma
           , defCmd "csv"
             registry_to_csv
             (cmdNoFileOut)
             intercalate_nl
           , defCmd "rechnung"
             (registry_apply_to_rechnung rechnung_template)
             (\t u -> (withFile (combine invoice_dirpath (u ++ ".tex")) WriteMode ((flip hPutStrLn) (T.unpack t))
                      >> return t
                      )
             )
             (literal_aggregate "rechnungen erstellt")
           , defCmd "nametag"
             (nameTagLatexText)
             (cmdNoFileOut)
             intercalate_nl
           ] 
           where
             intercalate_komma = T.intercalate (T.pack ",")
             intercalate_nl    = T.intercalate (T.pack "\n")
             literal_aggregate l = (\_ -> T.pack l)


rechnung_template:: IO String
rechnung_template = getArgs >>= (readFile.secondArg)
                   where
                     secondArg:: [String] -> String
                     secondArg cs = case listToMaybe $ tail cs of
                                     Just f  -> f
                                     Nothing -> error "No second argument (invoice template) given"



main:: IO ()
main = runO $
       do
         cmd <- getArgs >>= return.(getCmd commands).head
         participant_filenames
           >>= traverse (\u -> genResults (cmdGenFun cmd) u
                               >>= (flip (cmdFileOut cmd) u)) 
           >>= aggregatedOutput (cmdAggregate cmd) stdout
       where
         genResults:: (Registry -> IO T.Text)  -> FilePath -> IO T.Text
         genResults fun user = readRegistryFile (combine participants_dirpath user) >>= fun
         aggregatedOutput:: ([T.Text] -> T.Text) -> Handle -> [T.Text] -> IO ()
         aggregatedOutput fun h =  (hPutStrLn h).(T.unpack).fun

  
         
readRegistryFile:: FilePath -> IO Registry 
readRegistryFile f =  readFile (observe "reading file: " f)
                      >>= (\str -> case parse_registration $ (T.pack) str of
                            Right x -> return (x ::Registry)
                            Left err -> error $ (prettyPrintParseException err)
                                        ++ "\n" ++ str
                          )


                      


registry_to_csv:: Registry -> IO T.Text
registry_to_csv reg = let
  fields_csv = [anrede
               , (fromMaybe (T.pack "")).titel, vorname, nachname
               , (fromMaybe (T.pack "")).affiliation, street, (T.pack).show.postcode, city, email] 
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
                                                         , (fromMaybe (T.pack "\\ ")) $ affiliation r
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
                             , (T.pack "LBAS15Teilnahmegebühr")
                             , fromMaybe (T.pack "") $ affiliation r]
  genHashText = T.pack . take 7 . concat . map (flip showHex "") . BS.unpack . hash . encodeUtf8
  in template >>= return
     .(T.replace anredeTag (anredeText r))
     .(T.replace affiliationTag (affiliationLatexText r))
     .(T.replace rechNrTag (rechNrText r))
     .(T.pack)


nameText r = T.intercalate (T.pack " ") [ fromMaybe (T.pack "") $ titel r
                                        , vorname r, nachname r ]

nameTagLatexText:: Registry -> IO T.Text
nameTagLatexText r = return
                     $ T.replace (T.pack "<<name>>") (nameText r)
                     $ T.replace (T.pack "<<affiliation>>") (fromMaybe (T.pack "") $ affiliation r)
                     $ T.pack "\\confpin{<<name>>}{<<affiliation>>}"

open_db:: String -> IO [Registry]
open_db db_file = undefined



  
