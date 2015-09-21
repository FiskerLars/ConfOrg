module CmdLineInterpreter (defCmd, cmdName, cmdGenFun, cmdFileOut, cmdNoFileOut, cmdAggregate, getCmd) where

import RegistrationParser (Registry)

import qualified Data.Text as T
import Data.List (find, isPrefixOf)
import Data.Maybe (listToMaybe, fromMaybe)


data Command = Command String (Registry -> IO T.Text) (T.Text -> String -> IO T.Text) ([T.Text] -> T.Text) 

cmdName::Command -> String
cmdName (Command s _ _ _) = s

cmdGenFun:: Command -> (Registry -> IO T.Text)
cmdGenFun (Command _ f _ _) = f

cmdFileOut:: Command -> (T.Text -> String -> IO T.Text)
cmdFileOut (Command _ _ f _) = f

cmdNoFileOut:: T.Text -> String -> IO T.Text
cmdNoFileOut t _ = return t

cmdAggregate:: Command -> ([T.Text] -> T.Text)
cmdAggregate (Command _ _ _ f) = f

defCmd:: String -> (Registry -> IO T.Text) -> (T.Text -> String -> IO T.Text) -> ([T.Text] -> T.Text) -> Command
defCmd = Command

getCmd:: [Command] -> String -> Command
getCmd cmds str =  (fromMaybe undefined) $ first_prefix_match cmds $ str

first_prefix_match:: [Command] -> String -> Maybe Command
first_prefix_match cmds init = let
  in find (\cmd -> isPrefixOf init $ cmdName cmd) cmds 



