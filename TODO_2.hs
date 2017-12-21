import Text.Read
import Data.List

---------------------Data

data TODO_item = TODO_item {description :: String}
instance Eq TODO_item where
    (TODO_item a) == (TODO_item b) = a == b
instance Ord TODO_item where
    compare (TODO_item a) (TODO_item b) = compare a b

type TODO_List = [TODO_item]

data Command = PureCommand{
                 commandName    :: String
                ,commandAction  :: TODO_List -> String -> TODO_List
               }
               |IoCommand{
                commandName    :: String
               ,commandIOAction  :: TODO_List -> String -> IO ()
               }
               
isPureCommand (PureCommand _ _) = True
isPureCommand _                 = False

---------------------const strings

cNO_COMMAND_EXIST = "No command exist"
cINVALID_COMMAND = "Invalid command"
cINCORRECT_ID = "Incorrect ID"
cNO_ID_EXIST = "No item with that ID exist"
cDEFAULT_FILEPATH = "TODO_SAVE.txt"
cDEFAULT_PROMPT = "> "
cDEFAULT_COMMANDS_LIST = ["load","add","remove","show","save","sort","help"]
cDEFAULT_COMMANDS_HELP_LIST =[
                "*load* todo list from file which you can save, throug *save* command"
                ,"*add [item description]*. Put todo item into the todo list"
                ,"*remove [item id]*. Put away iem from your todo list"
                ,"*show* Display current too list"
                ,"*save* Save current todo list into the file in "
                ,"*sort* [-d] todo list in alphabeic order in ascending order by default or descending order"
                ,"*help [command name]*. Show help."]
cDEFAULT_COMMANDS_NAME_HELP_LIST = zip cDEFAULT_COMMANDS_LIST cDEFAULT_COMMANDS_HELP_LIST
cINTRO_MSG = "  Default commands:\n " ++ (intercalate "; " cDEFAULT_COMMANDS_LIST) ++"\nFor quick help, use command > help [command name]"

---------------------main entry point

main :: IO()
main = do
      printMessage cINTRO_MSG      
      todoShell []

---------------------main todo shell

todoShell :: TODO_List -> IO()
todoShell todo_list = do
                printPrompt cDEFAULT_PROMPT
                commandLine <- getLine
                execute (extractCommandName commandLine []) (extractArgs commandLine) commandList  todo_list

--------------------------------Syntax parse

extractCommandName :: String -> String-> String
extractCommandName (x:line) command = if(x == ' ')
                                  then command
                                  else extractCommandName line (command ++ [x])
extractCommandName [] command  = command

extractArgs :: String -> String
extractArgs (x:xs) = if(x==' ')then xs
                     else extractArgs xs
extractArgs [] = []

--------------------------execute

execute :: String -> String -> [Command] -> TODO_List -> IO()
execute [] _ _ todoList = todoShell todoList
execute cmdString args (commandItem:xs) todoList = do
                                         if ((commandName commandItem) == cmdString)
                                         then  do 
                                            if (isPureCommand commandItem)
                                            then do
                                                let newTODOList = (commandAction commandItem) todoList args
                                                todoShell newTODOList
                                            else do 
                                                (commandIOAction commandItem) todoList args
                                         else execute cmdString args xs todoList
execute cmdString args [] todoList = do
                        printMessage cNO_COMMAND_EXIST
                        todoShell todoList

------------------------------Commands
commandList = [
              PureCommand "add" addCommand
              ,PureCommand "remove" removeCommand
              ,IoCommand "show" showCommand
              ,PureCommand "sort" sortCommand
              ,IoCommand "save" saveCommand
              ,IoCommand "load" loadCommand
              ,IoCommand "exit" exitCommand]
              
----Pure commands
              
addCommand :: TODO_List -> String -> TODO_List
addCommand todoList [] = todoList
addCommand todoList item = (todoList ++ [(TODO_item {description = item})])

removeCommand :: TODO_List -> String -> TODO_List
removeCommand todo_list args = 
                       case fmap (remove todo_list) (readMaybe args :: Maybe Int) of
                            Nothing -> todo_list
                            Just (Just updated_todo_list) -> updated_todo_list
                            Just Nothing -> todo_list
{-                
removeCommand :: TODO_List -> String -> IO ()
removeCommand todo_list args = if isNum possibleId
                               then do case (remove todo_list (toNum possibleId))
                                       (Just newTODOList) -> todoShell newTODOList
                                        Nothing -> do
                                                printMessage cNO_ID_EXIST
                                                todoShell todoList (remove todo_list)
                               else do
                                   printMessage cINCORRECT_ID
                                   todoShell todoList (remove todo_list)
                             where 
                                possibleId = (readMaybe args :: Maybe Int)
-}
isNum :: (Maybe Int) -> Bool
isNum (Just _) = True
isNum Nothing = False

toNum :: (Maybe Int) -> Int
toNum (Just id) = id
toNum Nothing = -1

sortCommand :: TODO_List -> String -> TODO_List
sortCommand todoList [] = sort todoList
sortCommand todoList "-d" = reverse (sort todoList)
sortCommand todoList _ = todoList

----IO Commands, after execution they return control to todoShell

showCommand :: TODO_List -> String -> IO ()
showCommand todoList _ = do 
                        printTODOList todoList
                        todoShell todoList

exitCommand :: TODO_List -> String -> IO ()
exitCommand _ _ = return ()

saveCommand ::  TODO_List -> String -> IO ()
saveCommand todoList _ = do saveToFile cDEFAULT_FILEPATH todoList
                            todoShell todoList

loadCommand :: TODO_List -> String -> IO ()
loadCommand _ _ = do 
                    content <- readFile cDEFAULT_FILEPATH
                    todoShell (fStringToTodo_list content)

---------------------IO functions

printPrompt :: String -> IO ()
printPrompt promptString = putStr promptString

printMessage :: String -> IO ()
printMessage msg = putStrLn msg

printTODOItem :: (Int, TODO_item) -> IO ()
printTODOItem (id, item) = putStrLn ("Id: " ++ (show id) ++ "; Content: " ++ (description item))

printTODOList :: TODO_List -> IO ()
printTODOList todo_list = mapM_ printTODOItem (zip [0..] todo_list)

saveToFile :: FilePath -> TODO_List -> IO ()
saveToFile path todo_list = do writeFile path (unlines (todo_listToFString todo_list))

--------------------pure functions

remove :: [a] -> Int -> Maybe [a]
remove (_:xs) 0 = Just xs
remove (x:xs) n = do
                _xs <- remove xs (n-1)
                return (x:_xs)
remove [] _ = Nothing

todo_listToFString :: TODO_List -> [String]
todo_listToFString [] = []
todo_listToFString (x:xs) = (description x) : (todo_listToFString xs)

fStringToTodo_list :: String -> TODO_List
fStringToTodo_list str = map createTODO_item (wordsWhen ('\n'==) str)

createTODO_item :: String -> TODO_item
createTODO_item args = TODO_item {description = args}

-- Cut String to [String] if p at char returns true. Found in the Net.

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'
