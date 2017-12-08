import Text.Read
import Data.List

---------------------Data

data TODO_item = TODO_item {description :: String}
instance Eq TODO_item where
    (TODO_item a) == (TODO_item b) = a == b
instance Ord TODO_item where
    compare (TODO_item a) (TODO_item b) = compare a b

type TODO_List = [TODO_item]
{-
data Command = Command{
                 commandName    :: String
                ,commandAction  :: TODO_List -> String -> TODO_List
               }
-}
---------------------const strings
--cEXIT_COMMAND = "exit"
cNO_COMMAND_EXIST = "No command exis"
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
                ,"*sort* todo list in alphabeic order through quicksort"
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
                command <- getLine
                execute command todo_list

---------------------IO cDEFAULT_COMMANDS_LIST
printPrompt :: String -> IO ()
printPrompt promptString = putStr promptString

printMessage :: String -> IO ()
printMessage msg = putStrLn msg

printTODOItem :: (Int, TODO_item) -> IO ()
printTODOItem (id, item) = putStrLn ("Id: " ++ (show id) ++ "; Content: " ++ (description item))

printTODOList :: TODO_List -> IO ()
printTODOList todo_list = mapM_ printTODOItem (zip [0..] todo_list)

saveToFile :: FilePath -> TODO_List -> IO ()
saveToFile path todo_list = do 
                             writeFile path (unlines (todo_listToFString todo_list))
                             todoShell todo_list

readFromFile :: FilePath -> IO ()
readFromFile path = do
                     content <- readFile cDEFAULT_FILEPATH
                     todoShell (fStringToTodo_list content)

--------------------------execute
{-
execute :: String -> [Command] -> TODO_List -> IO()
execute _ [] todoList = do 
                        printMessage cNO_COMMAND_EXIST
                        todoShell todoList
execute cmdString (commandItem:xs) todoList = do
                                         if (hasCommand (commandName commandItem) cmdString)
                                         then  do 
                                             let newTODOList = (commandAction commandItem) todoList (extractArgs cmdString (commandName commandItem)) 
                                             printTODOList newTODOList
                                             todoShell newTODOList
                                         else execute cmdString xs todoList
execute cEXIT_COMMAND _ _ = return ()


hasCommand :: String -> String -> Bool
hasCommand [] [] = True
hasCommand [] _ = True;
hasCommand  _ [] = False;
hasCommand (cmdLetter:xs) (textLetter:ys) = if cmdLetter == textLetter
                                            then hasCommand xs ys
                                            else False
extractArgs :: String -> String -> String
extractArgs [] [] = []
extractArgs args [] = args
extractArgs (command:ys) (defaultCmd:xs) = extractArgs ys xs
-}

execute :: String -> TODO_List -> IO ()
execute ('a':'d':'d':' ':todo_item) todo_list = do
                            let updated_todo_list = (todo_list ++ [(createTODO_item todo_item)])
                            execute "show" updated_todo_list
execute ('r':'e':'m':'o':'v':'e':' ':idS) todo_list = 
                                 case fmap (remove todo_list) (readMaybe idS :: Maybe Int) of
                                    Nothing -> do
                                            printMessage cINCORRECT_ID
                                            todoShell todo_list
                                    Just (Just updated_todo_list) -> execute "show" updated_todo_list
                                    Just Nothing -> do
                                                 printMessage cNO_ID_EXIST
                                                 todoShell todo_list
execute ('h':'e':'l':'p':' ':command_name) todo_list = do
                           printMessage (getHelp command_name cDEFAULT_COMMANDS_NAME_HELP_LIST)
                           todoShell todo_list
execute "save" todo_list = saveToFile cDEFAULT_FILEPATH todo_list
execute "load" _ = readFromFile cDEFAULT_FILEPATH
execute "sort" todo_list = do 
                            let sortedList = quicksort todo_list
                            execute "show" sortedList
execute "show" todo_list = do
                            printMessage "TODO_List :"
                            printTODOList todo_list
                            todoShell todo_list
execute "" todo_list = todoShell todo_list
execute "exit" _ = return ()
execute _ todo_list = do
                       printMessage cINVALID_COMMAND
                       todoShell todo_list
------------------------------Commands
{-
commandList = [Command "add " addCommand
              ,Command "remove " removeCommand
              ]
              
addCommand :: TODO_List -> String -> TODO_List
addCommand todoList item = (todoList ++ [(TODO_item {description = item})])

removeCommand :: TODO_List -> String -> TODO_List
removeCommand todo_list args = 
                       case fmap (remove todo_list) (readMaybe args :: Maybe Int) of
                            Nothing -> todo_list
                            Just (Just updated_todo_list) -> updated_todo_list
                            Just Nothing -> todo_list
-}              

--------------------quick help (from files)
getHelp :: String -> [(String,String)] -> String
getHelp command_name ((commandName,commandHelp):xs) = 
                                                if command_name == commandName
                                                then commandHelp
                                                else getHelp command_name xs
getHelp _ [] = cNO_COMMAND_EXIST

--------------------pure functions

remove :: [a] -> Int -> Maybe [a]
remove (_:xs) 0 = Just xs
remove (x:xs) n = do
                _xs <- remove xs (n-1)
                return (x:_xs)
remove [] _ = Nothing

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
    where
        lesser  = filter (< x) xs
        greater = filter (>= x) xs

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