import Data.List.Split (splitOn)
import Data.Char (isSpace, toUpper)
import Debug.Trace (trace)
import Text.Read (readMaybe)

type TableName = String
type ColumnName = String
type Value = String -- vsechny hodnoty pro jednoduchost typu string
type ColumnList = [ColumnName] -- sloupce v tabulce
type Row = [Value] -- hodnoty v jednom radku
type Database = [(TableName, (ColumnList, [Row]))] -- databaze obsahuje tabulky a radky

data SQLStatement
    = CreateTable TableName [ColumnName]
    | Insert TableName [Value]
    | Select [ColumnName] TableName (Maybe [Condition]) (Maybe Join)
    | Quit
data Condition
    = Equals ColumnName Value
    | NotEquals ColumnName Value
    | GreaterThan ColumnName Value
    | LessThan ColumnName Value
    | GreaterThanOrEqual ColumnName Value
    | LessThanOrEqual ColumnName Value
    deriving Show

data Join
    = InnerJoin TableName TableName ColumnName ColumnName -- join na zaklade stejnych sloupcu

-- pomocna funkce na orezani mezer kolem stringu
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

createTable :: Database -> TableName -> [ColumnName] -> Database
createTable db name columns = (name, (columns, [])) : db

insertInto :: Database -> TableName -> [Value] -> Database
insertInto db tableName values = map updateTable db
  where
    updateTable (name, (columns, rows))
      | name == tableName = (name, (columns, values : rows))
      | otherwise = (name, (columns, rows))

select :: Database -> [ColumnName] -> TableName -> Maybe [Condition] -> Maybe Join -> [[Value]]
select db columns tableName conditions Nothing =
    case lookup tableName db of
        Nothing -> error $ "Table " ++ tableName ++ " not found."
        Just (tableCols, rows) ->
            let conditionRows = maybe rows (\conds -> applyConditions tableCols conds rows) conditions
                filteredRows = map (filterColumns columns tableCols) conditionRows
            in filteredRows
    where
        filterRows :: [ColumnName] -> [ColumnName] -> [[Value]] -> [[Value]]
        filterRows _ _ [] = []
        filterRows columns tableColumns (r:rs) = filteredRow : filterRows columns tableColumns rs
            where filteredRow = filterColumns columns tableColumns r
        
        filterColumns :: [ColumnName] -> [ColumnName] -> [Value] -> [Value]
        filterColumns _ [] _ = []
        filterColumns _ _ [] = []
        filterColumns columns (column : tableColumns) (value:rowValues)
            | column `elem` columns = value : filterColumns columns tableColumns rowValues -- pokus je sloupec z tabulky pozadovany v SELECT, pripoj jeho hodnotu do vyfiltrovaného radku
            | otherwise = filterColumns columns tableColumns rowValues -- jinak hledej pozadovane sloupecky dal

applyConditions :: [ColumnName] -> [Condition] -> [[Value]] -> [[Value]]
applyConditions cols conditions rows = filter (meetsAllConditions conditions cols) rows

meetsAllConditions :: [Condition] -> [ColumnName] -> [Value] -> Bool
meetsAllConditions conditions cols row = all (\condition -> meetsCondition condition cols row) conditions

meetsCondition :: Condition -> [ColumnName] -> [Value] -> Bool
meetsCondition cond cols row = case lookupValue cols row (conditionColumn cond) of
    Just val -> case cond of
        Equals _ val2 -> val == val2
        NotEquals _ val2 -> val /= val2
        GreaterThan _ val2 -> compareMaybeNums val val2 (>)
        LessThan _ val2 -> compareMaybeNums val val2 (<)
        GreaterThanOrEqual _ val2 -> compareMaybeNums val val2 (>=)
        LessThanOrEqual _ val2 -> compareMaybeNums val val2 (<=)
    Nothing -> False

compareMaybeNums :: String -> String -> (Double -> Double -> Bool) -> Bool
compareMaybeNums v1 v2 op = case (readMaybe v1 :: Maybe Double, readMaybe v2 :: Maybe Double) of
    (Just num1, Just num2) -> num1 `op` num2
    _ -> False

lookupValue :: [ColumnName] -> [Value] -> ColumnName -> Maybe Value
lookupValue cols row col = case lookup col (zip cols row) of
    Just val -> Just val
    Nothing -> Nothing

conditionColumn :: Condition -> ColumnName
conditionColumn (Equals col _) = col
conditionColumn (NotEquals col _) = col
conditionColumn (GreaterThan col _) = col
conditionColumn (LessThan col _) = col
conditionColumn (GreaterThanOrEqual col _) = col
conditionColumn (LessThanOrEqual col _) = col

printResult :: [[Value]] -> IO ()
printResult [] = putStrLn "No results found."
printResult rows = mapM_ (putStrLn . unwords) rows

extractParenthesizedContent :: String -> String
extractParenthesizedContent input
    | null maybeOpenParen = error $ "No opening parenthesis found in input: " ++ input
    | null maybeCloseParen = error $ "No closing parenthesis found after opening in input: " ++ input
    | otherwise = maybeCloseParen
    where
        maybeOpenParen = dropWhile (/= '(') input
        maybeCloseParen = takeWhile (/= ')') (tail maybeOpenParen)

parseColumns :: String -> [ColumnName]
parseColumns input = splitOn "," (trim input)

parseValues :: String -> [Value]
parseValues input = splitOn "," (trim input)

extractStringValue :: String -> String
extractStringValue s = 
    if head s == '\'' && last s == '\'' 
    then tail (init s)  -- Remove the first and last character (the quotes)
    else s

parseConditions :: [String] -> [Condition]
parseConditions input =
    case input of
        "WHERE":field:op:rest -> 
            let value = extractStringValue $ unwords rest
            in [case op of
                "=" -> Equals field value
                "!=" -> NotEquals field value
                ">" -> GreaterThan field value
                "<" -> LessThan field value
                ">=" -> GreaterThanOrEqual field value
                "<=" -> LessThanOrEqual field value
                _ -> error "Unsupported operator"]
        _ -> []

parseSQL :: String -> SQLStatement
parseSQL input =
    let w = words $ map toUpper input
    in case w of
            ("QUIT":_) -> Quit
            ("CREATE":"TABLE":tableName:rest) ->
                let columns = parseColumns $ extractParenthesizedContent (unwords rest)
                in CreateTable tableName columns
            ("INSERT":"INTO":tableName:rest) ->
                let vals = parseValues $ extractParenthesizedContent (unwords rest)
                in Insert tableName vals
            "SELECT":cols:"FROM":tableName:rest ->
                let columns = parseColumns $ extractParenthesizedContent cols
                    conditions = parseConditions rest
                in Select columns tableName (Just conditions) Nothing
            _ -> error $ "Unknown command: " ++ input

interpretSQL :: Database -> SQLStatement -> IO Database
interpretSQL db (CreateTable name columns) = do
    putStrLn "Table created."
    return $ createTable db name columns
interpretSQL db (Insert tableName values) = do
    putStrLn "Data inserted."
    return $ insertInto db tableName values
interpretSQL db (Select columns tableName conditions join) = do
    let result = select db columns tableName conditions join
    printResult result
    return db
interpretSQL db Quit = do
    putStrLn "Exiting..."
    return db

main :: IO ()
main = do
    putStrLn "Welcome to the Haskell SQL interpreter!"
    runInterpreter [] -- zacneme s prazdnou databazi
  where
    runInterpreter db = do
        line <- getLine
        case parseSQL line of
            Quit -> return ()
            stmt -> do
                db' <- interpretSQL db stmt -- updatujeme databazi
                runInterpreter db' -- pokracujeme dalsi iteraci s novou databazi

-- zatim pouze hruby navrh joinu...
parseJoin :: [String] -> Maybe Join
parseJoin ("JOIN":table2:"ON":col1:"=":col2:_) = Just (InnerJoin table2 table2 col1 col2)
parseJoin _ = Nothing

{-
main
CREATE TABLE Students (ID,Name)
INSERT INTO Students (1,Alice)
INSERT INTO Students (2,Bobek)
SELECT (ID,Name) FROM Students
(join zatim nemam...) SELECT (ID,Name) FROM Students JOIN School ON Students.ID = School.StudentID

CREATE TABLE StudentsWithAge (ID,Name,Age)
INSERT INTO StudentsWithAge (1,Alice,22)
INSERT INTO StudentsWithAge (2,Bob,19)
INSERT INTO StudentsWithAge (3,Charlie,25)
SELECT (ID,Name) FROM StudentsWithAge WHERE Age > 20
SELECT (ID) FROM StudentsWithAge WHERE Name = 'Alice'
-}