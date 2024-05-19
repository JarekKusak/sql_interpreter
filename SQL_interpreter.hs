import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.Char (isSpace, toUpper)

type TableName = String
type ColumnName = String
type Value = String -- vsechny hodnoty pro jednoduchost typu string
type ColumnList = [ColumnName] -- sloupce v tabulce
type Row = [Value] -- hodnoty v jednom radku

-- type Database = [("Students", (["ID", "Name"], [["1", "Alice"], ["2", "Bob"]]))]
type Database = [(TableName, (ColumnList, [Row]))] -- databaze obsahuje tabulky a radky

data SQLStatement
    = CreateTable TableName [ColumnName]
    | Insert TableName [Value]
    | Select [ColumnName] TableName (Maybe Join)
    | Quit -- ukonceni interpretu

data Join
    = InnerJoin TableName TableName ColumnName ColumnName -- join na zaklade stejnych sloupcu

createTable :: Database -> TableName -> [ColumnName] -> Database
createTable db name columns = trace ("Creating table " ++ name ++ " with columns " ++ show columns) $ (name, (columns, [])) : db -- pripoji se k existujicim tabulkam

insertInto :: Database -> TableName -> [Value] -> Database
insertInto db tableName values = trace ("Inserting into " ++ tableName ++ " values " ++ show values) $ map updateTable db -- map vyuzivam v podstate k iteraci
  where
    updateTable (name, (columns, rows))
      | name == tableName = (name, (columns, values : rows)) -- prida novy radek (zaznam) do pozadovane tabulky
      | otherwise = (name, (columns, rows)) -- ostatni tabulky nech nezmenene

select :: Database -> [ColumnName] -> TableName -> Maybe Join -> [[Value]]
select db columns tableName Nothing =
    trace ("Selecting columns " ++ show columns ++ " from table " ++ tableName) $
    case lookup tableName db of -- vyhledame pozadovanou tabulku v databazi
        Nothing -> error $ "Table " ++ tableName ++ " not found." -- neexistuje
        Just (tableCols, rows) ->
            let filteredRows = map (filterColumns columns tableCols) rows
            in trace ("Filtered rows: " ++ show filteredRows) filteredRows
  where
    filterColumns cols tableCols row = [value | (col, value) <- zip tableCols row, col `elem` cols]

printResult :: [[Value]] -> IO ()
printResult [] = putStrLn "No results found."
printResult rows = mapM_ (putStrLn . unwords) rows

extractParenthesizedContent :: String -> String
extractParenthesizedContent input =
    let maybeOpenParen = dropWhile (/= '(') input
    in if null maybeOpenParen
       then error $ "No opening parenthesis found in input: " ++ input
       else
           let maybeCloseParen = takeWhile (/= ')') (tail maybeOpenParen)
           in if null maybeCloseParen
              then error $ "No closing parenthesis found after opening in input: " ++ input
              else maybeCloseParen

parseSQL :: String -> SQLStatement
parseSQL input = 
    let w = words $ map toUpper input
    in case w of
        ("QUIT":_) ->
            Quit
        ("CREATE":"TABLE":tableName:rest) ->
            let columns = parseColumns $ extractParenthesizedContent (unwords rest)
            in CreateTable tableName columns
        ("INSERT":"INTO":tableName:rest) ->
            let vals = parseValues $ extractParenthesizedContent (unwords rest)
            in Insert tableName vals
        ("SELECT":cols:"FROM":tableName:rest) ->
            let columns = parseColumns $ extractParenthesizedContent cols
            in Select columns tableName (parseJoin rest)
        _ -> error $ "Unknown command: " ++ input

-- Pomocná funkce pro parsování sloupců
parseColumns :: String -> [ColumnName]
parseColumns input = splitOn "," (trim input)  -- odstrani zavorky a rozdeli podle carek

-- Pomocná funkce pro parsování hodnot
parseValues :: String -> [Value]
parseValues input = splitOn "," (trim input)  -- odstrani zavorky a rozdeli podle carek

parseJoin :: [String] -> Maybe Join
parseJoin ("JOIN":table2:"ON":col1:"=":col2:_) = Just (InnerJoin table2 table2 col1 col2)
parseJoin _ = Nothing

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

interpretSQL :: Database -> SQLStatement -> IO Database
interpretSQL db (CreateTable name columns) = do
    putStrLn "Table created."
    return $ createTable db name columns
interpretSQL db (Insert tableName values) = do
    putStrLn "Data inserted."
    return $ insertInto db tableName values
interpretSQL db (Select columns tableName Nothing) = do
    let result = select db columns tableName Nothing
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
            Quit -> return () -- pokud Quit, ukonci aplikaci
            stmt -> do -- jinak:
                db' <- interpretSQL db stmt -- updatni databazi
                runInterpreter db'

{-
CREATE TABLE Students (ID,Name)
INSERT INTO Students (1,Alice)
SELECT (ID,Name) FROM Students
SELECT (ID,Name) FROM Students JOIN School ON Students.ID = School.StudentID
-}

{-
-- Funkce pro vyhledání indexu sloupce v tabulce
findColumnIndex :: [ColumnName] -> ColumnName -> Maybe Int
findColumnIndex columns name = lookup name (zip columns [0..])

-- Funkce pro provedení INNER JOIN
innerJoin :: Database -> TableName -> TableName -> ColumnName -> ColumnName -> [Row]
innerJoin db table1Name table2Name col1Name col2Name =
    case (lookup table1Name db, lookup table2Name db) of
        (Just (cols1, rows1), Just (cols2, rows2)) ->
            case (findColumnIndex cols1 col1Name, findColumnIndex cols2 col2Name) of
                (Just col1Index, Just col2Index) -> concatMap (matchRows col1Index col2Index rows2) rows1
                _ -> []
        _ -> []

    where
        -- Match a row from the first table with all rows from the second table
        matchRows :: Int -> Int -> [Row] -> Row -> [Row]
        matchRows col1Index col2Index rows2 row1 =
            [ row1 ++ row2 | row2 <- rows2, row1 !! col1Index == row2 !! col2Index ]

-- Výše uvedený kód předpokládá, že obě tabulky a sloupce pro join existují. Funkce `matchRows` zpracovává každý řádek z první tabulky a hledá odpovídající řádky ve druhé tabulce.
-- Když nalezne shodu, spojí řádky z obou tabulek do jednoho řádku, který je přidán do výsledného seznamu.
-}