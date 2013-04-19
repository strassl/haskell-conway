import Conway
import Control.Monad

main = do
    text <- readFile "field.txt"
    let f = parseField text
    putStr $ printField f
    putStr "\n"

    runGame f


runGame f = do
    l <- getLine
    let f2 = nextState f
    putStr $ printField f2
    putStr "\n"
    runGame f2
        

parseField :: String -> Field
parseField s = foldl (\acc r -> acc ++ [(parseRow (length acc) r)]) [] rows
    where rows = lines s

parseRow :: Int -> String -> [Cell]
parseRow iy str = foldl (\acc c -> acc ++ [(parseChar (length acc) iy c)]) [] str

parseChar :: Int -> Int -> Char -> Cell
parseChar ix iy chr = Cell { alive = (if chr == '1' then True else False), x = ix, y = iy }

printField :: Field -> String
printField f = unlines $ map show f
