{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib2
  ( Query(..),
    Parser(..),
    parseString,
    query,
    BookInfo(..),
    BookGenre(..),
    State(..),
    emptyState,
    stateTransition,
    parseQuery,
    parseAddBookQuery,
    parseRemoveBookQuery,
    parseBookInfo,
    parseBookGenre,
  ) where
    
import Control.Applicative (Alternative (empty), (<|>), many, optional)
import qualified Data.Char as C

data Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f functor = Parser $ \input ->
        case runParser functor input of
            Left e -> Left e
            Right (v, r) -> Right (f v, r)

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \input -> Right (a, input)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    ff <*> fa = Parser $ \input ->
        case runParser ff input of
            Left e1 -> Left e1
            Right (f, r1) -> case runParser fa r1 of
                                Left e2 -> Left e2
                                Right (a, r2) -> Right (f a , r2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \input -> Left $ "Could not parse " ++ input
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \inp ->
        case (runParser p1 inp) of
            Right r1 -> Right r1
            Left e1 -> case (runParser p2 inp) of
                            Right r2 -> Right r2
                            Left e2 -> Left e2

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) -> case runParser (mf a) r1 of
                                Left e2 -> Left e2
                                Right (b, r2) -> Right (b, r2)

-- Basic Parsers

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseChar :: Char -> Parser Char
parseChar c = Parser $ \s -> 
  case s of
    [] -> Left "No match"
    (h:t) -> if c == h 
             then Right (c, t) 
             else Left "No match"

parseLetter :: Parser Char
parseLetter = Parser $ \s -> case s of
  []       -> Left "No match"
  (h : t)  -> if C.isLetter h
                 then Right (h, t)
                 else Left "No match"

parseDigit :: Parser Char
parseDigit = Parser $ \s -> case s of
  []       -> Left "No match"
  (h : t)  -> if C.isDigit h
                 then Right (h, t)
                 else Left "No match"

parseString :: String -> Parser String
parseString [] = return []
parseString (c:cs) = do
    _ <- parseChar c
    rest <- parseString cs
    return (c : rest)

many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest  <- many p
  return (first : rest)  

-- Data Types
data Query
    =  AddBookQuery BookInfo
    | RemoveBookQuery BookInfo
    deriving (Eq, Show)

data BookInfo = BookInfo Title Author BookGenre 
    deriving (Eq, Show)

type Title = String
type Author = String

data BookGenre = Fantasy | Detective | Scientific | Dictionary | Fiction 
    deriving (Show, Read, Eq)


type Name = String


query :: Parser Query
query =
        parseAddBookQuery
    <|> parseRemoveBookQuery
   


parseQuery :: String -> Either String Query
parseQuery s =
  case runParser query s of
    Left err -> Left err
    Right (q, r) -> if null r then Right q else Left ("Unrecognized characters:" ++ r)

-- Query Parsers

parseAddBookQuery :: Parser Query
parseAddBookQuery = do
    _ <- parseString "add "
    book <- parseBookInfo
    return $ AddBookQuery book

parseRemoveBookQuery :: Parser Query
parseRemoveBookQuery = do
    _ <- parseString "remove "
    book <- parseBookInfo
    return $ RemoveBookQuery book

parseBookInfo :: Parser BookInfo
parseBookInfo = do
  title <- parseTitle
  _ <- parseSpace
  author <- parseAuthor
  _ <- parseSpace
  genre <- parseBookGenre
  return $ BookInfo title author genre

parseTitle :: Parser Title
parseTitle = many1 parseLetter

parseAuthor :: Parser Author
parseAuthor = many1 parseLetter

parseBookGenre :: Parser BookGenre
parseBookGenre = parseData ["Fantasy", "Detective", "Scientific", "Dictionary", "Fiction"]

parseData :: (Read a) => [String] -> Parser a
parseData = foldr (\str acc -> parseString str *> return (read str) <|> acc) empty


-- States and State Transitions

data State = State { books :: [BookInfo]}

emptyState :: State
emptyState = State []

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition s (AddBookQuery book) =
    if bookExists book s
        then Left "Book already exists"
        else Right (Just (show book ++ " added."), addBook book s)

stateTransition s (RemoveBookQuery book) =
    if not (bookExists book s)
        then Left "Book not found."
        else Right (Just (show book ++ " removed."), removeBook book s)


bookExists :: BookInfo -> State -> Bool
bookExists book s = book `elem` books s

addBook :: BookInfo -> State -> State
addBook book s = s { books = book : books s }

removeBook :: BookInfo -> State -> State
removeBook book s = s { books = filter (/= book) (books s) }



