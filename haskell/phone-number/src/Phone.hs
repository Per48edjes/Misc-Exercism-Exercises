{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Phone where

import Control.Applicative (Alternative (..))

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
        (a, s') <- p s
        Just (f a, s')

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (Parser p1) <*> (Parser p2) = Parser $ \s -> do
        (f, s') <- p1 s
        (a, s'') <- p2 s'
        Just (f a, s'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \s -> do
        (a, s') <- p s
        runParser (f a) s'

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    [] -> Nothing
    (c : cs) -> if p c then Just (c, cs) else Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

space :: Parser Char
space = char ' '

digitN :: Parser Char
digitN = satisfy (`elem` ['2' .. '9'])

digitX :: Parser Char
digitX = satisfy (`elem` ['0' .. '9'])

optional :: Parser a -> Parser ()
optional p = Parser $ \s -> case runParser p s of
    Nothing -> Just ((), s)
    Just (_, s') -> Just ((), s')

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = Parser $ \s -> case runParser p s of
    Nothing -> Just ([], s)
    Just (a, s') -> do
        (as, s'') <- runParser (zeroOrMore p) s'
        Just (a : as, s'')

countryCode :: Parser String
countryCode = do
    _ <- optional $ char '+'
    _ <- optional $ char '1'
    _ <- optional $ char ' '
    return "1"

areaCode :: Parser String
areaCode = do
    _ <- optional $ char '(' <|> char '-' <|> char '.'
    d1 <- digitN
    d2 <- digitX
    d3 <- digitX
    _ <- optional $ char ')' <|> char '-' <|> char '.'
    return [d1, d2, d3]

exchangeCode :: Parser String
exchangeCode = do
    _ <- optional $ char '-' <|> char '.'
    d1 <- digitN
    d2 <- digitX
    d3 <- digitX
    _ <- optional $ char '-' <|> char '.'
    return [d1, d2, d3]

subscriberNumber :: Parser String
subscriberNumber = do
    _ <- optional $ char '-' <|> char '.'
    d1 <- digitX
    d2 <- digitX
    d3 <- digitX
    d4 <- digitX
    return [d1, d2, d3, d4]

number :: String -> Maybe String
number s = case parsed s of
    Just (pn, "") -> Just (if length pn == 10 then pn else drop 1 pn)
    _ -> Nothing
  where
    spaces = zeroOrMore space
    parsed = runParser $ do
        cc <- spaces *> countryCode <* spaces
        ac <- spaces *> areaCode <* spaces
        ec <- spaces *> exchangeCode <* spaces
        sn <- spaces *> subscriberNumber <* spaces
        return $ cc ++ ac ++ ec ++ sn
