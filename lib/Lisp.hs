module Lisp where

import Control.Applicative
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either (fromLeft, fromRight, isRight, lefts, rights)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.Environment (getArgs)
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data Expr
    = Number Int
    | Ident String
    | List [Expr]
    | Function Expr Expr
    | Boolean Bool
    | Char Char
    | Write Expr
    | Nil
    deriving (Ord, Eq)

instance Show Expr where
    show (Number x) = show x
    show (Write x) = show x
    show (Ident x) = x
    show Nil = "nil"
    show (List x)
        | all isChar x = show $ map fromChar x
        | otherwise = "(" ++ unwords (map show x) ++ ")"
      where
        fromChar :: Expr -> Char
        fromChar (Char x) = x
        isChar :: Expr -> Bool
        isChar (Char _) = True
        isChar _ = False
    show (Function args body) = "(lambda " ++ show args ++ " " ++ show body ++ ")"
    show (Boolean True) = "true"
    show (Boolean False) = "false"
    show (Char x) = "'" ++ [x]

type Variables = Map.Map Expr Expr

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither x Nothing = Left x
maybeToEither _ (Just x) = Right x

parseList :: P.Parser Expr
parseList = do
    _ <- P.char '('
    _ <- P.spaces
    list <- P.sepBy parseExpression P.spaces
    _ <- P.spaces
    _ <- P.char ')'
    return $ List list

parseComment :: P.Parser String
parseComment = do
    P.char ';'
    P.manyTill P.anyChar (P.char '\n')

parseBool :: P.Parser Expr
parseBool = (Boolean True <$ P.try (P.string "true")) <|> (Boolean False <$ P.try (P.string "false"))

parseNumber :: P.Parser Expr
parseNumber = Number . read <$> P.many1 P.digit

parseIdent :: P.Parser Expr
parseIdent = Ident <$> P.many1 (P.letter <|> P.oneOf "+-=")

parseString :: P.Parser Expr
parseString = do
    _ <- P.char '"'
    result <- P.many1 (P.letter <|> P.oneOf "+-=")
    _ <- P.char '"'
    return $ List $ Ident "list" : map Char result

parseChar :: P.Parser Expr
parseChar = do
    P.char '\''
    Char <$> P.anyChar

parseQuoted :: P.Parser Expr
parseQuoted = P.try $ do
    _ <- P.char '\''
    result <- parseList
    let (List xs) = result
    return $ List $ Ident "list" : xs

parseExpression :: P.Parser Expr
parseExpression = do
    P.spaces
    P.many parseComment
    P.spaces
    parseBool <|> parseNumber <|> parseIdent <|> parseList <|> parseString <|> parseQuoted <|> parseChar

evalExpression :: Variables -> Expr -> Either String (Variables, Expr)
evalExpression vars (Char x) = Right (vars, Char x)
evalExpression vars (Boolean x) = Right (vars, Boolean x)
evalExpression vars (Number x) = Right (vars, Number x)
evalExpression vars (Ident x) = case Map.lookup (Ident x) vars of
    Just a -> Right (vars, a)
    Nothing -> Left $ "not defined " ++ x
evalExpression vars (List (Ident "+" : args))
    | all isRight evalargs = if isRight (snd folded) then Right (second (fromRight (Number 0)) folded) else Left (fromLeft "" (snd folded))
    | otherwise = Left $ intercalate "\n" (lefts evalargs)
  where
    folded = foldem (map (second Right) (rights evalargs))
    foldem = foldl (\(accmap, accx) (yo, x) -> (Map.union accmap yo, plus accx x)) (Map.empty, Right (Number 0))
    evalargs = map (evalExpression vars) args
    plus :: Either String Expr -> Either String Expr -> Either String Expr
    plus (Right (Number a)) (Right (Number b)) = Right $ Number $ a + b
    plus _ _ = Left "cant plus non numbers"
evalExpression vars (List [Ident "=", a, b]) = do
    (amap, resa) <- evalExpression vars a
    (bmap, resb) <- evalExpression amap b
    result <- equal resa resb
    return (bmap, result)
  where
    equal :: Expr -> Expr -> Either String Expr
    equal x y = Right $ Boolean $ x == y
evalExpression vars (List [Ident "-", a, b]) = do
    (amap, resa) <- evalExpression vars a
    (bmap, resb) <- evalExpression amap b
    result <- minus resa resb
    return (bmap, result)
  where
    minus :: Expr -> Expr -> Either String Expr
    minus (Number x) (Number y) = Right $ Number $ x - y
    minus _ _ = Left "can only minus number"
evalExpression vars (List [Ident "listp", elem]) = do
    (newvars, result) <- evalExpression vars elem
    return $ case result of
        (List _) -> (vars, Boolean True)
        _ -> (vars, Boolean False)
evalExpression vars (List [Ident "write", expr]) = second Write <$> evalExpression vars expr
evalExpression vars (List (Ident "list" : list)) = do
    result <- traverse (evalExpression vars) list
    return (vars, List (map snd result))
evalExpression vars (List [Ident "null", list]) = do
    (newvars, result) <- evalExpression vars list
    let (List a) = result
    return (vars, Boolean (null a))
evalExpression vars (List [Ident "cons", elemenet, list]) = do
    (newvars, result) <- evalExpression vars list
    (newvars2, el) <- evalExpression vars elemenet
    let (List a) = result
    return (newvars2, List (el : a))
evalExpression vars (List [Ident "tail", x]) = do
    (newvars, result) <- evalExpression vars x
    let (List a) = result
    Right (newvars, List (tail a))
evalExpression vars (List [Ident "first", x]) = do
    (newvars, result) <- evalExpression vars x
    let (List a) = result
    let myhead l
                | null l = Left "cant call first on empty list"
                | otherwise = Right $ head l
    res <- myhead a
    Right (newvars, res)
evalExpression vars (List [Ident "if", expression, iftrue, iffalse]) = do
    (newvars, result) <- evalExpression vars expression
    let (Boolean bla) = result
    if bla
        then
            evalExpression newvars iftrue
        else
            evalExpression newvars iffalse
evalExpression vars (List [Ident "lambda", arguments, body]) = Right (vars, Function arguments body)
evalExpression vars (List [Ident "defvar", name, value]) = do
    result <- evalExpression vars value
    return $ first (Map.insert name (snd result)) result
evalExpression vars (List [Ident "defun", name, args, body]) = Right (Map.insert name (Function args body) vars, Function args body)
evalExpression vars (List x) = do
    function <- maybeToEither ("cant find element " ++ show (head x)) $ Map.lookup (head x) vars
    let (Function arguments body) = function
    evaled <- traverse (evalExpression vars) (tail x)
    args <- getlist arguments
    let newmap = Map.union (Map.fromList $ zip args (map snd evaled)) vars
    result <- evalExpression newmap body
    return (vars, snd result)
  where
    getlist :: Expr -> Either String [Expr]
    getlist (List a) = Right a
    getlist _ = Left "should be list"
evalExpression _ (Function _ _) = error "should not reach this"

unwrap :: Either a a -> a
unwrap (Right x) = x
unwrap (Left x) = x

runfile :: Variables -> String -> String -> Either String (Variables, String)
runfile startvariables filename file = case parsed of
    Right x -> foldl uhh (Right (startvariables, "")) x
    Left x -> Right (Map.empty, show x)
  where
    parsed = P.runParser (P.sepEndBy parseExpression P.spaces) () filename file
    uhh (Right (vars, output)) a = second handlewrite <$> evalExpression vars a
      where
        handlewrite (Write x) = output ++ show x ++ "\n"
        handlewrite _ = output
    uhh (Left x) _ = Left x

runmain :: String -> String -> IO String
runmain filename file = do
    stdfile <- readFile "std.lisp"
    let std = fst <$> runfile (Map.fromList [(Ident "nil", Nil)]) "std.list" stdfile
    case std of
        Left x -> return x
        Right x -> return $ unwrap $ snd <$> runfile x filename file
