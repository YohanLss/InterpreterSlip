{-# OPTIONS_GHC -Wall #-}

module Psil (run) where
import Text.ParserCombinators.Parsec
import Data.Char
import System.IO

-- 1ère représentation interne des expressions de notre langage
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          deriving (Show, Eq)

-- Les commentaires commencent par un point-virgule et se terminent à la fin de la ligne.
pComment :: Parser ()
pComment = do
  _ <- char ';'
  _ <- many (satisfy (/= '\n'))
  _ <- (char '\n' <|> (eof >> return '\n'))
  return ()

-- N'importe quelle combinaison d'espaces et de commentaires est considérée comme du blanc.
pSpaces :: Parser ()
pSpaces = many (space <|> (pComment >> return ' ')) >> return ()

-- Un nombre entier est composé de chiffres.
integer :: Parser Int
integer = do
  c <- digit
  integer' (digitToInt c)
  <|> do
    _ <- char '-'
    n <- integer
    return (-n)
  where
    integer' :: Int -> Parser Int
    integer' n = do
      c <- digit
      integer' (10 * n + digitToInt c)
      <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes de ponctuations.
pSymchar :: Parser Char
pSymchar = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do
  s <- many1 pSymchar
  return (case parse integer "" s of
            Right n -> Snum n
            _ -> Ssym s)

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do
  c <- satisfy (\c -> c `elem` "'`,")
  pSpaces
  e <- pSexp
  return (Scons
          (Scons Snil
                 (Ssym (case c of
                          ',' -> "shorthand-comma"
                          '`' -> "shorthand-backquote"
                          _   -> "shorthand-quote")))
          e)

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do
  _ <- char '('
  pSpaces
  (do
      _ <- char ')'
      return Snil
    <|> do
      hd <- do
              e <- pSexp
              pSpaces
              do
                _ <- char '.'
                pSpaces
                return e
                <|> return (Scons Snil e)
      pLiat hd)
  where
    pLiat :: Sexp -> Parser Sexp
    pLiat hd = do
      _ <- char ')'
      return hd
      <|> do
        e <- pSexp
        pSpaces
        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do
  c <- anyChar
  return (Just c)
  <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do
  pTsil
  <|> pQuote
  <|> pSymbol
  <|> do
    x <- pAny
    case x of
      Nothing -> pzero
      Just c -> error ("Unexpected char '" ++ [c] ++ "'")

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do
  pSpaces
  many (do
          e <- pSexpTop
          pSpaces
          return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction générique "read".
instance Read Sexp where
  readsPrec _ s = case parse pSexp "" s of
                    Left _ -> []
                    Right e -> [(e,"")]

-- Sexp Pretty Printer
showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
  where
    showHead (Scons Snil e') = showString "(" . showSexp' e'
    showHead (Scons e1' e2') = showHead e1' . showString " " . showSexp' e2'
    showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show" (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela, il faut enlever le "deriving Show" dans la déclaration de Sexp.
--instance Show Sexp where
--    showsPrec p = showSexp'

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

-- Représentation intermédiaire Lexp
type Var = String

data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lproc Var Lexp      -- Fonction anonyme prenant un argument.
          | Ldo Lexp Lexp       -- Appel de fonction, avec un argument.
          | Lnull               -- Constructeur de liste vide.
          | Lnode Lexp Lexp     -- Constructeur de liste.
          | Lcase Lexp Lexp Var Var Lexp -- Expression conditionnelle.
          | Ldef [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "null") = Lnull
s2l (Ssym s) = Lvar s
s2l (Scons (Ssym "+") (Scons e1 (Scons e2 Snil))) = Ldo (Ldo (Lvar "+") (s2l e1)) (s2l e2)
s2l (Scons (Ssym "-") (Scons e1 (Scons e2 Snil))) = Ldo (Ldo (Lvar "-") (s2l e1)) (s2l e2)
s2l (Scons (Ssym "*") (Scons e1 (Scons e2 Snil))) = Ldo (Ldo (Lvar "*") (s2l e1)) (s2l e2)
s2l (Scons (Ssym "/") (Scons e1 (Scons e2 Snil))) = Ldo (Ldo (Lvar "/") (s2l e1)) (s2l e2)
s2l (Scons f args) = foldl Ldo (s2l f) (map s2l (sexpToList args))
s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))

sexpToList :: Sexp -> [Sexp]
sexpToList Snil = []
sexpToList (Scons h t) = h : sexpToList t
sexpToList s = error ("Invalid Sexp list: " ++ showSexp s)

-- Représentation du contexte d'exécution
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vop (Value -> Value)
           | Vclosure VEnv Var Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _ Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _ (Vop _) = showString "<primitive>"
    showsPrec _ (Vclosure _ _ _) = showString "<fonction>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop :: (Int -> Int -> Int) -> Value
           binop op = Vop (\v1 -> case v1 of
                                   (Vnum n1)
                                    -> Vop (\v2 -> case v2 of
                                                    (Vnum n2)
                                                     -> Vnum (n1 `op` n2)
                                                    _ -> error "Pas un nombre")
                                   _ -> error "Pas un nombre")
      in [("+", binop (+)),
          ("*", binop (*)),
          ("/", binop div),
          ("-", binop (-))]

-- Évaluateur
eval :: VEnv -> Lexp -> Value
eval _ (Lnum n) = Vnum n
eval env (Lvar x) = case lookup x env of
                      Just v -> v
                      Nothing -> error ("Variable non définie: " ++ x)
eval env (Lproc x body) = Vclosure env x body
eval env (Ldo f a) = case eval env f of
                       Vclosure env' x body -> eval ((x, eval env a) : env') body
                       Vop op -> case eval env a of
                                   Vop op2 -> op (Vop op2)
                                   arg -> op arg
                       _ -> error "Application sur une valeur non-fonction"
eval env Lnull = Vnil
eval env (Lnode e1 e2) = Vcons (eval env e1) (eval env e2)
eval env (Lcase e en x xs ec) = case eval env e of
                                  Vnil -> eval env en
                                  Vcons v1 v2 -> eval ((xs, v2) : (x, v1) : env) ec
                                  _ -> error "Condition sur une valeur non-liste"
eval env (Ldef defs body) = eval (foldr (\(x, e) env' -> (x, eval env' e) : env') env defs) body

-- Toplevel
evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename = do
  inputHandle <- openFile filename ReadMode
  hSetEncoding inputHandle utf8
  s <- hGetContents inputHandle
  hPutStr stdout . show $
    let sexps s' = case parse pSexps filename s' of
                     Left _ -> [Ssym "#<parse-error>"]
                     Right es -> es
    in map evalSexp (sexps s)
  hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
