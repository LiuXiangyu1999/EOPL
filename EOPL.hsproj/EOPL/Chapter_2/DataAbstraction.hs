module Chapter_2.DataAbstraction where

import Data.Char



-- The procedure empty-env, applied to no arguments, must produce a repre- sentation of the empty environment

-- apply-env applies a representation of an environment to a variable

--(extend-env var val env) produces a new environment that behaves like env, except that its value at variable var is v


-- we can divide the procedures of the interface into constructors and observers. In this example, empty-env and extend-env are the constructors, and apply-env is the only observer.


type Symbol = String
data SchemeVal = I Int | C Char | L [SchemeVal]
type Env = Symbol -> SchemeVal



emptyEnv :: Env
emptyEnv = \_ -> error "report-no-binding-found"


extendEnv :: Symbol -> SchemeVal -> Env -> Env
extendEnv var val e1 = \var' -> if var' == var then val
                                               else applyEnv e1 var

applyEnv :: Env -> Symbol -> SchemeVal
applyEnv e var = e var


type Identifier = String
data LcExp = Id Identifier | Lambda (Identifier) LcExp | Apply LcExp LcExp deriving (Show, Eq)



varExp :: Identifier -> LcExp
varExp = Id


lambdaExp :: Identifier -> LcExp -> LcExp
lambdaExp = Lambda


appExp :: LcExp -> LcExp -> LcExp
appExp = Apply


isVarExp :: LcExp -> Bool
isVarExp (Id _) = True
isVarExp _ = False


isLambdaExp :: LcExp -> Bool
isLambdaExp (Lambda _ _) = True
isLambdaExp _ = False


isAppExp :: LcExp -> Bool
isAppExp (Apply _ _) = True
isAppExp _ = False



varExp2Var :: LcExp -> Identifier
varExp2Var (Id i) = i
varExp2Var _ = error "Not varExp"


lambdaExp2BoundVar :: LcExp -> Identifier
lambdaExp2BoundVar (Lambda (i) _) = i
lambdaExp2BoundVar _ = error "Not lambdaExp"


lambdaExp2Body :: LcExp -> LcExp
lambdaExp2Body (Lambda _ body) = body
lambdaExp2Body _ = error "Not lambdaExp"



appExp2rator :: LcExp -> LcExp
appExp2rator (Apply l r) = l
appExp2rator _ = error "Not applyExp"



appExp2rand :: LcExp -> LcExp
appExp2rand (Apply l r) = r
appExp2rand _ = error "Not applyExp"



-- Exercise 2.20

data Tree = Nil | Branch Int Tree Tree deriving (Show, Eq)


insert :: Int -> Tree -> Tree
insert i Nil = Branch i Nil Nil
insert i (Branch j l r) | i >= j    = Branch j l (insert i r)
                        | otherwise = Branch j (insert i l) r
                        
buildTreeFromList :: [Int] -> Tree
buildTreeFromList [] = Nil
buildTreeFromList (i:is) = insert i (buildTreeFromList is) 



data Direction = NewLeft | NewRight deriving (Show, Eq)

type Parent = (Direction, Int, Tree)


data NewTree = NewBranch ([Parent], Tree) deriving (Show, Eq)



creatNewTree :: Tree -> NewTree
creatNewTree t = NewBranch ([], t)



moveLeft :: NewTree -> NewTree
moveLeft (NewBranch (ps, Nil)) = error "Cannot move to left"
moveLeft (NewBranch (ps, (Branch i l r))) = let newParent = (NewLeft, i, r)
                                            in NewBranch (newParent:ps, l)



moveRight :: NewTree -> NewTree
moveRight (NewBranch (ps, Nil)) = error "Cannot move to right"
moveRight (NewBranch (ps, (Branch i l r))) = let newParent = (NewRight, i, l)
                                              in NewBranch (newParent:ps, r)
                                              


moveUp :: NewTree -> NewTree
moveUp (NewBranch ([], _)) = error "Cannot move up"
moveUp (NewBranch ((NewLeft, i, t):ps, t')) = NewBranch (ps, Branch i t' t)
moveUp (NewBranch ((NewRight, i, t):ps, t')) = NewBranch (ps, Branch i t t')



-- Abstract Syntax and Its Representation

data Par a = Par (String -> [(a, String)])


instance Monad Par where
  return x = Par $ \s -> [(x, s)]
  p >>= k = Par $ \s -> [(y, s'') | (x, s') <- runPar p s, (y, s'') <- runPar (k x) s']


runPar :: Par a -> String -> [(a, String)]
runPar (Par f) = f


fail' = Par $ \s -> []

sat :: (Char -> Bool) -> Par Char
sat p = Par f
        where f [] = []
              f (c:cs) | p c       = [(c, cs)]
                       | otherwise = []



char :: Char -> Par ()
char c = do {
              sat (==c);
              return ();
          }



string :: String -> Par ()
string [] = return ();
string (c:cs) = do {
                      sat (== c);
                      string cs;
                      return ();
                  }



(<|>) :: Par a -> Par a -> Par a
p <|> q = Par $ \s -> if null (runPar p s) then if null (runPar q s) then []
                                                                     else (runPar q s)
                                           else (runPar p s)


none = return []

optional p = p <|> none

some :: Par a -> Par [a]
many :: Par a -> Par [a]
some p = do {
              x <- p;
              xs <- many p;
              return (x:xs);
          }
many p = optional (some p)




spaces :: Par ()
spaces = (many (char ' ')) >> return ()



symbol :: String -> Par ()
symbol xs = spaces >> string xs


token :: Par a -> Par a 
token p = spaces >> p

getVarName :: Par String
getVarName = token (some (sat isAlpha))


parId :: Par LcExp
parId = do {
              id <- getVarName;
              return (Id id);
          }


parLambda :: Par LcExp
parLambda = do {
                symbol "(";
                symbol "lambda";
                symbol "("; 
                x <- getVarName;
                symbol ")";
                exp <- parLcExp;
                symbol ")";
                return (Lambda (x) exp);
              }


parApply :: Par LcExp
parApply = do {
                symbol "(";
                symbol "apply";
                operator <- token parLcExp;
                operand <- token parLcExp;
                symbol ")";
                return (Apply operator operand);
            }



parLcExp :: Par LcExp
parLcExp = parLambda <|> parApply <|> parId




-- Exercise 2.31

parDigi :: Par Int
parDigi = do { x <-  token (some (sat isDigit)); return (read x :: Int);}


data PrefixExp = Number Int | Neg PrefixExp PrefixExp deriving Show


parNumber :: Par PrefixExp
parNumber = parDigi >>= (\x -> return (Number x))

parNeg :: Par PrefixExp
parNeg = do {
              symbol "-";
              x <- parPrefixExp;
              y <- parPrefixExp;
              return (Neg x y);
          }


parPrefixExp :: Par PrefixExp
parPrefixExp = parNumber <|> parNeg



parPrefixList :: Par PrefixExp
parPrefixList = do {symbol "("; exp <- parPrefixExp; symbol ")"; return exp}












