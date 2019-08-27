module Chapter_1.Recursive() where
  

  
-- Definition 1.1.1


inS :: Int -> Bool
inS 0 = True
inS n | n < 0     = False
      | otherwise = inS (n - 3)
      

-- Definition 1.1.6
type SList = [SExp]
data SExp = Symbol String | SL SList





-- Definition 1.1.8
type Indentifier = String
data LcExp = I Indentifier | Lambda (Indentifier) LcExp | Apply LcExp LcExp



-- 1.2.4 Occurs Free? 

occursFree :: String -> LcExp -> Bool
occursFree x (I e) = x == e
occursFree x (Lambda (y) e) = (x /= y) && (occursFree x e)
occursFree x (Apply e1 e2) = (occursFree x e1) || (occursFree x e2)




-- 1.2.5 Subst

--subst : Sym × Sym × S-list → S-list
--subst-in-s-exp : Sym × Sym × S-exp → S-exp

subSt :: String -> String -> SList -> SList
subStInSExp :: String -> String -> SExp -> SExp
subSt new old [] = []
subSt new old (e: es) = (subStInSExp new old e) : (subSt new old es)

subStInSExp new old (Symbol s) = if (s == old) then Symbol new
                                               else Symbol old
subStInSExp new old (SL sl) = SL (subSt new old sl)


-- 1.3 Auxiliary Procedures and Context Arguments

numberElements :: [a] -> [(Int, a)]
numberElements x = zip [0, 1 ..] x


-- Mysterious Auxiliaries!
-- When defining an auxiliary procedure, always specify what it does on all arguments, not just the initial values
   
-- Exercise 1.34

data BinTree = Nil | BT Int BinTree BinTree
data Direction = Left | Right deriving Show


path :: Int -> BinTree -> [Direction]
inTree :: Int -> BinTree -> Bool

path i Nil = []
path i (BT j t1 t2) | i == j    = []
                    | otherwise = if inTree i t1 then Chapter_1.Recursive.Left : (path i t1)
                                                 else if  inTree i t2 then Chapter_1.Recursive.Right : (path i t2)
                                                                      else []
inTree i Nil = False
inTree i (BT j t1 t2) = (i == j) || (inTree i t1) || (inTree i t2)


-- Use state Monad to implement Exercise 1.35

data State s a = ST (s -> (a, s))

runState :: State s a -> (s -> (a, s))
runState (ST f) = f

get :: State s s
get = ST (\s -> (s, s))


modify f (ST g)= ST (\s -> let a = fst . g $ s
                            in (a, f s))


instance Monad (State s) where
  return x = ST (\s -> (x, s))
  p >>= q = ST (\s -> let (a, s') = runState p s
                          tmp = q a
                      in runState tmp s')





--type Stack = [Int]

--push :: Int -> State Stack ()
--push x = ST (\s -> ((), x:s))



--pop :: State Stack Int
--pop = ST (\(s:ss) -> (s, ss))



data LeafInt = Leaf Int | Branch String LeafInt LeafInt deriving Show


type Walker = State Int LeafInt



change :: LeafInt -> Walker
change (Leaf i) = do {n <- get; modify (+1) (return (Leaf n)) }
change (Branch s l1 l2) = do {
                                l1' <- change l1; 
                                l2' <- change l2; 
                                return (Branch s l1' l2'); 
                             }
























































