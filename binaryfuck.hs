import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Char
import Data.List
import System.Environment
import Text.Parsec

data Inst = Inc | Dec | U | R | L | In | Out

data CodeTree = Node Inst CodeTree
              | Loop CodeTree CodeTree
              | Code CodeTree
              | Nil
              | Err String

data InstPtr = ToLoop   CodeTree
             | FromLoop CodeTree
             | ReadNext CodeTree
             | Start

data MemoryTree = Leaf Int | Fork Int MemoryTree MemoryTree

data Ptr = GoRight Int MemoryTree
         | GoLeft  Int MemoryTree

runBFParser p input = case parse p "" input of
    Left  err -> Err $ show err
    Right val -> Code val

parseBF = foldr1 apply <$> many1 parsers
    where parsers = loopP <|> incP <|> decP <|> uP <|> rP <|> lP <|> inpP <|> oupP
          loopP = liftM2 Loop (between (char '[') (char ']') parseBF) $ try parseBF <|> return Nil
          incP  = char '+' >> return (Node Inc Nil)
          decP  = char '-' >> return (Node Dec Nil)
          uP    = char '^' >> return (Node U   Nil)
          rP    = char '>' >> return (Node R   Nil)
          lP    = char '<' >> return (Node L   Nil)
          inpP  = char ',' >> return (Node In  Nil)
          oupP  = char '.' >> return (Node Out Nil)

apply (Node i Nil) y = Node i y
apply _            _ = Nil

run code = eval ([], code) ([], Leaf 0)

eval codeZipper memZipper = do
    let (iptrs, tree) = codeZipper
    case tree of
        (Code x)     -> eval (Start                    :iptrs, x) (front, back)
        (Loop x   y) -> eval ((ToLoop y)               :iptrs, x) (front, back)
        (Node Inc x) -> eval ((ReadNext $ Node Inc Nil):iptrs, x) (front, chHead inc back)
        (Node Dec x) -> eval ((ReadNext $ Node Dec Nil):iptrs, x) (front, chHead dec back)
        (Node U   x) -> eval ((ReadNext $ Node U   Nil):iptrs, x) $ moveU (front, back)
        (Node R   x) -> eval ((ReadNext $ Node R   Nil):iptrs, x) $ moveR (front, back)
        (Node L   x) -> eval ((ReadNext $ Node L   Nil):iptrs, x) $ moveL (front, back)
        (Node In  x) -> ord <$> getChar >>= \n ->
                        eval ((ReadNext $ Node In  Nil):iptrs, x) (front, chHead (\m->n) back)
        (Node Out x) -> (putChar $ chr $ getHead back) >>
                        eval ((ReadNext $ Node Out Nil):iptrs, x) (front, back)
        Nil          -> readBack codeZipper Nil
        Err x        -> putStrLn $ "Parse Error:\n" ++ x
    where (front, back) = memZipper
          getHead (Leaf x)         = x
          getHead (Fork x l r)  = x 
          chHead f (Leaf x)        = Leaf (f x)
          chHead f (Fork x l r) = Fork (f x) l r
          inc x = if x < 255 then x+1 else 0
          dec x = if x > 0   then x-1 else 255
          moveU ([]             , b) = ([], Fork 0 (Leaf 0) b)
          moveU ((GoRight x m):f, b) = (f , Fork x m b)
          moveU ((GoLeft  x m):f, b) = (f , Fork x b m)
          moveR (f, (Leaf x))        = (GoRight x (Leaf 0) : f, (Leaf 0)) 
          moveR (f, (Fork x l r)) = (GoRight x l : f, r)
          moveL (f, (Leaf x))        = (GoLeft  x (Leaf 0) : f, (Leaf 0))
          moveL (f, (Fork x l r)) = (GoLeft  x r : f, l)
          readBack (iptrs, tree) tmpTree
            = case iptrs of
                  ((ReadNext p):ps) -> readBack (ps, tree) (apply p tmpTree)
                  ((FromLoop p):ps) -> readBack (ps, tree) (Loop  p tmpTree)
                  ((ToLoop   p):ps) -> if getHead back == 0
                                           then eval ((FromLoop tmpTree):ps, p)   memZipper
                                           else eval (iptrs, (Loop tmpTree tree)) memZipper
                  [Start]           -> return ()
                  _                 -> putStrLn "Error"

rmNoises = concat.words.(concatMap (concat.takeWhile (/= "//").group)).lines

main = do
    [filePath] <- getArgs
    input <- rmNoises <$> readFile filePath
    run $ runBFParser parseBF input
