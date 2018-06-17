module Main where

import Cp
import Nat
import Graphics.Gloss


data FTree a b = Unit b | Comp a (FTree a b) (FTree a b) deriving (Eq,Show)
type PTree = FTree Square Square
type Square = Float

inFTree :: Either b (a, (FTree a b, FTree a b)) -> FTree a b
outFTree :: FTree a1 a2 -> Either a2 (a1, (FTree a1 a2, FTree a1 a2))
baseFTree :: (a1 -> b1) -> (a2 -> b2) -> (a3 -> d) -> Either a2 (a1, (a3, a3)) -> Either b2 (b1, (d, d))
recFTree :: (a -> d) -> Either b1 (b2, (a, a)) -> Either b1 (b2, (d, d))
cataFTree :: (Either b1 (b2, (d, d)) -> d) -> FTree b2 b1 -> d
anaFTree :: (a1 -> Either b (a2, (a1, a1))) -> a1 -> FTree a2 b
hyloFTree :: (Either b1 (b2, (c, c)) -> c) -> (a -> Either b1 (b2, (a, a))) -> a -> c
{-
generatePTree n = aux n 100 where 
       aux :: Int -> Float -> PTree 
       aux 0 x = Unit x
       aux n x = Comp x (aux (n-1) (x * (sqrt(2)/2))) (aux (n-1) (x * (sqrt(2)/2))) 
-}
inFTree = either Unit (uncurry(\a -> uncurry (Comp a)))
outFTree (Unit c)         = Left c
outFTree (Comp a t1 t2) = Right(a,(t1,t2))

cataFTree a = a . (recFTree (cataFTree a)) . outFTree
anaFTree f = inFTree . (recFTree (anaFTree f) ) . f
hyloFTree a c = cataFTree a . anaFTree c
baseFTree f g h  = g -|- (f  >< (h >< h))
recFTree f = baseFTree id id f

instance BiFunctor FTree
         where bmap f g = cataFTree ( inFTree . baseFTree f g id )

generatePTreeAux tam n = anaFTree (h . outNat) 
            where h = (const (side 0)) -|- (split (side . succ) (split id id))
                  side = size . (n -)
                  size = (tam *) . (((sqrt 2) / 2) ^)
                  
generatePTree n = generatePTreeAux 100 n n


drawPTree :: PTree -> [Picture]
drawPTree p = aux p (0,0) 0 where
      aux :: PTree -> (Float, Float) -> Float -> [Picture]
      aux (Unit c) (x,y) ang = [Translate x y (Rotate ang (square c))]
      aux (Comp c l r) (x,y) ang = [Translate x y (Rotate ang (square c))] ++ (aux l (x + somaXLeft,y + somaYLeft) (ang - 45)) ++ (aux r (x + somaXRight,y + somaYRight) (ang + 45)) 
                  where somaX = c/2
                        somaY = c
                        angRads = ang * pi / 180
                        branchToGlobal angle (dx,dy) = (dx * cos angle + dy * sin angle, dy * cos angle - dx * sin angle) 
                        (somaXLeft, somaYLeft) = branchToGlobal angRads (-somaX, somaY) 
                        (somaXRight, somaYRight) = branchToGlobal angRads (somaX, somaY)


window = (InWindow "CP" (800,800) (0,0))
square s = rectangleSolid s s

main = animate window white draw
    where
    pics = drawPTree (generatePTree 10)
    draw t = Pictures $ pics
