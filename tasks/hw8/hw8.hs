{-# OPTIONS_GHC -fno-warn-orphans #-}
import Data.Monoid
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gl f) = GL (e:gl) ((empFun e) + f)

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL gl1 f1) (GL gl2 f2) = GL (gl1 ++ gl2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node n xs) = f n (map (treeFold f) xs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = ((GL [e] (empFun e)), (GL [] 0))
nextLevel e gls = (glCons e (foldr1 (<>) (snd p)), (foldr1 (<>) (fst p)))
  where p = unzip gls

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

main = print $ maxFun testCompany
