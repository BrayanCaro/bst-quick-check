{-# LANGUAGE ScopedTypeVariables #-}

module BST.Generator.CorrectGeneratorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Operations

spec :: Spec
spec = do
  it "el árbol generado es válido" $ do
    property $ prop_Valid
  it "shrink genera arboles válidos cuando el árbol generado ya era válido" $ do
    property $ prop_ShrinkValid

{-
   El siguiente generador ya crea árboles correctos aún cuando los encojemos con shrink

   Se puede comprobar usando:

   ```
   $ stack repl test/BST/Generator/CorrectGeneratorSpec.hs
   > genericShrink (Branch (Branch Leaf (-1) 0 Leaf) 0 0 Leaf :: Tree)
   > [...]    -- <---- ¡ya todos son válidos 🎉!
   ```

   ---
   Nota: Se pueden obtener una instancia de los árboles generados con:

   ```
   $ stack repl test/BST/Generator/CorrectGeneratorSpec.hs
   > generate arbitrary :: IO Tree
   ```
-}
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do
    items <- arbitrary
    return $ foldr (uncurry insert) Leaf (items :: [(k,v)])
  shrink = filter valid . genericShrink -- Se agrego el filtro

{-
   Es válido pensar en verificar si nuestros árboles generados son válidos
   por la necesidad de garantizar que todos los nodos hijos derechos tienen llaves menores al nodo actual,
   lo mismo para los izquierdos, pero sus llaves tienen que ser mayores.

   Si nuestra generación de árboles es erronea, entonces QuickCheck nos dirá que las
   operaciones que realicemos (insert, delete, union, ...) están mal cuando es un falso positivo,
   pues el problema es la generación con Arbitrary.
-}
prop_Valid :: Tree -> Bool
prop_Valid t = valid t

{-
   También es necesario verificar que los árboles que encogemos siguen
   siendo válidos, y si no, solo quedarnos son los que si lo son.
-}
prop_ShrinkValid :: Tree -> Property
prop_ShrinkValid t = valid t ==> filter (not . valid) (shrink t) === []
