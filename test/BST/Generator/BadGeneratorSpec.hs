{-# LANGUAGE ScopedTypeVariables #-}

module BST.Generator.BadGeneratorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Operations

spec :: Spec
spec = do
  it "el borrado de árboles genera árboles validos, es ineficiente, pero es una forma de resolverlo" $ do
    property $ prop_DeleteValid
  it "el árbol generado es válido" $ do
    property $ prop_Valid
  it "shrink genera arboles válidos cuando el árbol generado ya era válido" $ do
    property $ prop_ShrinkValid

{-
   El siguiente generador está mal definido.

   La razón, la función shrink simplifica los árboles generados
   pero los nuevos arboles generados no son válidos.

   Se puede comprobar usando:

   ```
   $ stack repl test/BST/Generator/BadGeneratorSpec.hs
   > genericShrink (Branch (Branch Leaf (-1) 0 Leaf) 0 0 Leaf :: Tree)
   > [... , Branch (Branch Leaf 0 0 Leaf) 0 0 Leaf]    -- <---- ¡este árbol no es válido!
   ```

   Lo cual es un problema, pues si necesitamos nuestros árboles generados encogidos
   a su expresión mínima, genericShrink.

   Pero genericShrink nos da una lista de árboles, así que podemos solo filtrar por
   los que nos interesa y eso se ve en el archivo: test/BST/Generator/CorrectGeneratorSpec.hs

   ---
   Nota: Se pueden obtener una instancia de los árboles generados con:

   ```
   $ stack repl test/BST/Generator/BadGeneratorSpec.hs
   > generate arbitrary :: IO Tree
   ```
-}
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do
    items <- arbitrary
    return $ foldr (uncurry insert) Leaf (items :: [(k,v)])
  shrink = genericShrink

prop_Valid :: Tree -> Bool
prop_Valid t = valid t

{-
  Ejemplo de uso de precondición.

  Dado que nuestros árboles generados son inválidos entonces
  podemos considerar que solo usaremos los que sean validos,
  pero es ineficiente, y no solo eso, si no que tampoco pasa.
-}
prop_DeleteValid :: Key -> Tree -> Property
prop_DeleteValid k t =  valid t ==> valid (delete k t)

prop_ShrinkValid :: Tree -> Property
prop_ShrinkValid t = valid t ==> filter (not . valid) (shrink t) === []
