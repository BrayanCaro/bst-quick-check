module BST.Metamorphic.BadSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BST.Data
import BST.Arbitrary ()
import BST.Operations

spec :: Spec
spec = do
  it "prop_InsertInsert" $ property prop_InsertInsert
  it "prop_InsertInsertSameKey" $ property prop_InsertInsertSameKey
  it "prop_InsertPreservesEquiv" $ property prop_InsertPreservesEquiv

-- La siguiente propiedad falla, pues no hemos definido
-- relaciones de equivalencia
-- Además de que no se considera el caso cuando la llave es la misma, pues insert
-- sobreescribe cuando los valores se repiten
prop_InsertInsert :: (Key, Val) -> (Key, Val) -> Tree -> Property
prop_InsertInsert (k , v) (k', v') t =
  insert k v (insert k' v' t) === insert k' v' (insert k v t)

-- La siguiente propiedad también falla, pues aunque ya considera la relación de equivalencia
prop_InsertInsertSameKey :: (Key, Val) -> (Key, Val) -> Tree -> Property
prop_InsertInsertSameKey (k , v) (k', v') t =
  insert k v (insert k' v' t) === if k == k' then insert k v t else insert k' v' (insert k v t)


-- Otra propiedad que nos gustaría verificar es probar que se preserva el orden
-- Pero dado que es muy poco probable que eso ocurra QuickCheck termina por darse por vencido
-- Para resoler esto necesitamos crear un par de árboles con las mismas llaves pero insertados en distinto
-- orden.
prop_InsertPreservesEquiv :: Key -> Val -> Tree -> Tree -> Property
prop_InsertPreservesEquiv k v t t' =
  t ~=~ t' ==> insert k v t ~=~ insert k v t'
