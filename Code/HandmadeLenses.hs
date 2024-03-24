{-# LANGUAGE Rank2Types #-}
import Data.Functor.Const
import Data.Functor.Identity
import Data.Function ((&))
data Address = Address { _street :: String, _city :: String, _zip :: String }
data Person = Person { _name :: String, _age :: Int, _address :: Address}

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- getters
getName :: Person -> String
getName (Person n _ _) = n

getAge :: Person -> Int
getAge (Person _ a _) = a

getAddress :: Person -> Address
getAddress (Person _ _ a) = a

getStreet :: Address -> String
getStreet (Address s _ _) = s

getCity :: Address -> String
getCity (Address _ c _) = c

getZip :: Address -> String
getZip (Address _ _ z) = z

-- setters
setName :: Person -> String -> Person
setName (Person _ a ad) n = Person n a ad

setAge :: Person -> Int -> Person
setAge (Person n _ ad) a = Person n a ad

setAddress :: Person -> Address -> Person
setAddress (Person n a _) ad = Person n a ad

setStreet :: Address -> String -> Address
setStreet (Address _ c z) s = Address s c z

setCity :: Address -> String -> Address
setCity (Address s _ z) c = Address s c z

setZip :: Address -> String -> Address
setZip (Address s c _) z = Address s c z

-- lenses
nameLens :: Functor f => (String -> f String) -> Person -> f Person
nameLens f p = fmap (\n -> setName p n) (f $ getName p)

ageLens :: Functor f => (Int -> f Int) -> Person -> f Person
ageLens f p = fmap (\a -> setAge p a) (f $ getAge p)

addressLens :: Functor f => (Address -> f Address) -> Person -> f Person
addressLens f p = fmap (\a -> setAddress p a) (f $ getAddress p)

streetLens :: Functor f => (String -> f String) -> Address -> f Address
streetLens f a = fmap (\s -> setStreet a s) (f $ getStreet a)

cityLens :: Functor f => (String -> f String) -> Address -> f Address
cityLens f a = fmap (\c -> setCity a c) (f $ getCity a)

zipLens :: Functor f => (String -> f String) -> Address -> f Address
zipLens f a = fmap (\z -> setZip a z) (f $ getZip a)

-- view
view :: Lens' s a -> s -> a
view l s = getConst $ l Const s

-- set
set :: Lens' s a -> a -> s -> s
set l a s = runIdentity $ l (\_ -> Identity a) s


main :: IO ()
main = do
    let p = Person "John" 30  (Address "Elm" "Springfield" "12345")       
    putStrLn $ "Name: " ++ _name p ++ ", age: " ++ show (_age p)
    putStrLn $ "Name: " ++ view nameLens p ++ ", age: " ++ show (view ageLens p) ++ ", street: " ++ view (addressLens . streetLens) p
    let p2 = p & (set nameLens "Alice") & (set ageLens 25) & (set (addressLens . streetLens) "Oak")
    putStrLn $ "Name: " ++ view nameLens p2 ++ ", age: " ++ show (view ageLens p2) ++ ", street: " ++ view (addressLens . streetLens) p2
