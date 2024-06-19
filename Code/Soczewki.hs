{-# LANGUAGE Rank2Types #-}
import Data.Functor.Const
import Data.Functor.Identity
import Data.Function ((&))
data Address = Address { _street :: String, _city :: String, _zip :: String }
data Person = Person { _name :: String, _age :: Int, _address :: Address}
data Student = Student { _person :: Person, _school :: String }

type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

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

getPerson :: Student -> Person
getPerson (Student p _) = p

getSchool :: Student -> String
getSchool (Student _ s) = s

getStudentStreet :: Student -> String
getStudentStreet (Student (Person _ _ (Address s _ _)) _) = s


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

setPerson :: Student -> Person -> Student
setPerson (Student _ s) p = Student p s

setSchool :: Student -> String -> Student
setSchool (Student p _) s = Student p s

setStudentStreet :: Student -> String -> Student
setStudentStreet (Student (Person n a (Address _ c z)) s) st = Student (Person n a (Address st c z)) s

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

personLens :: Functor f => (Person -> f Person) -> Student -> f Student
personLens f s = fmap (\p -> setPerson s p) (f $ getPerson s)

schoolLens :: Functor f => (String -> f String) -> Student -> f Student
schoolLens f s = fmap (\sc -> setSchool s sc) (f $ getSchool s)

studentStreetLens :: Functor f => (String -> f String) -> Student -> f Student
studentStreetLens f s = fmap (\st -> setStudentStreet s st) (f $ getStudentStreet s)

-- view
view :: Lens' s a -> s -> a
view l s = getConst $ l Const s

-- set
set :: Lens' s a -> a -> s -> s
set l a s = runIdentity $ l (\_ -> Identity a) s


main :: IO ()
main = do
    let s = Student (Person "Homer" 40 (Address "123 Elm St" "Springfield" "12345")) "MIT"
    -- set street of student's address
    let s2 = s & (set (personLens . addressLens . streetLens) "123 Oak St")
    let s3 = s & (set studentStreetLens "John St")
    putStrLn $ "Student street: " ++ view studentStreetLens s2
    putStrLn $ "Student street: " ++ view studentStreetLens s3