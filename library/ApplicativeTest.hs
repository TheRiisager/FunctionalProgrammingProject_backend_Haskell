module ApplicativeTest where

data Status a = 
    Failure
    | Success a deriving(Show)

instance Functor Status where
    fmap func (Success a) = Success $ func a
    fmap func Failure = Failure

instance Applicative Status where
    pure                            = Success
    (Success func)  <*> (Success a) = Success $ func a
    Failure         <*> _           = Failure
    _               <*> Failure     = Failure

data Dumbass = Dumbass
    { idiotName :: String 
    , stupidLvl :: Int
    } deriving(Show)

idiotMaker :: Status Dumbass
idiotMaker =
    Dumbass <$> Success "Idiot McStupid" <*> Success 9001

idiotMakerEvolved :: Status Dumbass
idiotMakerEvolved =
    Dumbass <$> Failure <*> Success 9001




