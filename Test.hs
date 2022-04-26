{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test where


class Conv a b where
    conv :: a -> b

instance Conv a a where
    conv v = v
instance Conv a Int where
    conv _ = 9


func :: b -> (b -> Int) -> Int
func v f = f v


f :: b -> Int
f _ = 0

g :: Conv a b => a -> Int
g v = func (conv v) f


data Ged a b
    = A a 
    | B b


function :: Int -> Int
function v = v

function :: String -> Int
function _ = 0
