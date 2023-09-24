module Gold (phi, polynomial) where

-- the golden ratio
phi :: Double
phi = (sqrt 5+1) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1
