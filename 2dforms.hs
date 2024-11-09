-- (C)Tim Jacobs 2024
-- since 23.03.2024
-- part of the geopack

import Prelude

-- func triangleArea since 23.03.2024
-- parameters: 
--              edge length a,
--              edge length b,
--              edge length c
triangleArea :: Float -> Float -> Float -> Float
triangleArea x y z = sqrt (s * (s - x) * (s - y) * (s - z))
      where
        s = (x + y + z) / 2

-- func rectangleArea since 23.03.2024
-- parameters: 
--              edge length a,
--              edge length b
rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

-- func parallelogramAreaAlphaAngel since 23.03.2024
-- if a and height are known rectangleArea
-- can also be used
-- parameters: 
--              edge length a,
--              edge length b,
--              alpha angle
parallelogramAreaAlphaAngle :: Float -> Float -> Float -> Float
parallelogramAreaAlphaAngle a b alpha = a * b * sin alpha

-- func cosineLaw since 09.11.2024
-- parameters:
--              edge length a,
--              edge length b,
--              edge length c
-- returvalue:
--              angle in degrees instead of radians
cosineLaw :: Float -> Float -> Float -> Float
cosineLaw a b c = acos ((a**2 + b**2 - c**2) / (2 * a * b)) * (180 / pi)


-- test module
main :: IO ()
main = do
    let side1 = 5.0
        side2 = 5.0
        side3 = 5.0

    putStrLn $ "A triangel 5x5x5        " ++ show (triangleArea side1 side2 side3)
    putStrLn $ "A rectangle 5x5         " ++ show (rectangleArea side2 side1)
    putStrLn $ "A parallel. 5x5 40°     " ++ show (parallelogramAreaAlphaAngle side1 side1 40.0)
    putStrLn $ "cosLaw 9.6 15 8         " ++ show (cosineLaw 9.6 15 8)
    putStrLn $ "cosLaw 15 9.6 8         " ++ show (cosineLaw 9.6 8 15)
    putStrLn $ "cosLaw 15 8 9.6         " ++ show (cosineLaw 15 8 9.6)