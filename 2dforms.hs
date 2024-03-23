-- (C)Tim Jacobs 2024
-- since 23.03.2024
-- part of the geopack
-- max column is 70!

-- triangleArea since 23.03.2024
-- parameters: 
--              edge length a,
--              edge length b,
--              edge length c
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c
    | a == b && b == c = equilateralArea a
    | a == b || b == c || c == a = rightAngledArea a b c
    | otherwise = generalArea a b c

  where
    equilateralArea s = sqrt 3 / 4 * s * s

    rightAngledArea x y z
      | x == y = x * z / 2
      | y == z = y * x / 2
      | otherwise = z * y / 2

    generalArea x y z = sqrt (s * (s - x) * (s - y) * (s - z))
      where
        s = (x + y + z) / 2

-- rectangleArea since 23.03.2024
-- works also for a rhombus,
-- parallelogram, square
-- parameters: 
--              edge length a,
--              edge length b
rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

-- parallelogramAreaAlphaAngel since 23.03.2024
-- if a and height are known rectangleArea
-- can also be used
-- parameters: 
--              edge length a,
--              edge length b,
--              alpha angle
parallelogramAreaAlphaAngle :: Float -> Float -> Float -> Float
parallelogramAreaAlphaAngle a b alpha = a * b * sin alpha

-- test module
main :: IO ()
main = do
    let side1 = 5.0
        side2 = 5.0
        side3 = 5.0

    putStrLn $ "A Dreieck 5x5x5: " ++ show (triangleArea side1 side2 side3)
    putStrLn $ "A Viereck 5x5: " ++ show (rectangleArea side2 side1)
    putStrLn $ "A Paral. 5x5 40°" ++ show (parallelogramAreaAlphaAngle side1 side1 40.0)
