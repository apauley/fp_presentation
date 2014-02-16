f x = (2 * x^2) - (2 * x) + 3
g x = (f x) + 1

h :: (Int->Int) -> (Int->Int) -> Int
h f1 f2 = (f1 3) + (f2 2)

main :: IO()
main = do
  putStrLn $ "f(3) = " ++ (show $ f 3)
  putStrLn $ "g(2) = " ++ (show $ g 2)
  putStrLn $ "h(f, g) = " ++ (show $ h f g)
