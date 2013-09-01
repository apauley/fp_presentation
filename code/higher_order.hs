f x = (2*x) + 3
g x = (f x) + 1

h :: (Int->Int) -> (Int->Int) -> Int -> Int
h func_a func_b x = (func_a x) + (func_b 2)

main :: IO()
main = do
  putStrLn $ "f(7) = " ++ (show $ f 7)
  putStrLn $ "g(2) = " ++ (show $ g 2)
  putStrLn $ "h(f, g, 7) = " ++ (show $ h f g 7)
