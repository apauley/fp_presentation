import qualified Data.Map as Map

data Bank = ABSA | Capitec | FNB | Nedbank | SBSA
     deriving (Show, Ord, Eq)

data Amount = Amount {value :: Rational}
     deriving (Eq, Ord)

instance Show Amount where
         show (Amount value) = "R"++show (fromRational value)

instance Num Amount where
         Amount a + Amount b = Amount (a+b)
         Amount a * Amount b = Amount (a*b)
         abs (Amount a) = Amount (abs a)
         fromInteger i = Amount (fromInteger i)
         signum (Amount a) = Amount (signum a)

data Account = Account {bank    :: Bank
                       ,accNum  :: String
                       ,owner   :: String
                       ,balance :: Amount}

instance Show Account where
         show (Account bank accNum owner balance) = 
              unwords [show bank, accNum,"("++owner++")", show balance]

accounts = [Account {accNum="81423",
                     owner="J. Doe",
                     balance=(Amount 123000.23),
                     bank=ABSA},
            Account {accNum="48687", 
                     owner="J. Black",
                     balance=(Amount 5782347.99),
                     bank=FNB},
            Account {accNum="62435",
                     owner="A. Kay",
                     balance=(Amount 100),
                     bank=ABSA},
            Account {accNum="48687",
                     owner="S. Jones",
                     balance=(Amount 2937361.45),
                     bank=FNB}]

balances :: [Account] -> [Amount]
balances = map balance

loaded :: [Account] -> [Account]
loaded = filter isLoaded

isLoaded :: Account -> Bool
isLoaded account = balance account >= (Amount 1000000)

balanceSum :: [Account] -> Amount
balanceSum accounts = foldl (+) 0 (balances accounts)

balancesPerBank :: [Account] -> Map.Map Bank Amount
balancesPerBank = foldl insertBankBalance Map.empty

insertBankBalance :: Map.Map Bank Amount -> Account -> Map.Map Bank Amount
insertBankBalance bankmap account = Map.insert key value bankmap
                          where key   = bank account
                                value = addBalance account bankmap

addBalance :: Account -> Map.Map Bank Amount -> Amount
addBalance account bankmap = case (Map.lookup (bank account) bankmap) of
                                Nothing  -> balance account
                                Just bal -> (balance account) + bal

main :: IO ()
main = do
    putStrLn $ "Account balances:\n" ++ show (balances accounts) ++ "\n"
    putStrLn $ "Accounts that are loaded:\n" ++ show (loaded accounts) ++ "\n"
    putStrLn $ "Sum of balances:\n" ++ show (balanceSum accounts) ++ "\n"
    putStrLn $ "Map of balances per bank:\n" ++ show (balancesPerBank accounts)
