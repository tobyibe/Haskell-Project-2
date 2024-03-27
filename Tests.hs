import Test.QuickCheck
import Expr
-- these tests were ran on non-university servers

-- Property 1: Identity Property
prop_evalIdentity :: Int -> Bool
prop_evalIdentity x = eval [] (Val x) == Just (IntVal x)

-- Property 2: Addition Commutativity
prop_additionCommutativity :: Int -> Int -> Bool
prop_additionCommutativity x y = eval [] (Add (Val x) (Val y)) == eval [] (Add (Val y) (Val x))

-- Property 3: Multiplication Commutativity
prop_multiplicationCommutativity :: Int -> Int -> Bool
prop_multiplicationCommutativity x y = eval [] (Mul (Val x) (Val y)) == eval [] (Mul (Val y) (Val x))

-- Property 4: Addition Associativity
prop_additionAssociativity :: Int -> Int -> Int -> Bool
prop_additionAssociativity x y z = eval [] (Add (Add (Val x) (Val y)) (Val z)) == eval [] (Add (Val x) (Add (Val y) (Val z)))

-- Property 5: Multiplication Associativity
prop_multiplicationAssociativity :: Int -> Int -> Int -> Bool
prop_multiplicationAssociativity x y z = eval [] (Mul (Mul (Val x) (Val y)) (Val z)) == eval [] (Mul (Val x) (Mul (Val y) (Val z)))

-- Property 6: Subtraction Property
prop_subtraction :: Int -> Int -> Bool
prop_subtraction x y = eval [] (Sub (Val x) (Val y)) == Just (IntVal (x - y))

-- Property 7: Division Property
prop_division :: Int -> Int -> Property
prop_division x y = y /= 0 ==> eval [] (Div (Val x) (Val y)) == if y /= 0 then Just (IntVal (x `div` y)) else Nothing

-- Property 8: Absolute Value Property
prop_absoluteValue :: Int -> Bool
prop_absoluteValue x = case eval [] (Abs (Val x)) of
                          Just (IntVal n) -> n >= 0
                          _ -> False

-- Property 9: Modulus Property
prop_modulus :: Int -> Int -> Property
prop_modulus x y = y /= 0 ==> eval [] (Mod (Val x) (Val y)) == if y /= 0 then Just (IntVal (x `mod` y)) else Nothing

-- Property 10: Power Property
prop_power :: Int -> Int -> Property
prop_power x y = eval [] (Pow (Val x) (Val y)) == if y >= 0 then Just (IntVal (x ^ y)) else Nothing

-- Run all tests
main :: IO ()
main = do
  putStrLn "Running tests:"
  quickCheck prop_evalIdentity
  quickCheck prop_additionCommutativity
  quickCheck prop_multiplicationCommutativity
  quickCheck prop_additionAssociativity
  quickCheck prop_multiplicationAssociativity
  quickCheck prop_subtraction
  quickCheck prop_division
  quickCheck prop_absoluteValue
  quickCheck prop_modulus
  quickCheck prop_power

