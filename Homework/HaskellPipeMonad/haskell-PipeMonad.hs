 {-# LANGUAGE InstanceSigs #-}

--   'PipedCalc' represents a staged or incremental calculation
--   that may depend on a certain number of integers being "piped in"
--   sequentially before it can produce a final result.
-- 
--   It has two cases:
-- 
--   * 'Result a' holds a final result when all necessary inputs have been provided.
--   * 'Waiting (Int -> PipedCalc a)' represents a suspended computation
--     waiting for the next integer input before it can either finish
--     or request another input.
--
--   This allows modeling pipelines of calculations where values are supplied
--   one at a time, and each input may advance the state of the computation.

data PipedCalc a
  = Result a
  | Waiting (Int -> PipedCalc a)

instance Show a => Show (PipedCalc a) where
  show (Result a)     = "Result " ++ show a
  show (Waiting _)    = "Waiting <function>"


instance Eq a => Eq (PipedCalc a) where
  (Result a) == (Result b) = a == b
  _ == _ = False

--  'send' takes a 'PipedCalc a' and an 'Int' input value,
--   and attempts to advance the computation by providing
--   that integer to the calculation.
--
--   If the 'PipedCalc' is already a 'Result', it remains unchanged.
--   If it is a 'Waiting' state, it applies the provided 'Int'
--   to the stored function, producing the next stage of the computation.
--
--   Example:
--
--   > let calc = Waiting (\x -> Result (x * 2))
--   > send calc 10  -- Result 20
--
send :: PipedCalc a -> Int -> PipedCalc a
send (Result val) _       = Result val
send (Waiting func) newSend = func newSend


--  'receive' is a simple utility representing a calculation
--   that waits for a single integer input and then immediately
--   returns it as the final result.
--
--   Essentially, it's a 'PipedCalc' waiting for one 'Int'.
--
--   Example:
--
--   > send receive 42  -- Result 42
--
receive :: PipedCalc Int
receive = Waiting Result

--   'concatWaiting' is a procedure that takes a procedure that
--   returns a PipedCalc result, and a procedure intended to
--   use that result and concats them.  Note that the order is
--   opposite the normal concat.  The result is a procedure
--   that takes the initial type and returns a PipedCalc
--   of the final type of the second proceure.
--
--   The only tricky case to note is that the result of applying
--   the procedure itself might be a waiting value.  In that case
--   recursion will help.

concatWaiting :: (a -> PipedCalc b) -> (b -> c) -> a -> PipedCalc c
concatWaiting f1 f2 val = undefined


--  Implementing functor is pretty straightforward if you have
--  concatWaiting.  I think it easier to do if rather than
--  write it as a procedure that takes a procedure and returns
--  1 parameter procedure, you write it as a procedure that
--  takes a procedure and a PipedCalc and returns and PipedCalc
instance Functor PipedCalc where
  fmap :: (a -> b) -> PipedCalc a -> PipedCalc b
  fmap f _ = undefined


-- For <*> cases where the function is a result just involve pulling
-- out the function like in Functor.  For cases where the function is
-- itself a waiting value, what you want to do is create a new waiting
-- that when the value comes it, it gets sent to the function and then
-- use the <*> recursively to see if you can make progress
instance Applicative PipedCalc where
  pure :: a -> PipedCalc a
  pure = undefined
  (<*>) :: PipedCalc (a -> b) -> PipedCalc a -> PipedCalc b
  _ <*> _ = undefined

-- very similar to <*>
instance Monad PipedCalc where
  (>>=) :: PipedCalc a -> (a -> PipedCalc b) -> PipedCalc b
  (>>=) = undefined

getN :: Int -> PipedCalc [Int]
getN 0 = Result []
getN n = do 
          h <- receive
          t <- getN (n - 1)
          return (h : t)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName actual expected
  | expected == actual = putStr "."
  | otherwise =
    do
      putStrLn $ testName ++ " FAILED"
      putStrLn $ "Expected: " ++ (show expected)
      putStrLn $ "Actual: " ++ (show actual)
      

addPipedValue :: Int -> PipedCalc Int
addPipedValue val = Waiting (\piped -> Result $ piped + val)

addThenSubtractPipedValue val = Waiting (\piped -> Waiting (\piped2 -> Result $ val + piped - piped2))



main :: IO ()
main = do

    -- concatWaiting +30
  let c1 = concatWaiting addPipedValue (+30) 100
  let c1' = send c1 5
  assertEqual "concatWaiting 1" c1' (Result 135)

  -- concatWaiting negate
  let c2 = concatWaiting addPipedValue negate 100
  let c2' = send c2 5
  assertEqual "concatWaiting 2" c2' (Result (-105))

  -- concatWaiting double waiting
  let c3 = concatWaiting addThenSubtractPipedValue (+30) 100
  let c3' = send c3 5
  let c3'' = send c3' 50
  
  assertEqual "concatWaiting 3" c3'' (Result 85)


  -- fmap with all results
  assertEqual "fmap 1" (fmap (+3) (Result 50)) (Result 53)

  -- fmap with a waiting value
  let f1 = fmap (+3) receive
  let f1' = send f1 50
  assertEqual "fmap 2" f1' (Result 53)

  -- fmap with a double wait
  let f2 = fmap (+3) (addThenSubtractPipedValue 50)
  let f2' = send f2 50
  let f2'' = send f2' 10
  assertEqual "fmap 3" f2'' (Result 93)

  -- Applicative tests - pure
  assertEqual "Applicative pure" (pure 42 :: PipedCalc Int) (Result 42)

    -- Applicative <*> with both Results
  let f1 = Result (+5)
  let v1 = Result 10
  assertEqual "<*> 1" (f1 <*> v1) (Result 15)
  -- Applicative <*> with function as Result and value Waiting
  let f2 = Result (+7)
  let v2 = receive  -- Waiting Int
  assertEqual "<*> 2"
    (send (f2 <*> v2) 3)
    (Result 10)

  -- Applicative <*> with function as Waiting and value as Result
  let f3 = Waiting (\subtractMe -> Result (\y -> y - subtractMe))
  let v3 = Result 8
  assertEqual "<*> 3"
    (send (f3 <*> v3) 4)
    (Result 4)

  -- Applicative <*> with both as Waiting
  let f3 = Waiting (\subtractMe -> Result (\y -> y - subtractMe))
  assertEqual "<*> 4"
    (send (send (f3 <*> receive) 4) 8)
    (Result 4)

  -- Applicative <*> convert multiparam func
  assertEqual "<*> 5"
    (pure (+) <*> Result 3 <*> Result 4)
    (Result 7)
  
  -- Monad >>= with Result
  let m1 = Result 10
  let f1 = \x -> Result (x + 5)
  assertEqual "Monad Result >>= Result" (m1 >>= f1) (Result 15)

  -- Monad >>= with Result into Waiting
  let f2 = \x -> Waiting (\y -> Result (x + y))
  assertEqual "Monad Result >>= Waiting"
    (send (m1 >>= f2) 7)
    (Result 17)

  -- Monad >>= with Waiting into Result
  let m2 = Waiting (\x -> Result (x * 3))
  let f3 = \y -> Result (y + 2)
  assertEqual "Monad Waiting >>= Result"
    (send (m2 >>= f3) 4)
    (Result 14)

  -- Monad >>= chaining two Waitings
  let m3 = Waiting (\x -> Result (x * 2))
  let f4 = \y -> Waiting (\z -> Result (y + z))
  let step3 = send (m3 >>= f4) 5
  let step4 = send step3 7
  assertEqual "Monad Waiting >>= Waiting"
    step4
    (Result 17)

  -- Test getN with 0 inputs
  assertEqual "getN 0" (getN 0) (Result [])

  -- Test getN with 3 inputs
  let g1 = getN 3
  let g2 = send g1 5
  let g3 = send g2 10
  let g4 = send g3 15
  assertEqual "getN 3 inputs"
    g4
    (Result [5,10,15])
  
  
  
  
  
