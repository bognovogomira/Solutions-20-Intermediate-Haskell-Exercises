-- https://blog.tmorris.net/posts/20-intermediate-haskell-exercises/

-- Parth's ratings are just based on how quickly I found the solution (unintended flex) 

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
-- Parth's Rating: 3 for given solution, 1 for recursive 
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
-- Parth's Rating: 1
instance Fluffy Maybe where
  furry func Nothing   = Nothing 
  furry func (Just a)  = Just (func a)

-- Exercise 3
-- Relative Difficulty: 5
-- Parth's Rating: 3 for given solution, 1 for recursive
instance Fluffy ((->) t) where
  furry = (.) 

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)
  
-- Exercise 4
-- Relative Difficulty: 5
-- Parth's Rating: 4
instance Fluffy (EitherLeft t) where
  furry func (EitherLeft (Left a) ) = EitherLeft (Left (func a))
  furry func (EitherLeft (Right a)) = EitherLeft (Right a)

-- Exercise 5
-- Relative Difficulty: 5
-- Parth's Rating: 0 (same as previous)
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Left a)) = EitherRight (Left a)
  furry f (EitherRight (Right a))= EitherRight (Right (f a))

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- Parth's Rating: 4
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
-- Parth's Rating: 6 for coming up with given solutions, 3 for recursive 
instance Misty [] where
  banana  = concatMap
  unicorn = flip (:) []

-- Exercise 8
-- Relative Difficulty: 2
-- Parth's Rating: 2
instance Misty Maybe where
  banana func Nothing   = Nothing
  banana func (Just a)  = func a
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
-- Parth's Rating: 4
instance Misty ((->) t) where
  banana f g x  = f (g x) x
  (unicorn a) t = a 

-- Exercise 10
-- Relative Difficulty: 6
-- Parth's Rating: 6
instance Misty (EitherLeft t) where
  banana func (EitherLeft(Left a)) = func a 
  banana func (EitherLeft(Right a))= EitherLeft(Right a)  
  unicorn a = EitherLeft(Left a)

-- Exercise 11
-- Relative Difficulty: 6
-- Parth's Rating: 0 (same as previous)
instance Misty (EitherRight t) where
  banana func (EitherRight(Left a)) = EitherRight (Left a) 
  banana func (EitherRight(Right a))= func a  
  unicorn a = EitherRight(Right a)

-- Exercise 12
-- Relative Difficulty: 3
-- Parth's Rating: 2
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id  

-- Exercise 13
-- Relative Difficulty: 6
-- Parth's Rating: 5
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x = banana func where func y = furry' y x 

-- Exercise 14
-- Relative Difficulty: 6
-- Parth's Rating: 9 for only using tools developed so far, 3 if done using banana2
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] f        = unicorn []
moppy (x:xs) f    = apple (f x) (furry' (flip (:)) (moppy xs f)) 

-- Exercise 15
-- Relative Difficulty: 6
-- Parth's Rating: 2
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = flip moppy id 

-- Exercise 16
-- Relative Difficulty: 6
-- Parth's Rating: 4
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 func x y = apple y (furry' func x)  

-- Exercise 17
-- Relative Difficulty: 6
-- Parth's Rating: 4
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 func x y z = apple z (banana2 func x y)  

-- Exercise 18
-- Relative Difficulty: 6
-- Parth's Rating: 0 (same as previous)
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 func x y z w = apple w (banana3 func x y z) 

newtype State s a = State {
  state :: s -> (s, a)
}

-- Exercise 19
-- Relative Difficulty: 9
-- Parth's Rating: 3 
instance Fluffy (State s) where
  furry func (State f) = State ((\(s,a)->(s,func a)).f)

-- Exercise 20
-- Relative Difficulty: 10
-- Parth's Rating: 5
instance Misty (State s) where
  banana func (State f) = State g where g x =(\(State t)-> t (fst (f x))) ((func.snd) (f x)) 
  unicorn a = State (\x->(x,a)) 