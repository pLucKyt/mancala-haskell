module GameMonad where

import Mancala

-- GameStateOp must have one parameter, because Functors and Monads accept a type of kind * -> *
newtype GameStateOp a = GameStateOp {runGameState :: GameState -> (a, GameState)}

-- this function returns a recipe on how to perform the given move
-- since it returns the recipe, nothing is ever computed until the recipe is called/used
-- actually the recipe is the field in the GameStateOp and we precisely return GameStateOp
applyMove :: Int -> GameStateOp Bool
applyMove k = GameStateOp (\gs -> let gsNew = playMove k gs in (isGameOver gsNew, gsNew))

-- since we want to return a GameStateOp, we basically must return a function and since we are returning a function, we are not producing anything, the only thing we are doing is writing
-- the rule in which our function will return something when actually called, so we use a lambda, where the x is an input that is going to be used somewhere and the rest is just the blueprint
{- HLINT ignore "Use first" -}
instance Functor GameStateOp where
    fmap :: (a -> b) -> GameStateOp a -> GameStateOp b
    fmap f gsOp = GameStateOp (\gs -> let fOp = runGameState gsOp in (f (fst (fOp gs)), snd (fOp gs)))


-- pure is used just to implement a request where we return a result without changing the game state, so basically constructing GameStateOp where for every single gs presented as an argument
-- to the lambda, the result will be a tuple (c, argument_gs), where argument_gs is the argument in the lambda, and c is a const, regardless of what the argument in the lambda is
-- complex as <*> is, we can still try to write a simple explanation
-- the whole point is to chain multiple GameStateOp in a sense that their function which they keep is applied properly
-- so since implementing <*> is for a base case chaining of two calls, the first GameStateOp (a -> b) is called with the lambda argument gs(which is the input of the whole chain basically), then
-- we can see that it's result produces (a -> b, GameState), so the first element is the blueprint for constructing at the end and the second element is gonna be used in the
-- second GameStateOp a, which is called in the same manner, but it's input argument is the thing from the first GameStateOp, and so now we have to form the output of this function,
-- which we do by simply make the tuple, so that the first element is the function (a -> b) that gets handed the argument from the output of the second GameStateOp and the second element is
-- the end state which is located in the second element of the output of the second GameStateOp
{- HLINT ignore "Use tuple-section" -}
{- HLINT ignore "Redundant bracket" -}
instance Applicative GameStateOp where
    pure :: a -> GameStateOp a
    pure t = GameStateOp (\gs -> (t, gs))
    (<*>) :: GameStateOp (a -> b) -> GameStateOp a -> GameStateOp b
    (<*>) gsOp1 gsOp2 = GameStateOp (\gs -> let fOp1 = runGameState gsOp1; rOp1 = fOp1 gs; fOp2 = runGameState gsOp2; rOp2 = fOp2 (snd rOp1) in ((fst rOp1) (fst rOp2), snd rOp2))
-- gsOp1 produces a function, gsOp2 the argument, the result is the application
-- gsOp2 is used as-is, meaning gsOp1's result is only needed to land in the output


-- the >>= is a little less complex than <*>, but it still needs careful explaining
-- the first argument is a GameStateOp with an argument a, for which we are gonna apply it's function with the input gs and that is our first step
-- in order for us to compute the second step, meaning to finish this chain, we have to first apply the given function to the first element of the previous result
-- with this we have the GameStateOp which when applied will return us the output that we want, and we apply it with the second element of the previous result
instance Monad GameStateOp where
    (>>=) :: GameStateOp a -> (a -> GameStateOp b) -> GameStateOp b                                               -- <=> t <- op, t gets value from f (fst rOp)
    (>>=) gsOp f = GameStateOp (\gs -> let fOp = runGameState gsOp; rOp = fOp gs; fOpEnd = runGameState (f (fst rOp)); rOpEnd = fOpEnd (snd rOp) in rOpEnd)
-- the 2nd argument is a function which builds an Op based on the result of Op(the 1st argument)
-- fOpEnd gets constructed by applying stuff to the function f
-- because f (fst rOp) is a function we can decide what's gonna happen with the step 2, meaning we can ask what happened and based on that guide if we continue or return a pure t at step 2


-- compile time
applyMoves :: GameStateOp Bool
applyMoves = do
            applyMove 2
            applyMove 4
            applyMove 8
            applyMove 11

applyMovesButBetter :: [Int] -> GameStateOp Bool
applyMovesButBetter [] = GameStateOp (\gs -> (isGameOver gs, gs))
applyMovesButBetter (x:xs) = do
                            applyMove x
                            applyMovesButBetter xs


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- same concept as before, but now instead of storing a single state, we store every created state, in reverse order of their arrival
-- so if we played moves which results in states a b c d e, in the list they are stored from newest like [e d c b a]
newtype GameStateOpH a = GameStateOpH {runGameStateH :: [GameState] -> (a, [GameState])}


{- HLINT ignore "Use :" -}
applyMoveH :: Int -> GameStateOpH Bool
applyMoveH k = GameStateOpH (\gss -> let newGs = playMove k (head gss) in (isGameOver newGs, [newGs] ++ gss))


instance Functor GameStateOpH where
    fmap :: (a -> b) -> GameStateOpH a -> GameStateOpH b
    fmap f gssOpH = GameStateOpH (\gss -> let fOpH = runGameStateH gssOpH; rOpH = fOpH gss in (f (fst rOpH), snd rOpH))


instance Applicative GameStateOpH where
    pure :: a -> GameStateOpH a
    pure t = GameStateOpH (\gss -> (t, gss))
    (<*>) :: GameStateOpH (a -> b) -> GameStateOpH a -> GameStateOpH b
    (<*>) gsOpH1 gsOpH2 = GameStateOpH (\gss -> let fOpH1 = runGameStateH gsOpH1; rOpH1 = fOpH1 gss; fOpH2 = runGameStateH gsOpH2; rOpH2 = fOpH2 (snd rOpH1) in ((fst rOpH1) (fst rOpH2), snd rOpH2))


instance Monad GameStateOpH where
    (>>=) :: GameStateOpH a -> (a -> GameStateOpH b) -> GameStateOpH b
    (>>=) gsOpH f = GameStateOpH (\gss -> let fOpH = runGameStateH gsOpH; rOpH = fOpH gss; fOpHEnd = runGameStateH (f (fst rOpH)); rOpHEnd = fOpHEnd (snd rOpH) in rOpHEnd)


-- initialize returns GameStateOpH Bool, in a clever way that it's inner function which we use doesn't care for the argument, because it will always return the thing we wrote below
-- but this also means that it is mandatory for us to be able to apply it to give it any argument of [GameState], meaning given [], [randomGs], [randomGs1, randomGs2] it will always return the same
-- the use of this is to prevent applyMoveH from crashing on an empty list []
{- HLINT ignore "Use const" -}
initialize :: GameStateOpH Bool
initialize = GameStateOpH (\_ -> (isGameOver mancalaInitialState, [mancalaInitialState]))


-- compile time
-- still to apply the result of this(the function inside GameStateOpH Bool), we must present an argument, for example [] or any other [...], it doesn't matter
-- this function is built in a way that whatever state argument we pass as the start [...] it will always start from mancalaInitialState which is seeded in the initialize
applyMovesH :: GameStateOpH Bool
applyMovesH = do
            initialize
            applyMoveH 2
            applyMoveH 4
            applyMoveH 8
            applyMoveH 11

-- if [Int] argument is [], then the applied argument to the function can be anything, since initialize is called
-- this function has the ability to apply the moves on any given starting state, but we must seed it ourselves with [someGs], since the initialize only is used to prevent
-- an error arising from giving it an empty list of moves
applyMovesHButBetter :: [Int] -> GameStateOpH Bool
applyMovesHButBetter [] = initialize
applyMovesHButBetter [x] = applyMoveH x -- most important part here, it separates the edge case [] call from the normal use, because at the end xs = [] overwrites the whole result with initialize
applyMovesHButBetter (x:xs) = do
                            applyMoveH x
                            applyMovesHButBetter xs

