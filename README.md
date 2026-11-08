# Mancala in Haskell

Mancala with a computer opponent. Course project for functional programming.

- `Mancala.hs` is the board and the rules
- `GameMonad.hs` is a state monad written from scratch instead of using mtl
- `Rose.hs` is a rose tree
- `Minimax.hs` is a minimax search over that tree, for the computer player
- `Draw.hs` and `Crtanje.hs` draw the board
- `PvP.hs` and `PvE.hs` are the two game modes, each with its own `main`

## Running

Compile the mode you want, for example `ghc PvE.hs` and run it, or load it in GHCi
and call `main`.
