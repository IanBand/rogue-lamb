# Rogue Lamb
A small roguelike for the purpose of learning haskell

The ultimate goal of the project is to have an engine with the same rough capability as a gen 1-2 pokemon game; including a 2d tile-based overworld, menu/bag system, turn based battles, and an advancing story. Additionally, a goal is to keep the project modular enough so that the visual data and rendering methods may be rewritten in the future to work with another API.

## Development notes
Test with ``` cabal repl app/Main.hs ```. This runs it in ghci, use :r to reload modules. 