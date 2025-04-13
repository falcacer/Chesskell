# Chesskell: A Haskell Chess Validator
Chesskell is an embedded domain-specific language (EDSL) written in Haskell that validates chess moves and provides a simple chess engine. It parses a custom chess notation, validates moves according to chess rules, and tracks game state including check detection.
## Architecture
The project is organized into several modules:

- Common.hs: Core data types and chess piece implementations
- Eval.hs: Move evaluation and execution logic
- Monads.hs: Chess-specific monads for state management and error handling
- Parse.y: Parser for the chess notation (using Happy parser generator)
- PrettyPrint.hs: Board visualization with Unicode chess pieces
- Utils.hs: Utility functions and board initialization

## Chess Notation
Chesskell uses a custom notation for chess moves:
- Move: ```p-e2>e4``` (piece at e2 moves to e4).
- Capture: ```pxd7>d8``` (piece at d7 capture piece at d5).
- Promotion: ```p-f8=Q``` (piece at f8 promotes to Queen).
- Multiple moves are separeted by semicolons: ```p-e2>e4; P-e7>e5; n-g1>f3```.

| Piece Name | EDSL Character (White/Black) | Unicode Representation (White/Black) |
|------------|------------------------------|-------------------------------------|
| King       | k / K                        | ♚ / ♔                              |
| Queen      | q / Q                        | ♛ / ♕                              |
| Rook       | r / R                        | ♜ / ♖                              |
| Bishop     | s / S                        | ♝ / ♗                              |
| Knight     | n / N                        | ♞ / ♘                              |
| Pawn       | p / P                        | ♟ / ♙                              |

## Building and Running
Prerequisites
- GHC.
- Stack.

Build:

``` stack build ```

Run:

``` stack exec Chesskell-exe [file.txt] ```

Where ```move_file.txt``` contains chess moves in the notation described above.

