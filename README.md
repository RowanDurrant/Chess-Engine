# Chess-Engine
Attempt to make an automatic chess opponent in R

It's not fast, it's not good, I'm half tempted to make a second chess.com account to test it's ELO but I imagine it'll be around 300 or so. 
But it does work in the barest sense of the term, and can do castling and en passant!

How it works: when it's your turn you will be presented with a list of possible moves. Type the row number (not piece ID!) into the console and it'll move the 
chosen piece to the chosen position. If you want to do something fancier either type in "O-O" for a king-side castle, "O-O-O" for a queen-side castle or "En Passant" followed
by the file of the pawn to en passant (ex: "En Passant D" to en passant with the D pawn).

choose_black_or_white.R does what it says on the tin and is mostly for ELO testing purposes, complete_with_en_passant.R lets you play as white every time
