# Chess-Engine
Attempt to make an automatic chess opponent in R

It's not fast, it's not good, I'm half tempted to make a second chess.com account to test its ELO but I imagine it'll be around 300 or so (update on that: it stalemates against the easiest computer player. Not sure how it would play against a human of the same level. Nick absolutely thrashes it). 
But it does work in the barest sense of the term, and can do castling and en passant!

How it works: when it's your turn you will be presented with a list of possible moves. Type the row number (not piece ID!) into the console and it'll move the 
chosen piece to the chosen position. If you want to do something fancier either type in "O-O" for a king-side castle, "O-O-O" for a queen-side castle or "En Passant" followed by the file of the pawn to en passant (ex: "En Passant D" to en passant with the D pawn).


TO FIX:
- [x] castling bug 
- [x] weird error message crashes it sometimes?
- [ ] make it actually good lol


31/10/22 Edit:

Having my annual go at this. Now comes with unicode pieces! Will try and make it into a shiny app and host somewhere so it can be inflicted on you all more easily. 
