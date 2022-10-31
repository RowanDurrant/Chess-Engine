activePieces = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Type", "XLoc", "YLoc", "Colour"))

X_Positions = 1:8
Y_Positions = 1:8

pawns = setNames(data.frame(matrix(ncol = 4, nrow = 16)), c("Type", "XLoc", "YLoc", "Colour"))
pawns$Type = "Pawn"
pawns$XLoc = X_Positions
pawns$YLoc = c(rep(2,8), rep(7,8))
pawns$Colour = c(rep("White", 8), rep("Black", 8))


bishops = setNames(data.frame(matrix(ncol = 4, nrow = 4)), c("Type", "XLoc", "YLoc", "Colour"))
bishops$Type = "Bishop"
bishops$XLoc = c(3,6,3,6)
bishops$YLoc = c(1,1,8,8)
bishops$Colour = c("White", "White", "Black", "Black")


rooks = setNames(data.frame(matrix(ncol = 4, nrow = 4)), c("Type", "XLoc", "YLoc", "Colour"))
rooks$Type = "Rook"
rooks$XLoc = c(1,8,1,8)
rooks$YLoc = c(1,1,8,8)
rooks$Colour = c("White", "White", "Black", "Black")


knights = setNames(data.frame(matrix(ncol = 4, nrow = 4)), c("Type", "XLoc", "YLoc", "Colour"))
knights$Type = "Night"
knights$XLoc = c(2,7,2,7)
knights$YLoc = c(1,1,8,8)
knights$Colour = c("White", "White", "Black", "Black")


kings = setNames(data.frame(matrix(ncol = 4, nrow = 2)), c("Type", "XLoc", "YLoc", "Colour"))
kings$Type = "King"
kings$XLoc = 5
kings$YLoc = c(1,8)
kings$Colour = c("White", "Black")


queens = setNames(data.frame(matrix(ncol = 4, nrow = 2)), c("Type", "XLoc", "YLoc", "Colour"))
queens$Type = c("Queen", "Queen")
queens$XLoc = 4
queens$YLoc = c(1,8)
queens$Colour = c("White", "Black")


activePieces = rbind(activePieces, pawns, rooks, knights, bishops, kings, queens)
activePieces$Moves = 0
activePieces$Taken = 0
activePieces$killCount = 0

wPMoves = list(c(0,1), c(0,2), c(1,1), c(-1,1))
bPMoves = list(c(0,-1), c(0,-2), c(1,-1), c(-1,-1))
BMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1))
RMoves = list(c(1,0), c(-1,0), c(0,1), c(0,-1))
NMoves = list(c(1,2), c(-1,2), c(1,-2), c(-1,-2), c(2,1), c(2,-1),c(-2,1), c(-2,-1))
QMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))
KMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))

