library(animation)
library(schoolmath)

# pieces set up ---------------------------------------------------------------

activePieces = setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Type", "XLoc", "YLoc", "Colour", "Moves", "Taken"))

X_Positions = 1:8
Y_Positions = 1:8

pawns = setNames(data.frame(matrix(ncol = 6, nrow = 16)), c("Type", "XLoc", "YLoc", "Colour", "Moves", "Taken"))
pawns$Type = "Pawn"
pawns$XLoc = X_Positions
pawns$YLoc = c(rep(2,8), rep(7,8))
pawns$Colour = c(rep("White", 8), rep("Black", 8))
pawns$Moves = 0
pawns$Taken = 0

bishops = setNames(data.frame(matrix(ncol = 6, nrow = 4)), c("Type", "XLoc", "YLoc", "Colour", "Moves", "Taken"))
bishops$Type = "Bishop"
bishops$XLoc = c(3,6,3,6)
bishops$YLoc = c(1,1,8,8)
bishops$Colour = c("White", "White", "Black", "Black")
bishops$Moves = 0
bishops$Taken = 0

rooks = setNames(data.frame(matrix(ncol = 6, nrow = 4)), c("Type", "XLoc", "YLoc", "Colour", "Moves", "Taken"))
rooks$Type = "Rook"
rooks$XLoc = c(1,8,1,8)
rooks$YLoc = c(1,1,8,8)
rooks$Colour = c("White", "White", "Black", "Black")
rooks$Moves = 0
rooks$Taken = 0

knights = setNames(data.frame(matrix(ncol = 6, nrow = 4)), c("Type", "XLoc", "YLoc", "Colour", "Moves", "Taken"))
knights$Type = "Night"
knights$XLoc = c(2,7,2,7)
knights$YLoc = c(1,1,8,8)
knights$Colour = c("White", "White", "Black", "Black")
knights$Moves = 0
knights$Taken = 0

kings = setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Type", "XLoc", "YLoc", "Colour", "Moves", "Taken"))
kings$Type = "King"
kings$XLoc = 5
kings$YLoc = c(1,8)
kings$Colour = c("White", "Black")
kings$Moves = 0
kings$Taken = 0

queens = setNames(data.frame(matrix(ncol = 6, nrow = 2)), c("Type", "XLoc", "YLoc", "Colour", "Moves", "Taken"))
queens$Type = c("Queen", "Queen")
queens$XLoc = 4
queens$YLoc = c(1,8)
queens$Colour = c("White", "Black")
queens$Moves = 0
queens$Taken = 0

activePieces = rbind(activePieces, pawns, rooks, knights, bishops, kings, queens)


wPMoves = list(c(0,1), c(0,2), c(1,1), c(-1,1))
bPMoves = list(c(0,-1), c(0,-2), c(1,-1), c(-1,-1))
BMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1))
RMoves = list(c(1,0), c(-1,0), c(0,1), c(0,-1))
NMoves = list(c(1,2), c(-1,2), c(1,-2), c(-1,-2), c(2,1), c(2,-1),c(-2,1), c(-2,-1))
QMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))
KMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))


# Max moves in direction ---------------------------------------------------------

maxMovesFun = function(piece, activePieces, pieceCol, chosenMove, multiplier){
  if(piece$Type != "Pawn"){      
    for(n in multiplier){
      if((piece$XLoc + chosenMove[1]*n) > max(X_Positions) || 
         (piece$XLoc + chosenMove[1]*n) < min(X_Positions) ||
         (piece$YLoc + chosenMove[2]*n) > max(Y_Positions) ||
         (piece$YLoc + chosenMove[2]*n) < min(Y_Positions)||
         nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*n) &
                           activePieces$YLoc == (piece$YLoc + chosenMove[2]*n) &
                           activePieces$Colour == pieceCol,])>0){
        maxMoves = n-1
        break()
        
      }             
      
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*n) &
                                activePieces$YLoc == (piece$YLoc + chosenMove[2]*n) &
                                activePieces$Colour != pieceCol,])==1){
        
        maxMoves = n
        break()
      }
      
      else{
        maxMoves = n
      }
      
      
    }
  }
  
  if(piece$Type == "Pawn"){
    for(n in multiplier){
      if(chosenMove[2] == 2 || chosenMove[2] == -2){
        
        if(piece$Moves > 0 ){
          maxMoves = 0 
          break()
        }
        
        else if((piece$XLoc + chosenMove[1]*n) > max(X_Positions) || 
           (piece$XLoc + chosenMove[1]*n) < min(X_Positions) ||
           (piece$YLoc + chosenMove[2]*n) > max(Y_Positions) ||
           (piece$YLoc + chosenMove[2]*n) < min(Y_Positions)||
           nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*n) &
                             activePieces$YLoc == (piece$YLoc + chosenMove[2]*n) &
                             activePieces$Colour == pieceCol,]) ==1 ||
           nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*0.5*n) &
                             activePieces$YLoc == (piece$YLoc + chosenMove[2]*0.5*n) &
                             activePieces$Colour == pieceCol,]) ==1 ||
           nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*n) &
                             activePieces$YLoc == (piece$YLoc + chosenMove[2]*n) &
                             activePieces$Colour != pieceCol,]) ==1 ||
           nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*0.5*n) &
                             activePieces$YLoc == (piece$YLoc + chosenMove[2]*0.5*n) &
                             activePieces$Colour != pieceCol,]) ==1 )
        {
          maxMoves = 0
          break()
          
        }             
        else{
          maxMoves = n
          break()
        }
      }
      
      else if((piece$XLoc + chosenMove[1]*n) > max(X_Positions) || 
         (piece$XLoc + chosenMove[1]*n) < min(X_Positions) ||
         (piece$YLoc + chosenMove[2]*n) > max(Y_Positions) ||
         (piece$YLoc + chosenMove[2]*n) < min(Y_Positions)||
         nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*n) &
                           activePieces$YLoc == (piece$YLoc + chosenMove[2]*n) &
                           activePieces$Colour == pieceCol,]) == 1){
        maxMoves = 0
        break()
        
      }             
      
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*n) &
                                activePieces$YLoc == (piece$YLoc + chosenMove[2]*n) &
                                activePieces$Colour != pieceCol & chosenMove[1] != 0,])==1){
        
        maxMoves = 1
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + chosenMove[1]*n) &
                                activePieces$YLoc == (piece$YLoc + chosenMove[2]*n) &
                                activePieces$Colour != pieceCol & chosenMove[1] == 0,])==1){
        
        maxMoves = 0
      }
      
      else{
        maxMoves = 1
      }
      
      
    }
  }
  
  return(maxMoves)
}

#Am I in check? --------------------------------------------------------------------

inCheck = function(activePieces, pieceCol){
  wPMoves = list(c(0,1), c(0,2), c(1,1), c(-1,1))
  bPMoves = list(c(0,-1), c(0,-2), c(1,-1), c(-1,-1))
  BMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1))
  RMoves = list(c(1,0), c(-1,0), c(0,1), c(0,-1))
  NMoves = list(c(1,2), c(-1,2), c(1,-2), c(-1,-2), c(2,1), c(2,-1),c(-2,1), c(-2,-1))
  QMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))
  
  king = activePieces[activePieces$Type == "King" & activePieces$Colour == pieceCol,]
  Threats = 0
  #check for straight line checks
  
  for(x in 1:4){
    for(w in 1:8){
      if(nrow(activePieces[activePieces$XLoc == (king$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + RMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Rook",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + RMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Queen",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + RMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "Queen" & activePieces$Type != "Rook",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + RMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
  }
  
  #check for diagonal checks

  for(x in 1:4){
    for(w in 1:8){
      if(nrow(activePieces[activePieces$XLoc == (king$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + BMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Bishop",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + BMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type == "Queen",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + BMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "Queen" & activePieces$Type != "Bishop",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + BMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
  }
  
  #check for knight checks
  for(x in 1:8){
    for(w in 1:1){
      if(nrow(activePieces[activePieces$XLoc == (king$XLoc + NMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + NMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Night",] ) == 1 ){
        Threats = Threats + 1
      }

      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + NMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + NMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "Night",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + NMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + NMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
    
    
  }
  
  #check for pawn checks
  
  if(pieceCol == "White"){pawnMates = bPMoves}
  if(pieceCol == "Black"){pawnMates = wPMoves}
  
  for(x in 3:4){
    for(w in 1:1){
      if(nrow(activePieces[activePieces$XLoc == (king$XLoc + pawnMates[[x]][1]*w) & activePieces$YLoc == (king$YLoc + pawnMates[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Pawn",] ) == 1 ){
        Threats = Threats + 1
      }
      
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + pawnMates[[x]][1]*w) & activePieces$YLoc == (king$YLoc + pawnMates[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "Pawn",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + pawnMates[[x]][1]*w) & activePieces$YLoc == (king$YLoc + pawnMates[[x]][2]*w) & 
                                activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
  }
  if(Threats > 0){return(TRUE)}
    if(Threats == 0){return(FALSE)}
}

# does this move capture anything?  -----------------------------------------------

moveScore = function(activePieces, piece, pieceCol, y, z){
  points = 0
  if(piece$Type == "Pawn" & y[1] == 0){points = 0}
  else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + y[1]*z) &
                            activePieces$YLoc == (piece$YLoc + y[2]*z) &
                            activePieces$Colour != pieceCol,])==1){
    
    takenPiece = activePieces[activePieces$XLoc == (piece$XLoc + y[1]*z) &
                   activePieces$YLoc == (piece$YLoc + y[2]*z) &
                   activePieces$Colour != pieceCol,]
    
    if(takenPiece$Type == "Pawn"){points = 1}
    if(takenPiece$Type == "Night" || takenPiece$Type == "Bishop"){points = 3}
    if(takenPiece$Type == "Rook"){points = 5}
    if(takenPiece$Type == "Queen"){points = 9}
    if(takenPiece$Type == "King"){points = 100}
    
  }
  else{}
  
  activePieces[match(rownames(piece), rownames(activePieces)),]$XLoc = piece$XLoc + y[1]*z
  activePieces[match(rownames(piece), rownames(activePieces)),]$YLoc = piece$YLoc + y[2]*z
  
  if(inCheck(activePieces, pieceCol) == TRUE){points = -100}
  else{}
  
  return(points)
}

# Loop -----------------------------------------------------------------------------

ani.options(interval=1)
saveGIF({ par(bg = "grey")
  i = 1
  
  while(nrow(activePieces[activePieces$Type == "King",])>1){
    if(is.even(i) == F){pieceCol = "White"}
    else{pieceCol = "Black"}
    #cn = sample(rownames(activePieces[activePieces$Colour == pieceCol,]),1)
    evalTable = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("PieceID", "Move", "NoSpaces", "Points"))
    
    
    for(cn in rownames(activePieces[activePieces$Colour == pieceCol,])){
      
    piece = activePieces[match(cn,rownames(activePieces)),]
    
    #pawn possible moves
    #when attack is possible
  
    
      if(piece$Type == "Pawn" & pieceCol == "White"){
      multiplier = 1:1
      if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + 1) &
                           activePieces$YLoc == (piece$YLoc + 1) & activePieces$Colour != pieceCol,])==1){
        if(piece$Moves == 0){moves = wPMoves[c(1,2,3)]}
        else{moves = wPMoves[c(1,3)]}
      }
      
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc - 1) &
                                activePieces$YLoc == (piece$YLoc + 1) & activePieces$Colour != pieceCol,])==1){
        if(piece$Moves == 0){moves = wPMoves[c(1,2,4)]}  #if first move, move 1 or 2 forwards
        else{moves = wPMoves[c(1,4)]}
      }
      else{
        if(piece$Moves ==0){moves = wPMoves[c(1,2)]}
        else{moves = wPMoves[1]}
      } 
    }
      if(piece$Type == "Pawn" & pieceCol == "Black"){
      multiplier = 1:1
      if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + 1) &
                           activePieces$YLoc == (piece$YLoc - 1) & activePieces$Colour != pieceCol,])==1){
        if(piece$Moves == 0){moves = bPMoves[c(1,2,3)]}
        else{moves = bPMoves[c(1,3)]}
      }
      
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc - 1) &
                                activePieces$YLoc == (piece$YLoc - 1) & activePieces$Colour != pieceCol,])==1){
        if(piece$Moves == 0){moves = bPMoves[c(1,2,4)]}
        else{moves = bPMoves[c(1,4)]}
      }
      else{
        if(piece$Moves ==0){moves = bPMoves[c(1,2)]}
        else{moves = bPMoves[1]}
      } 
    }
      if(piece$Type == "Bishop"){moves = BMoves
    multiplier = 1:8}
      if(piece$Type == "Rook"){moves = RMoves
    multiplier = 1:8}
      if(piece$Type == "Night"){moves = NMoves
    multiplier = 1:1}
      if(piece$Type == "Queen"){moves = QMoves
    multiplier = 1:8}
      if(piece$Type == "King"){moves = KMoves
    multiplier = 1:1}
    
    for(y in 1:length(moves)){      
      
      chosenMove = moves[[y]]

      maxMoves = maxMovesFun(piece, activePieces, pieceCol, chosenMove, multiplier)
      if(piece$Type == "Pawn" & piece$Moves > 0 & y == 2){}
      else if(maxMoves > 0){
        
        
        evalTable2 = setNames(data.frame(matrix(ncol = 4, nrow = 1)), c("PieceID", "Move", "NoSpaces", "Points"))
        
        evalTable2$PieceID = cn
        evalTable2$Move = y
        evalTable2$NoSpaces = maxMoves
        evalTable2$Points = moveScore(activePieces, piece, pieceCol, chosenMove, maxMoves)
        
        evalTable = rbind(evalTable, evalTable2)
        
        
        }
      else{}
  

      }
    
    }
       
    evalTable = evalTable[order(-evalTable$Points),]
    bestMove = head(evalTable, 1)
    if(bestMove$Points == 0){
      bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0,]), 1),]
    }
    
    chosenPiece = activePieces[match(bestMove$PieceID, rownames(activePieces)),]
    
    if(chosenPiece$Type == "Pawn" & pieceCol == "White"){nmoves = wPMoves}
    if(chosenPiece$Type == "Pawn" & pieceCol == "Black"){nmoves = bPMoves}
    if(chosenPiece$Type == "Bishop"){nmoves = BMoves}
    if(chosenPiece$Type == "Rook"){nmoves = RMoves}
    if(chosenPiece$Type == "Night"){nmoves = NMoves}
    if(chosenPiece$Type == "Queen"){nmoves = QMoves}
    if(chosenPiece$Type == "King"){nmoves = KMoves}
    
    noMoves = bestMove$NoSpaces
    nextMove = nmoves[[bestMove$Move]]
    
    activePieces$XLoc[match(bestMove$PieceID,rownames(activePieces))] = chosenPiece$XLoc + nextMove[1]*noMoves
    activePieces$YLoc[match(bestMove$PieceID,rownames(activePieces))] = chosenPiece$YLoc + nextMove[2]*noMoves
    activePieces$Moves[match(bestMove$PieceID,rownames(activePieces))] = chosenPiece$Moves + 1
    

      activePieces$Taken[activePieces$XLoc == activePieces$XLoc[match(bestMove$PieceID,rownames(activePieces))] &
                           activePieces$Colour != pieceCol &
                     activePieces$YLoc == activePieces$YLoc[match(bestMove$PieceID,rownames(activePieces))]] = 1
      activePieces = activePieces[activePieces$Taken == 0,]
    
    
                
                  
    activePieces$Type[activePieces$Type == "Pawn" & activePieces$YLoc == 8 & activePieces$Colour == "White"] = "Queen"
    activePieces$Type[activePieces$Type == "Pawn" & activePieces$YLoc == 1 & activePieces$Colour == "Black"] = "Queen"
    
    if(nrow(activePieces[activePieces$Type == "King",]) < 2){
      print(paste(pieceCol, "wins!"))
    }
    
    i = i + 1
    
    #plot board
    plot(1, type="n", xlab="", ylab="", xlim=c(0.5, 8.5), ylim=c(0.5, 8.5))
    for(j in c(1,3,5,7)){
      for(k in c(1,3,5,7)){
        rect(j-0.5, k - 0.5, j +0.5, k+0.5, col = "darkgrey", border = NA)
      }
    }
    for(l in c(2,4,6,8)){
      for(m in c(2,4,6,8)){
        rect(l-0.5, m - 0.5, l +0.5, m+0.5, col = "darkgrey", border = NA)
      }
    }
    
    points(data = activePieces, YLoc ~ XLoc, pch = Type, col = Colour)
    
  }

}, interval = 0.5, movie.name = "chess.gif", ani.width = 500, ani.height = 500)