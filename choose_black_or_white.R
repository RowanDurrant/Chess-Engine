library(animation)
library(schoolmath)

# pieces set up ---------------------------------------------------------------

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
  KMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))
  
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
    if(nrow(activePieces[activePieces$XLoc == (king$XLoc - pawnMates[[x]][1]) & activePieces$YLoc == (king$YLoc - pawnMates[[x]][2]) & 
                         activePieces$Colour != pieceCol & activePieces$Type == "Pawn",] ) == 1 ){
      Threats = Threats + 1
    }
    
    else if(nrow(activePieces[activePieces$XLoc == (king$XLoc - pawnMates[[x]][1]) & activePieces$YLoc == (king$YLoc - pawnMates[[x]][2]) & 
                              activePieces$Colour != pieceCol & activePieces$Type != "Pawn",]) == 1 ){
      break()
    }
    else if(nrow(activePieces[activePieces$XLoc == (king$XLoc - pawnMates[[x]][1]) & activePieces$YLoc == (king$YLoc - pawnMates[[x]][2]) & 
                              activePieces$Colour == pieceCol,]) == 1){
      break()
    }
    else{}  
    
  }
  
  #dont let kings get within 1 square
  
  for(x in 1:8){
    for(w in 1:1){
      if(nrow(activePieces[activePieces$XLoc == (king$XLoc + KMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + KMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "King",] ) == 1 ){
        Threats = Threats + 1
      }
      
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + KMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + KMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "King",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (king$XLoc + KMoves[[x]][1]*w) & activePieces$YLoc == (king$YLoc + KMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
  }
  
  if(Threats > 0){return(TRUE)}
  if(Threats == 0){return(FALSE)}
}

# does this move capture anything?  -----------------------------------------------

moveScore = function(activePieces, piece, pieceCol, opponent, y, z){
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
  activePieces$Taken[activePieces$XLoc == piece$XLoc+ y[1]*z &
                       activePieces$Colour != pieceCol &
                       activePieces$YLoc == piece$YLoc+ y[2]*z] = 1
  activePieces = activePieces[activePieces$Taken == 0,]
  
  if(inCheck(activePieces, opponent) == TRUE){points = points + 5}
  if(threatened(activePieces, piece, pieceCol ) == TRUE){
    if(piece$Type == "Pawn"){points = points - 1}
    if(piece$Type == "Night" || piece$Type == "Bishop"){points = points - 3}
    if(piece$Type == "Rook"){points = points - 5}
    if(piece$Type == "Queen"){points = points - 9}
  }
  if(inCheck(activePieces, pieceCol) == TRUE){points = -100}
  else{}
  
  return(points)
}

# can i castle? --------------------------------------------------------------------

canCastleKingside = function(activePieces, pieceCol){
  if(pieceCol == "White"){
    if(activePieces[activePieces$Type == "King" & activePieces$Colour == "White",]$Moves == 0 &
       nrow(activePieces[activePieces$Type == "Rook" & activePieces$Colour == "White" & activePieces$XLoc == 8
                         & activePieces$Moves == 0,]) == 1 &
       nrow(activePieces[activePieces$XLoc == 6 & activePieces$YLoc == 1,]) == 0 &
       nrow(activePieces[activePieces$XLoc == 7 & activePieces$YLoc == 1,]) == 0){
      
      activePieces[activePieces$Type == "King" & activePieces$Colour == "White",]$XLoc = 7
      activePieces[activePieces$Type == "Rook" & activePieces$Colour == "White" & activePieces$XLoc == 8,]$XLoc = 6
      if(inCheck(activePieces, pieceCol) == TRUE){return(FALSE)}
      if(inCheck(activePieces, pieceCol) == FALSE){return(TRUE)}
    }
    else{return(FALSE)}
  }
  
  if(pieceCol == "Black"){
    if(activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$Moves == 0 &
       nrow(activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 8
                         & activePieces$Moves == 0,]) == 1 &
       nrow(activePieces[activePieces$XLoc == 6 & activePieces$YLoc == 8,]) == 0 &
       nrow(activePieces[activePieces$XLoc == 7 & activePieces$YLoc == 8,]) == 0){
      
      activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 7
      activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 8,]$XLoc = 6
      if(inCheck(activePieces, pieceCol) == TRUE){return(FALSE)}
      if(inCheck(activePieces, pieceCol) == FALSE){return(TRUE)}
    }
    else{return(FALSE)}
  }
  
}

canCastleQueenside = function(activePieces, pieceCol){
  if(pieceCol == "White"){
    if(activePieces[activePieces$Type == "King" & activePieces$Colour == "White",]$Moves == 0 &
       nrow(activePieces[activePieces$Type == "Rook" & activePieces$Colour == "White" & activePieces$XLoc == 1 
                         & activePieces$Moves == 0,]) == 1 &
       nrow(activePieces[activePieces$XLoc == 2 & activePieces$YLoc == 1,]) == 0 &
       nrow(activePieces[activePieces$XLoc == 3 & activePieces$YLoc == 1,]) == 0 &
       nrow(activePieces[activePieces$XLoc == 4 & activePieces$YLoc == 1,]) == 0){
      
      activePieces[activePieces$Type == "King" & activePieces$Colour == "White",]$XLoc = 3
      activePieces[activePieces$Type == "Rook" & activePieces$Colour == "White" & activePieces$XLoc == 1,]$XLoc = 4
      if(inCheck(activePieces, pieceCol) == TRUE){return(FALSE)}
      if(inCheck(activePieces, pieceCol) == FALSE){return(TRUE)}
    }
    else{return(FALSE)}
  }
  
  if(pieceCol == "Black"){
    if(activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$Moves == 0 &
       nrow(activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 1
                         & activePieces$Moves == 0,]) == 1 &
       nrow(activePieces[activePieces$XLoc == 2 & activePieces$YLoc == 8,]) == 0 &
       nrow(activePieces[activePieces$XLoc == 3 & activePieces$YLoc == 8,]) == 0 &
       nrow(activePieces[activePieces$XLoc == 4 & activePieces$YLoc == 8,]) == 0){
      
      activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 3
      activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 1,]$XLoc = 4
      if(inCheck(activePieces, pieceCol) == TRUE){return(FALSE)}
      if(inCheck(activePieces, pieceCol) == FALSE){return(TRUE)}
    }
    else{return(FALSE)}
  }
  
}
#can i en passant? -----------------------------------------------------------------
#set this up with a "was last move a double space pawn move" query and then loop through
#all possible current pawn moves
canEnPassant = function(thisPawn, lastMovedPawn){
  if(thisPawn$Moves == 2 & 
     lastMovedPawn$Type == "Pawn" & 
     lastMovedPawn$Moves == 1 &
     thisPawn$YLoc == lastMovedPawn$YLoc &
     (thisPawn$XLoc - lastMovedPawn$XLoc)^2 == 1 &
     thisPawn$killCount == 0){return(TRUE)}
  else{return(FALSE)}
}

#make move --------------------------------------------------------------------------

makeMove = function(activePieces, bestMove,pieceCol){
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
  
  print(paste(activePieces[match(bestMove$PieceID,rownames(activePieces)),]$Type, 
              paste0(letters[activePieces[match(bestMove$PieceID,rownames(activePieces)),]$XLoc], 
                     activePieces[match(bestMove$PieceID,rownames(activePieces)),]$YLoc), "to", 
              paste0(letters[activePieces[match(bestMove$PieceID,rownames(activePieces)),]$XLoc + nextMove[1]*noMoves], 
                     activePieces[match(bestMove$PieceID,rownames(activePieces)),]$YLoc + nextMove[2]*noMoves)))
  
  
  activePieces$XLoc[match(bestMove$PieceID,rownames(activePieces))] = chosenPiece$XLoc + nextMove[1]*noMoves
  activePieces$YLoc[match(bestMove$PieceID,rownames(activePieces))] = chosenPiece$YLoc + nextMove[2]*noMoves
  activePieces$Moves[match(bestMove$PieceID,rownames(activePieces))] = chosenPiece$Moves + 1
  activePieces$killCount[match(bestMove$PieceID,rownames(activePieces))] = chosenPiece$killCount + 1
  
  activePieces$Taken[activePieces$XLoc == activePieces$XLoc[match(bestMove$PieceID,rownames(activePieces))] &
                       activePieces$Colour != pieceCol &
                       activePieces$YLoc == activePieces$YLoc[match(bestMove$PieceID,rownames(activePieces))]] = 1
  activePieces = activePieces[activePieces$Taken == 0,]
  
  
  
  
  activePieces$Type[activePieces$Type == "Pawn" & activePieces$YLoc == 8 & activePieces$Colour == "White"] = "Queen"
  activePieces$Type[activePieces$Type == "Pawn" & activePieces$YLoc == 1 & activePieces$Colour == "Black"] = "Queen"
  
  return(activePieces)
}

#can this piece be taken if I move it? --------------------------------------------------

threatened = function(activePieces, piece, pieceCol){
  wPMoves = list(c(0,1), c(0,2), c(1,1), c(-1,1))
  bPMoves = list(c(0,-1), c(0,-2), c(1,-1), c(-1,-1))
  BMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1))
  RMoves = list(c(1,0), c(-1,0), c(0,1), c(0,-1))
  NMoves = list(c(1,2), c(-1,2), c(1,-2), c(-1,-2), c(2,1), c(2,-1),c(-2,1), c(-2,-1))
  QMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))
  KMoves = list(c(1,1), c(-1,1), c(1,-1), c(-1,-1),c(1,0), c(-1,0), c(0,1), c(0,-1))
  
  Threats = 0
  #check for straight line checks
  
  for(x in 1:4){
    for(w in 1:8){
      if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + RMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Rook",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + RMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type == "Queen",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + RMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "Queen" & activePieces$Type != "Rook",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + RMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + RMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
  }
  
  #check for diagonal checks
  
  for(x in 1:4){
    for(w in 1:8){
      if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + BMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Bishop",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + BMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type == "Queen",]) == 1 ){
        Threats = Threats + 1
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + BMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "Queen" & activePieces$Type != "Bishop",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + BMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + BMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
  }
  
  #check for knight checks
  for(x in 1:8){
    for(w in 1:1){
      if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + NMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + NMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "Night",] ) == 1 ){
        Threats = Threats + 1
      }
      
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + NMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + NMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "Night",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + NMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + NMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
    
    
  }
  
  #check for pawn checks
  
  if(pieceCol == "White"){pawnMates = bPMoves}
  if(pieceCol == "Black"){pawnMates = wPMoves}
  
  for(x in 3:4){
    if(nrow(activePieces[activePieces$XLoc == (piece$XLoc - pawnMates[[x]][1]) & activePieces$YLoc == (piece$YLoc - pawnMates[[x]][2]) & 
                         activePieces$Colour != pieceCol & activePieces$Type == "Pawn",] ) == 1 ){
      Threats = Threats + 1
    }
    
    else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc - pawnMates[[x]][1]) & activePieces$YLoc == (piece$YLoc - pawnMates[[x]][2]) & 
                              activePieces$Colour != pieceCol & activePieces$Type != "Pawn",]) == 1 ){
      break()
    }
    else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc - pawnMates[[x]][1]) & activePieces$YLoc == (piece$YLoc - pawnMates[[x]][2]) & 
                              activePieces$Colour == pieceCol,]) == 1){
      break()
    }
    else{}  
    
  }
  
  #dont let kings get within 1 square
  
  for(x in 1:8){
    for(w in 1:1){
      if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + KMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + KMoves[[x]][2]*w) & 
                           activePieces$Colour != pieceCol & activePieces$Type == "King",] ) == 1 ){
        Threats = Threats + 1
      }
      
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + KMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + KMoves[[x]][2]*w) & 
                                activePieces$Colour != pieceCol & activePieces$Type != "King",]) == 1 ){
        break()
      }
      else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + KMoves[[x]][1]*w) & activePieces$YLoc == (piece$YLoc + KMoves[[x]][2]*w) & activePieces$Colour == pieceCol,]) == 1){
        break()
      }
      else{}  
    }
  }
  
  if(Threats > 0){return(TRUE)}
  if(Threats == 0){return(FALSE)}
}
# Loop -----------------------------------------------------------------------------

chessSim = function(){
  resign= 0
  par(bg = "grey")
  i = 1
  
  colSelect = 0
  while(colSelect == 0){
    playerCol = readline("Black or White?")
    if(playerCol != "White" & playerCol != "Black"){
      print("invalid choice")
    }
    else{
      colSelect = 1
    }
  }
  
  
  while(nrow(activePieces[activePieces$Type == "King",])>1){
    
    #plot board
    plot(1, type="n", xlab="", ylab="", xlim=c(0.5, 8.5), ylim=c(0.5, 8.5), xaxt = "n")
    axis(1, at=1:8, labels=letters[1:8])
    axis(2, at=1:8, labels=1:8)
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
    
    if(is.even(i) == F){pieceCol = "White"
    opponent = "Black"}
    else{pieceCol = "Black"
    opponent = "White"}
    
    evalTable = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("PieceID", "Move", "NoSpaces", "Points", "Position"))
    
    
    for(cn in rownames(activePieces[activePieces$Colour == pieceCol,])){
      
      piece = activePieces[match(cn,rownames(activePieces)),]
      
      #pawn possible moves
      #when attack is possible
      
      
      if(piece$Type == "Pawn" & pieceCol == "White"){
        multiplier = 1:1
        if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + 1) &
                             activePieces$YLoc == (piece$YLoc + 1) & activePieces$Colour != pieceCol,])==1){
          if(piece$Moves == 0){moves = wPMoves[c(1,2,3)]
          names(moves) = c(1,2,3)}
          else{moves = wPMoves[c(1,3)]
          names(moves) = c(1,3)}
        }
        
        else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc - 1) &
                                  activePieces$YLoc == (piece$YLoc + 1) & activePieces$Colour != pieceCol,])==1){
          if(piece$Moves == 0){moves = wPMoves[c(1,2,4)]
          names(moves) = c(1,2,4)}  #if first move, move 1 or 2 forwards
          else{moves = wPMoves[c(1,4)]
          names(moves) = c(1,4)}
        }
        else{
          if(piece$Moves ==0){moves = wPMoves[c(1,2)]
          names(moves) = c(1,2)}
          else{moves = wPMoves[1]
          names(moves) = c(1)
          }
        } 
      }
      if(piece$Type == "Pawn" & pieceCol == "Black"){
        multiplier = 1:1
        if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + 1) &
                             activePieces$YLoc == (piece$YLoc - 1) & activePieces$Colour != pieceCol,])==1){
          if(piece$Moves == 0){moves = bPMoves[c(1,2,3)]
          names(moves) = c(1,2,3)}
          else{moves = bPMoves[c(1,3)]
          names(moves) = c(1,3)}
        }
        
        else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc - 1) &
                                  activePieces$YLoc == (piece$YLoc - 1) & activePieces$Colour != pieceCol,])==1){
          if(piece$Moves == 0){moves = bPMoves[c(1,2,4)]
          names(moves) = c(1,2,4)}
          else{moves = bPMoves[c(1,4)]
          names(moves) = c(1,4)}
        }
        else{
          if(piece$Moves ==0){moves = bPMoves[c(1,2)]
          names(moves) = c(1,2)}
          else{moves = bPMoves[1]
          names(moves) = c(1)}
        } 
      }
      if(piece$Type == "Bishop"){moves = BMoves
      names(moves) = c(1,2,3,4)
      multiplier = 1:8}
      if(piece$Type == "Rook"){moves = RMoves
      names(moves) = c(1,2,3,4)
      multiplier = 1:8}
      if(piece$Type == "Night"){moves = NMoves
      names(moves) = c(1,2,3,4,5,6,7,8)
      multiplier = 1:1}
      if(piece$Type == "Queen"){moves = QMoves
      names(moves) = c(1,2,3,4,5,6,7,8)
      multiplier = 1:8}
      if(piece$Type == "King"){moves = KMoves
      names(moves) = c(1,2,3,4,5,6,7,8)
      multiplier = 1:1}
      
      for(y in as.numeric(names(moves))){      
        
        chosenMove = moves[[as.character(y)]]
        
        maxMoves = maxMovesFun(piece, activePieces, pieceCol, chosenMove, multiplier)
        if(piece$Type == "Pawn" & piece$Moves > 0 & y == 2){}
        else if(maxMoves > 0){
          for(e in 1:maxMoves){
            
            evalTable2 = setNames(data.frame(matrix(ncol = 5, nrow = 1)), c("PieceID", "Move", "NoSpaces", "Points", "Position"))
            
            evalTable2$PieceID = cn
            evalTable2$Move = y
            evalTable2$NoSpaces = e
            evalTable2$Points = moveScore(activePieces, piece, pieceCol, opponent, chosenMove, e)
            evalTable2$Position = paste(activePieces[match(cn,rownames(activePieces)),]$Type, paste0(letters[activePieces[match(cn,rownames(activePieces)),]$XLoc], 
                                                                                                     activePieces[match(cn,rownames(activePieces)),]$YLoc), "to", 
                                        paste0(letters[activePieces[match(cn,rownames(activePieces)),]$XLoc + chosenMove[1]*e], 
                                               activePieces[match(cn,rownames(activePieces)),]$YLoc + chosenMove[2]*e))
            
            evalTable = rbind(evalTable, evalTable2)
            
          }
        }
        else{}
        
        
      }
      
    }
    
    if(nrow(evalTable[evalTable$Points > -1,]) == 0 & inCheck(activePieces, pieceCol) == TRUE){
      print(paste("CHECKMATE!", opponent,"wins!"))
      break()
    } 
    else if(nrow(evalTable[evalTable$Points > -100,]) == 0 & inCheck(activePieces, pieceCol) == FALSE){
      print("STALEMATE!")
      break()
    }
    else if(nrow(activePieces) < 3){
      print("STALEMATE!")
      break()
    }
    
    if(pieceCol != playerCol){
      evalTable = evalTable[order(-evalTable$Points),]
      bestMove = head(evalTable, 1)
      if(bestMove$Points == 0){
        if(i > 1){
          if(activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Type == "Pawn" &
           nrow(activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc + 1 
                             & activePieces$YLoc == 4 & activePieces$Type == "Pawn",]) ==1){
          if(canEnPassant(activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc + 1
                                       & activePieces$YLoc == 4,], 
                          activePieces[match(lastMove$PieceID,rownames(activePieces)),]) == T){
            activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc + 1 & activePieces$YLoc == 4,]$killCount = activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc + 1 & activePieces$YLoc == 4,]$killCount + 1
            activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc + 1 & activePieces$YLoc == 4,]$YLoc = 3
            activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc + 1 & activePieces$YLoc == 3,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
            activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          }
          else if(activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Type == "Pawn" &
                  nrow(activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 
                                    & activePieces$YLoc == 4 & activePieces$Type == "Pawn",]) ==1){
            if(canEnPassant(activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1
                                         & activePieces$YLoc == 4,], 
                            activePieces[match(lastMove$PieceID,rownames(activePieces)),]) == T){
              activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 4,]$killCount = activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 4,]$killCount + 1
              activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 4,]$YLoc = 3
              activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 3,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
              activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
            }
            else if(canCastleKingside(activePieces, pieceCol) == T){        
              activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 7
              activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 8,]$XLoc = 6
            }
            else if(canCastleQueenside(activePieces, pieceCol) == T){
              activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 3
              activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 1,]$XLoc = 4
            }
            else{bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0,]), 1),]
            activePieces = makeMove(activePieces, bestMove, pieceCol)}
          }
          
          else if(canCastleKingside(activePieces, pieceCol) == T){        
            activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 7
            activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 8,]$XLoc = 6
          }
          else if(canCastleQueenside(activePieces, pieceCol) == T){
            activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 3
            activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 1,]$XLoc = 4
          }
          else{bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0,]), 1),]
          activePieces = makeMove(activePieces, bestMove, pieceCol)}
        }
          else if(activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Type == "Pawn" &
                nrow(activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 
                                  & activePieces$YLoc == 4 & activePieces$Type == "Pawn",]) ==1){
          if(canEnPassant(activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1
                                       & activePieces$YLoc == 4,], 
                          activePieces[match(lastMove$PieceID,rownames(activePieces)),]) == T){
            activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 4,]$killCount = activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 4,]$killCount + 1
            activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 4,]$YLoc = 3
            activePieces[activePieces$XLoc == activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc - 1 & activePieces$YLoc == 3,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
            activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          }
          else if(canCastleKingside(activePieces, pieceCol) == T){        
            activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 7
            activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 8,]$XLoc = 6
          }
          else if(canCastleQueenside(activePieces, pieceCol) == T){
            activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 3
            activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 1,]$XLoc = 4
          }
          else{bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0,]), 1),]
          activePieces = makeMove(activePieces, bestMove, pieceCol)}
        }
        
          else if(canCastleKingside(activePieces, pieceCol) == T){        
          activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 7
          activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 8,]$XLoc = 6
        }
          else if(canCastleQueenside(activePieces, pieceCol) == T){
          activePieces[activePieces$Type == "King" & activePieces$Colour == "Black",]$XLoc = 3
          activePieces[activePieces$Type == "Rook" & activePieces$Colour == "Black" & activePieces$XLoc == 1,]$XLoc = 4
        }
          else{bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0,]), 1),]
          activePieces = makeMove(activePieces, bestMove, pieceCol)}
        }
        
        else{bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0,]), 1),]
        activePieces = makeMove(activePieces, bestMove, pieceCol)}
        
      }
      else{activePieces = makeMove(activePieces, bestMove, pieceCol)}
      
    }
    
    if(pieceCol == playerCol){
      if(i == 1){
        print("You play as white")
        print("Type the row number of your chosen move")
        print("Alternatively, type O-O to castle kingside, O-O-O to castle queenside")
        print("Or type En Passant followed by your pawn's file letter to capture En Passant")
        print("If you want to quit, type resign")
      }
      validMove = 0
      while(validMove == 0){      
        print(evalTable[evalTable$Points > -100, c(1,5)])
        choice = readline(prompt = "Choose your move (enter rowname)")
        if(choice %in% rownames(evalTable[evalTable$Points > -100,]) == T){
          bestMove = evalTable[choice,]
          activePieces = makeMove(activePieces, bestMove, pieceCol)
          validMove = 1
        }
        
        ####INSERT EN PASSANT STUFF HERE
        
        else if(choice == "En Passant A" && 
                canEnPassant(activePieces[activePieces$XLoc == 1 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 1 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 1 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 1 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 1 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        else if(choice == "En Passant B" &&
                canEnPassant(activePieces[activePieces$XLoc == 2 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 2 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 2 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 2 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 2 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        else if(choice == "En Passant C" &&
                canEnPassant(activePieces[activePieces$XLoc == 3 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 3 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 3 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 3 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 3 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        else if(choice == "En Passant D" && 
                canEnPassant(activePieces[activePieces$XLoc == 4 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 4 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 4 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 4 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 4 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        else if(choice == "En Passant E" &&
                canEnPassant(activePieces[activePieces$XLoc == 5 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 5 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 5 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 5 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 5 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        else if(choice == "En Passant F" && 
                canEnPassant(activePieces[activePieces$XLoc == 6 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 6 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 6 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 6 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 6 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        else if(choice == "En Passant G" &&
                canEnPassant(activePieces[activePieces$XLoc == 7 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 7 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 7 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 7 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 7 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        else if(choice == "En Passant H" && 
                canEnPassant(activePieces[activePieces$XLoc == 8 & activePieces$YLoc == 5,], 
                             activePieces[match(lastMove$PieceID,rownames(activePieces)),])){
          activePieces[activePieces$XLoc == 8 & activePieces$YLoc == 5,]$killCount = activePieces[activePieces$XLoc == 8 & activePieces$YLoc == 5,]$killCount + 1
          activePieces[activePieces$XLoc == 8 & activePieces$YLoc == 5,]$YLoc = 6
          activePieces[activePieces$XLoc == 8 & activePieces$YLoc == 6,]$XLoc = activePieces[match(lastMove$PieceID,rownames(activePieces)),]$XLoc
          activePieces[match(lastMove$PieceID,rownames(activePieces)),]$Taken = 1
          validMove = 1
        }
        
        else if(choice == "O-O" & canCastleKingside(activePieces, pieceCol) == T){
          activePieces[activePieces$Type == "King" & activePieces$Colour == "White",]$XLoc = 7
          activePieces[activePieces$Type == "Rook" & activePieces$Colour == "White" & activePieces$XLoc == 8,]$XLoc = 6
          validMove = 1
        }
        else if(choice == "O-O-O" & canCastleQueenside(activePieces, pieceCol) == T){
          activePieces[activePieces$Type == "King" & activePieces$Colour == "White",]$XLoc = 3
          activePieces[activePieces$Type == "Rook" & activePieces$Colour == "White" & activePieces$XLoc == 1,]$XLoc = 4
          validMove = 1
        }
        else if(choice == "resign"){print(paste(opponent, "wins!"))
          resign = 1
          break()}
        
        else{ print("Invalid choice, try again")}
        
      }
    }
    
    if(resign == 1){break()}
    
    if(nrow(activePieces[activePieces$Type == "King",]) < 2){
      print(paste(pieceCol, "wins!"))
    }
    lastMove = bestMove
    i = i + 1
    
    
  }
}
