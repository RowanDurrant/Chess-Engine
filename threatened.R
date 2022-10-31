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