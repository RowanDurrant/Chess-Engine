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