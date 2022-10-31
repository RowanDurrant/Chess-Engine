moveScore = function(activePieces, piece, pieceCol, opponent, y, z){
  points = 0
  if(piece$Type == "Pawn" & y[1] == 0){points = 0}
  else if(nrow(activePieces[activePieces$XLoc == (piece$XLoc + y[1]*z) &
                            activePieces$YLoc == (piece$YLoc + y[2]*z) &
                            activePieces$Colour != pieceCol,])==1){
    
    takenPiece = activePieces[activePieces$XLoc == (piece$XLoc + y[1]*z) &
                                activePieces$YLoc == (piece$YLoc + y[2]*z) &
                                activePieces$Colour != pieceCol,]
    
    if(takenPiece$Type == "Pawn"){points = points + 1}
    if(takenPiece$Type == "Night" || takenPiece$Type == "Bishop"){points = points + 3}
    if(takenPiece$Type == "Rook"){points = points + 5}
    if(takenPiece$Type == "Queen"){points = points + 9}
    if(takenPiece$Type == "King"){points = points + 100}
    
  }
  else{}
  
  activePieces[match(rownames(piece), rownames(activePieces)),]$XLoc = piece$XLoc + y[1]*z
  activePieces[match(rownames(piece), rownames(activePieces)),]$YLoc = piece$YLoc + y[2]*z
  activePieces$Taken[activePieces$XLoc == piece$XLoc+ y[1]*z &
                       activePieces$Colour != pieceCol &
                       activePieces$YLoc == piece$YLoc+ y[2]*z] = 1
  activePieces = activePieces[activePieces$Taken == 0,]
  
  if(inCheck(activePieces, opponent) == TRUE){points = points + 1}
  if(threatened(activePieces, piece, pieceCol ) == TRUE){
    if(piece$Type == "Pawn"){points = points - 1}
    if(piece$Type == "Night" || piece$Type == "Bishop"){points = points - 3}
    if(piece$Type == "Rook"){points = points - 5}
    if(piece$Type == "Queen"){points = points - 9}
  }
  if(inCheck(activePieces, pieceCol) == TRUE){points = -1000}
  else{}
  
  return(points)
}