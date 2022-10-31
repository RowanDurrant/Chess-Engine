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