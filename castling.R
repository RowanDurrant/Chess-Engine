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