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
    if(playerCol == "White"){
      plot(1, type="n", xlab="", ylab="", xlim=c(0.5, 8.5), ylim=c(0.5, 8.5), xaxt = "n")
      axis(1, at=1:8, labels=letters[1:8])
      axis(2, at=1:8, labels=1:8) 
    }
    if(playerCol == "Black"){
      plot(1, type="n", xlab="", ylab="", xlim=rev(c(0.5, 8.5)), ylim=rev(c(0.5, 8.5)), xaxt = "n")
      axis(1, at=1:8, labels=letters[1:8])
      axis(2, at=1:8, labels=1:8) 
    }
    
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
    
    if(nrow(evalTable[evalTable$Points > -100,]) == 0 & inCheck(activePieces, pieceCol) == TRUE){
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
          else{
            pieceToMove = sample(evalTable$PieceID[evalTable$Points == 0], 1)
            bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0 & evalTable$PieceID == pieceToMove,]), 1),]
            activePieces = makeMove(activePieces, bestMove, pieceCol)}
        }
        
        else{bestMove = evalTable[sample(rownames(evalTable[evalTable$Points == 0,]), 1),]
        activePieces = makeMove(activePieces, bestMove, pieceCol)}
        
      }
      else{activePieces = makeMove(activePieces, bestMove, pieceCol)}
      
    }
    
    if(pieceCol == playerCol){
      if(i == 1 | i == 2){
        print("You play as white")
        print("Type the row number of your chosen move")
        print("Alternatively, type O-O to castle kingside, O-O-O to castle queenside")
        print("Or type En Passant followed by your pawn's file letter to capture En Passant")
        print("If you want to quit, type resign")
      }
      validMove = 0
      while(validMove == 0){      
        print(evalTable[evalTable$Points > -100, c(2,5)])
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
