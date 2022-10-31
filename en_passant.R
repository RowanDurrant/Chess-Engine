canEnPassant = function(thisPawn, lastMovedPawn){
  if(thisPawn$Moves == 2 & 
     lastMovedPawn$Type == "Pawn" & 
     lastMovedPawn$Moves == 1 &
     thisPawn$YLoc == lastMovedPawn$YLoc &
     (thisPawn$XLoc - lastMovedPawn$XLoc)^2 == 1 &
     thisPawn$killCount == 0){return(TRUE)}
  else{return(FALSE)}
}