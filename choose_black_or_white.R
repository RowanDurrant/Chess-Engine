library(animation)
library(schoolmath)

# pieces set up 
source("pieces_set_up.R")
# Max moves in direction 
source("max_moves.R")
#Am I in check? 
source("in_check.R")
# does this move capture anything?
source("move_score.R")
# can i castle? 
source("castling.R")
#can i en passant?
#set this up with a "was last move a double space pawn move" query and then loop through
#all possible current pawn moves
source("en_passant.R")
#make move 
source("make_move.R")
#can this piece be taken if I move it? 
source("threatened.R")
#the main guts
source("main_stuff.R")


chessSim()
