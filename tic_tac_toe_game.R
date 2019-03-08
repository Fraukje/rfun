get_player_move <- function (player, board) {
        prompt_message = paste("Player", player, ", choose your move (1-9): ", sep = " " )
        n <- suppressWarnings(as.integer(readline(prompt=prompt_message)))
        while(if(is.na(board[n])){TRUE} else {board[n] != 0}){
                prompt_message = paste("Invalid or taken position, please choose again:")
                n <- suppressWarnings(as.integer(readline(prompt=prompt_message)))
                print(board)
        }
        return(n)
}

check_winner <- function(board, player){
        if(any(apply(board, 1, function(x, want) isTRUE(all.equal(x, want)), c(player, player, player))) |
           any(apply(board, 2, function(x, want) isTRUE(all.equal(x, want)), c(player, player, player)))) {
                  return(paste("Player", player, "has three in a row and wins!"))
        }      
        else{ 
                  return(NULL)}
}

tic_tac_toe <- function() {
        
        library(raster) ##Visualizing the board needs raster package
  
        print("Welcome to Tic, Tac, Toe game!")
  
        # Initiate board, set turn and choose random starting player
        board <- matrix(0, 3, 3)
        turn <- 1
        player <- sample(1:2, 1)
        print(paste("Player", player, "may start", sep = " "))
        
        ## Start playing: get player move, update and show board.
        while(turn < 10) {
                n <- get_player_move(player, board)
                board[n] <- player
                plot(raster(board), legend = "false")
                turn <- turn + 1

                ## Check if game is done
                result <- check_winner(board, player)
                if (!is.null(result)) {
                          print(result)
                          break
                }
                ## Swap player
                player <- if(player == 1){2}
                else {1}
        }
}