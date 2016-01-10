init_board <- function(height = 5, width = 10) {
	
  # empty board
  display <- list(board = matrix(),
                  fruit = numeric(),
                  snake = vector(),
                  direction = numeric())
                  
  display$fruit <- sample(width*height, 1)
  display$snake <- sample(width*height, 1)
  
  while (display$snake == display$fruit | any((display$snake %% height + 1) %in% c(1, height)) | any((display$snake %% width) %in% c(1, width))) {
    display$snake <- sample(width*height, 1)
  }
  
  display$direction <- sample(4, 1)
  display$board <- matrix(data = 0, nrow = height, ncol = width)
  display$board[display$fruit] <- 5
  display$board[display$snake] <- display$direction
  
  return(display)
}



draw_board <- function(display)






1. générer les positions aléatoirement
2. dessiner le jeu
3. entrer une direction
4. déplacer le serpent et résoudre