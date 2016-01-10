library(plyr)

init_board <- function(height = 5, width = 10) {
	
  # empty board
  display <- list(board = matrix(),
                  mouse = numeric(),
                  snake = vector(),
                  direction = numeric())
                  
  display$mouse <- sample(width*height, 1)
  display$snake <- sample(width*height, 1)
  
  while (display$snake == display$mouse | any((display$snake %% height + 1) %in% c(1, height)) | any((display$snake %% width) %in% c(1, width))) {
    display$snake <- sample(width*height, 1)
  }
  
  display$direction <- sample(4, 1)
  display$board <- matrix(data = 0, nrow = height, ncol = width)
  display$board[display$mouse] <- 5
  display$board[display$snake] <- display$direction
  
  return(display)
}

# 0 empty square \xE2\xAC\x9C
# 1 right arrow  \xE2\x9E\xA1
# 2 up    arrow  \xE2\xAC\x86
# 3 left  arrow  \xE2\xAC\x85
# 4 down  arrow  \xE2\xAC\x87
# 5 mouse        \xF0\x9F\x90\xAD

emoji.table <- data.frame(code = 0:5, 
                          emoji = c("\xE2\xAC\x9C", "\xE2\x9E\xA1", "\xE2\xAC\x86", "\xE2\xAC\x85", "\xE2\xAC\x87", "\xF0\x9F\x90\xAD"), 
                          stringsAsFactors = F)

draw_board <- function(display) {
  tweet <- lapply(1:nrow(display$board), function(n) emoji.table$emoji[match(display$board[n,], emoji.table$code)])
  tweet <- lapply(tweet, function(x) paste0(x, collapse = ""))
  tweet <- do.call(rbind, tweet)[,1]
  tweet <- paste0(tweet, collapse = "\n")
  return(tweet)
}



# display <- init_board()
# hey <- draw_board(display)
# tweet(hey)
