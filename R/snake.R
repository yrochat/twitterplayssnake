rm(list=ls())

library(httr)
library(twitteR)

is_on_the_border <- function(pos1, pos2, height, width) {
  pos1 == pos2 | any(((pos1 - 1) %/% width + 1) %in% c(1, height)) | any(((pos1) %% width) %in% c(0, 1))
}

init_board <- function(height = 5, width = 10) {

  # empty board
  display <- list(board = matrix(), 
                  mouse = numeric(), 
                  snake = vector(), 
                  direction = numeric(), 
                  finished = logical(),
                  lastmove = character())

  display$mouse <- sample(width * height, 1)
  display$snake <- sample(width * height, 1)  

  while (is_on_the_border(display$snake, display$mouse, height = height, width = width)) {
    display$snake <- sample(width * height, 1)
  }

  display$direction <- sample(4, 1)
  display$board <- matrix(data = 0, nrow = height, ncol = width)
  display$board[display$mouse] <- 5
  display$board[display$snake] <- 6
  
  display$finished <- F

  return(display)
}

# 0 empty square \u2b1c
# 1 right arrow  ➡
# 2 up    arrow  \u2b06
# 3 left  arrow  \u2b05
# 4 down  arrow  \u2b07
# 5 mouse        \U0001f42d
# 6 snake        \xF0\x9F\x90\x8D

emoji.table <- data.frame(code = 0:6, emoji = c("\u2b1c", "➡", "\u2b06", "\u2b05", "\u2b07", "\U0001f42d", "\xF0\x9F\x90\x8D"), stringsAsFactors = F)

draw_board <- function(display) {
  tweet <- lapply(1:nrow(display$board), function(n) emoji.table$emoji[match(display$board[n, ], emoji.table$code)])
  tweet <- lapply(tweet, function(x) paste0(x, collapse = ""))
  tweet <- do.call(rbind, tweet)[, 1]
  tweet <- paste0(tweet, collapse = "\n")
  return(tweet)
}

directions <- c("droite", "right", "rightwards", "➡",
                "haut", "top", "upwards", "\u2b06",
                "gauche", "left", "leftwards", "\u2b05",
                "bas", "bottom", "downwards", "\u2b07")
                
directions.df <- data.frame(directions = directions, code = as.vector(sapply(1:4, function(n) rep(n, 4))), stringsAsFactors = F)

directions.collapsed <- paste0(directions.df$directions, collapse = "|")

update_board <- function(display = display) {
  
}
















