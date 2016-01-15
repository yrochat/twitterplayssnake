library(httr)
library(twitteR)

### the name speaks for itself

is_on_the_border <- function(pos1, pos2, height, width) {
  pos1 == pos2 | any(((pos1 - 1) %/% width + 1) %in% c(1, height)) | any(((pos1) %% width) %in% c(0, 1))
}

### the first time, and after « game over »

init_board <- function(height, width) {

  # empty board
  display <- list(board = matrix(), 
                  mouse = numeric(), 
                  snake = vector(), 
                  direction = numeric(), 
                  finished = F,
                  lastmove = "")

  display$board <- matrix(data = 0, nrow = height, ncol = width, byrow = F)

  display$mouse <- sample(width * height, 1)
  display$snake <- sample(width * height, 1)  

  while (is_on_the_border(display$snake, display$mouse, height = height, width = width)) {
    display$snake <- sample(width * height, 1)
  }

  display$direction <- sample(4, 1)
  display$board[display$mouse] <- 5
  display$board[display$snake] <- 6
  
  if (display$direction == 1) {
  	display$snake[2] <- display$snake - 10
    display$board[display$snake[2]] <- 1
  } else if (display$direction == 2) {
    display$snake[2] <- display$snake + 1
    display$board[display$snake[2]] <- 2
  } else if (display$direction == 3) {
    display$snake[2] <- display$snake + 10
    display$board[display$snake[2]] <- 3
  } else {
    display$snake[2] <- display$snake - 1
    display$board[display$snake[2]] <- 4
  }
  
  display$finished <- F

  return(display)
}

### which emoji to draw

# 0 empty square \u2b1c
# 1 right arrow  ➡
# 2 up    arrow  \u2b06
# 3 left  arrow  \u2b05
# 4 down  arrow  \u2b07
# 5 mouse        \U0001f42d
# 6 snake        \xF0\x9F\x90\x8D

emoji.table <- data.frame(code = 0:6, emoji = c("\u2b1c", "➡", "\u2b06", "\u2b05", "\u2b07", "\U0001f42d", "\xF0\x9F\x90\x8D"), stringsAsFactors = F)

# I don't remember what that one does… :-P

draw_board <- function(display) {
  tweet <- lapply(1:nrow(display$board), function(n) emoji.table$emoji[match(display$board[n, ], emoji.table$code)])
  tweet <- lapply(tweet, function(x) paste0(x, collapse = ""))
  tweet <- do.call(rbind, tweet)[, 1]
  tweet <- paste0(tweet, collapse = "\n")
  return(tweet)
}

### Which inputs to allow, and what they correspond to

directions <- c("droite", "right", "rightwards", "➡",
                "haut", "up", "upwards", "\u2b06",
                "gauche", "left", "leftwards", "\u2b05",
                "bas", "down", "downwards", "\u2b07")
                
directions.df <- data.frame(directions = directions, code = as.vector(sapply(1:4, function(n) rep(n, 4))), stringsAsFactors = F)

directions.collapsed <- paste0(directions.df$directions, collapse = "|")

### Choose the direction of someone

get_directions <- function(last_mentions) {
  ok_mentions <- last_mentions[grepl(directions.collapsed, tolower(last_mentions$text)),]
  ok_mentions <- ok_mentions[!duplicated(ok_mentions$screenName),]
  return(ok_mentions)
}

### Take the direction proposed by someone and update the board

update_directions <- function(display, ok_mentions = data.frame()) {
  if (nrow(ok_mentions) > 0) {
    regex <- regexpr(directions.collapsed, tolower(ok_mentions$text))
    regex <- match(regmatches(tolower(ok_mentions$text), regex), directions.df$directions)
    if (length(regex) == 1) {
      display$lastmove <- ok_mentions$screenName
      new_direction <- as.numeric(directions.df$code[regex])
    } else {
      sampled <- sample(1:length(regex), 1)
      display$lastmove <- ok_mentions$screenName[sampled]
      new_direction <- as.numeric(directions.df$code[regex][sampled])
    }
    if ((display$direction != new_direction) && ((display$direction + new_direction) %in% c(4, 6))) {
      display$finished <- T
    } else {
      display$direction <- new_direction
    }
  } else {
  	display$lastmove <- ""
  }
  return(display)
}

### at random times the mouse moves

update_mouse <- function(display) {
  display$board[display$mouse] <- 0
  	
  temp_mouse <- display$mouse + c(-1, +1, -10, +10)
  temp_mouse <- temp_mouse[temp_mouse %in% 1:100]
  display$mouse <- sample(temp_mouse, 1)
    
  display$board[display$mouse] <- 5
  
  return(display)
}

### big one with lot of exceptions

update_board <- function(display) {
  if (display$direction == 1) {
    # serpent sur le bord droite ?
    next_move <- (display$snake[1] + ncol(display$board)) %% (height * width)
  } else if (display$direction == 2) {
  	# serpent sur le bord supérieur ?
    next_move <- display$snake[1] - 1
    if (next_move %% nrow(display$board) == 0) {
    	  next_move <- next_move + nrow(display$board)
    }
  } else if (display$direction == 3) {
    # serpent sur le bord gauche ?
  	next_move <- (display$snake[1] - ncol(display$board)) %% (height * width)
  } else if (display$direction == 4) {
    # serpent sur le bord inférieur ?
  	next_move <- display$snake[1] + 1
  	if (next_move %% nrow(display$board) == 1) {
      next_move <- next_move - nrow(display$board)
    }
  }

  # donner serpent au premier élément
  display$board[next_move] <- 6
    
  # donner la direction au second élément
  display$board[display$snake[1]] <- display$direction

  # ajouter l'élément au serpent
  display$snake <- c(next_move, display$snake)

  # si le serpent a atteint la souris…
  if (next_move == display$mouse) {
    # déterminer une nouvelle souris
    display$mouse <- sample(which(display$board == 0), 1)
    display$board[display$mouse] <- 5
  } else {
    # et sinon, enlever le dernier
    display$board[display$snake[length(display$snake)]] <- 0
    display$snake <- display$snake[-length(display$snake)]
    # et peut-être déplacer la souris
    if (sample(5, 1) == 1) {
      display <- update_mouse(display)
    }
  }
  
  # si le serpent s'est mordu
  if (any(duplicated(display$snake))) {  	
  	display$finished <- T
  }
  
  return(display)
}

### if game over, restart

reinit <- function(height, width) {
  display <- init_board(height, width)
  tweet(paste0(draw_board(display), "\n\nNew game !", collapse = ""))
  save(display, file = "display.Rdata")
}











