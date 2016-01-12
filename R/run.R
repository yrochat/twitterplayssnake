rm(list=ls())

source("my_tokens.R")
source("snake.R")

height <- 10
width <- 10

# callback url http://127.0.0.1:1410

options(httr_oauth_cache=TRUE) 
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# run only the first time
#
# display <- init_board(10, 10)
# tweet(draw_board(display))
# save(display, file = "display.Rdata")
#

# Load last state

load("display.Rdata")

# If the game was finished, start a new one and stop the script

reinit(display$finished, height, width)

# that's id_threshold

id_threshold <- scan("last_mention_id.txt", quiet = T)

# If the game was not finished, get the last tweets 

last_mentions <- twListToDF(mentions())
last_mentions <- last_mentions[last_mentions$id > as.numeric(id_threshold),]

# We update the value for the last ID

id_threshold <- last_mentions$id[1]
if (length(id_threshold) > 0) {
  cat(id_threshold, file = "last_mention_id.txt")
}

# We select the tweets with values for direction

ok_mentions <- get_directions(last_mentions)

display <- update_directions(display, ok_mentions)

display <- update_board(display)	

display

if (display$lastmove == "") {
  tweet(draw_board(display))	
} else {
  tweet(paste0(draw_board(display), "\nLast move by @", display$lastmove, collapse = ""))	
}

display$lastmove <- ""

save(display, file = "display.Rdata")








