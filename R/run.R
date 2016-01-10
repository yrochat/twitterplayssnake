# 0. Twitter authentication
# 1. Load last state
# 2. Is it a new game ?
# 2a. If yes, initialise a game, tweet it and BREAK.
# 2b. If no, get tweets from the last 5 minutes. 
# 2ba. If none, KEEP SAME DIRECTION.
# 2bb. If yes, Extract direction and username from last tweet.
# 3a. If new, update usernames count.
# 3b. If an old tweet, drop username.
# 4. 




source("my_tokens.R")

# callback url http://127.0.0.1:1410

options(httr_oauth_cache=TRUE) 
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# run only the first time
#
# display <- init_board()
# tweet(draw_board(display))
# save(display, file = "display.Rdata")
#

# Load last state

load("display.Rdata")

# If the game was finished, start a new one and stop the script

if (display$finished == T) {
  display <- init_board()
  tweet(draw_board(display))
  save(display, file = "display.Rdata")
  stop("Game initialised")
}

# If the game was not finished, get the last tweets

load("last_mention_id.Rdata")

last_mentions <- twListToDF(mentions())
last_mentions <- last_mentions[last_mentions$id > id_threshold,]

# We update the value for the last ID

id_threshold <- last_mentions$id[1]
if (length(id_threshold) > 0) {
  save(id_threshold, file = "last_mention_id.Rdata")
}

# We select the tweets with values for direction

ok_mentions <- last_mentions[grepl(directions.collapsed, last_mentions$text),]

if (nrow(ok_mentions) > 0) {
  regex <- regexpr(directions.collapsed, ok_mentions$text)
  regex <- match(regmatches(ok_mentions$text, regex), directions.df$directions)
  if (length(regex) == 1) {
  	display$direction <- as.numeric(regex)
  } else {
  	display$direction <- as.numeric(sample(directions.df$code[regex], 1))
  }
  update_board(display, new_direction = display$direction)
} else {
  update_board(display, new_direction = display$direction)	
}










