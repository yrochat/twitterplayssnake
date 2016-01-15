##############################
### Twitter authentication ###
##############################

library(httr)
library(twitteR)

# callback url http://127.0.0.1:1410

source("my_tokens.R")

options(httr_oauth_cache=TRUE) 
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#################################################################
### that's last tweet ID so that we include only fresh tweets ###
#################################################################

id_threshold <- scan("last_mention_id.txt", quiet = T, what = character())

if (debug) print(as.character(id_threshold))

last_mentions <- mentions(sinceID = id_threshold)

if (debug) print(last_mentions)

##############################
### Pick a mention, if any ###
##############################

options("scipen" = 100, "digits" = 4)  # Say no to scientific notation

if (length(last_mentions) > 0) {
  last_mentions <- twListToDF(last_mentions)

  # We update the value for the last ID
  # Kind of a dirty hack
  # See why here: https://twitter.com/yrochat/status/687942528809734144
  temp_id_threshold <- last_mentions$id[1]
  last_digit <- substring(temp_id_threshold, nchar(temp_id_threshold) - 5, nchar(temp_id_threshold))
  last_digit <- as.numeric(last_digit) + 1
  substring(temp_id_threshold, nchar(temp_id_threshold) - 5, nchar(temp_id_threshold)) <- as.character(last_digit)
  
  if (debug) sprintf(as.character(temp_id_threshold))
  
  cat(temp_id_threshold, file = "last_mention_id.txt")
  
  # We select the tweets with values for direction

  ok_mentions <- get_directions(last_mentions)

  if (debug) print(ok_mentions)

  display <- update_directions(display, ok_mentions)
}

if (debug) print(display)

##############################################################
### Update the board with the picked mention, or autopilot ###
##############################################################

display <- update_board(display)

if (debug) print(display)

if (display$finished == T) {
  if (display$lastmove == "") {
    tweet(paste0(draw_board(display), "\n\nGame over by autopilot", collapse = ""))
  } else {
    tweet(paste0(draw_board(display), "\n\nGame over by @", display$lastmove, collapse = ""))	
  }  	
} else {
  if (display$lastmove == "") {
    tweet(paste0(draw_board(display), "\n\nLast move by autopilot", collapse = ""))	
  } else {
    tweet(paste0(draw_board(display), "\n\nLast move by @", display$lastmove, collapse = ""))	
  }
}

display$lastmove <- ""

if (debug) print(display)

save(display, file = "display.Rdata")

# If the game was finished, start a new one and stop the script

if (display$finished == T) {
  Sys.sleep(15)
  reinit(height, width)
}







