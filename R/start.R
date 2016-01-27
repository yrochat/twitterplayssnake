rm(list=ls())

debug <- T

setwd("your_working_directory")

##################
### Dimensions ###
##################

height <- 10
width <- 10

###############################
### run only the first time ###
###############################
#
# display <- init_board(10, 10)
# tweet(draw_board(display))
# save(display, file = "display.Rdata")
#

#######################
### Load last state ###
#######################

load("display.Rdata")

if (debug) print(display)

##########################################################
### Speed of the game (which is executed every minute) ###
##########################################################

library(lubridate)

if (debug) print(now())

minute_now <- minute(now())

if (debug) print(sum(display$board %in% 1:4))

if (debug) print(minute_now)

# if the length of the snake is between 1 and 3 without the head => /10 minutes
if (any(sum(display$board %in% 1:4) == c(1,2,3)) && (minute_now %% 10) == 0) {
  source("snake.R")
  source("run.R")
} else if (any(sum(display$board %in% 1:4) == c(4,5,6)) && (minute_now %% 5) == 0) {
  # if the length of the snake is between 4 and 6 without the head => /5 minutes
  source("snake.R")
  source("run.R")
} else if (any(sum(display$board %in% 1:4) == c(7,8,9)) && (minute_now %% 4) == 0) {
  # if the length of the snake is between 7 and 9 without the head => /4 minutes
  source("snake.R")
  source("run.R")
} else if (any(sum(display$board %in% 1:4) == c(10,11,12)) && (minute_now %% 3) == 0) {
  # if the length of the snake is between 10 and 12 without the head => /3 minutes
  source("snake.R")
  source("run.R")
} else if (any(sum(display$board %in% 1:4) == c(13,14,15)) && (minute_now %% 2) == 0) {
  # if the length of the snake is between 13 and 15 without the head => /2 minutes
  source("snake.R")
  source("run.R")
} else if (sum(display$board %in% 1:4) > 15) {
  # if the length of the snake is 16 or over without the head => /1 minute
  source("snake.R")
  source("run.R")
}

