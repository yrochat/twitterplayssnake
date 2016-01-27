Replace "Twitch" by "Twitter", then "Pokemon" by "Snake" and you get…
# Twitter Plays Snake
These scripts provide a simple snake game, simple visual outputs, and simple commands to deal with inputs from Twitter.

The result is [@letsplaysnake](http://twitter.com/letsplaysnake).

It follows [@dariusk](https://github.com/dariusk)'s «[Basic Twitter bot etiquette»](http://tinysubversions.com/2013/03/basic-twitter-bot-etiquette/).

Instructions are in the account's bio.

Yes, it's all written in R. 

## Some rules

* The script gets one random tweet since its last update. Only the first command. No more than one per player.
* If it gets no input, it keeps moving in the same direction like in the original game.
* Speed varies like in the version I was playing on a Macintosh (Classic?) long time ago. [Tweets are more frequent as the snake gets longer](https://github.com/yrochat/twitterplayssnake/blob/master/R/start.R#L45-L69).
* The mouse moves randomly, but not often. This was meant to avoid Twitter blocking the account when it gets no input and thus posts the same tweet twice.
* The board is a torus.
