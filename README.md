# R-Code-for-tweet-sentiment-analysis
library(twitteR)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

api_key <- 'xxxXXXXXXXXXXXXXxxXXXXXXXX'
api_secret <- 'xxxXXXXXXXXXXXXXxxXXXXXXXX'
access_token <- "366131435-xxxXXXXXXXXXXXXXxxXXXXXXXX"
access_token_secret <- "xxxXXXXXXXXXXXXXxxXXXXXXXX"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#the function for extracting and analyzing liquor tweets
search <- function(searchterm)
{
  #extact liquor tweets and create storage file
  
  list <- searchTwitter(searchterm, n=100,lang="en")
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d')
  if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_based.csv'), row.names=F)
  
  #merge the last extraction with storage file and remove duplicates
  stack <- read.csv(file=paste(searchterm, '_based.csv'))
  stack <- rbind(stack, df)
  stack <- subset(stack, !duplicated(stack$text))
  write.csv(stack, file=paste(searchterm, '_based.csv'), row.names=F)
  
  #tweets evaluation function
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    scores <- laply(sentences, function(sentence, pos.words, neg.words){
      sentence <- gsub('[[:punct:]]', "", sentence)
      sentence <- gsub('[[:cntrl:]]', "", sentence)
      sentence <- gsub('\\d+', "", sentence)
      sentence<-str_replace_all(sentence,"[^[:graph:]]", " ") 
      sentence <- tolower(sentence)
      word.list <- str_split(sentence, '\\s+')
      words <- unlist(word.list)
      pos.matches <- match(words, pos.words)
      neg.matches <- match(words, neg.words)
      pos.matches <- !is.na(pos.matches)
      neg.matches <- !is.na(neg.matches)
      score <- sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos.words, neg.words, .progress=.progress)
    scores.df <- data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  pos <- scan('C:/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
  neg <- scan('C:/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
  pos.words <- c(pos, 'Best','Good','Healthy','Tasty','affordable','cheaper')
  neg.words <- c(neg, 'bitter','bad','waste','dangerous','unaffordable','costly')
  Dataset <- stack
  Dataset$text <- as.factor(Dataset$text)
  scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
  write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results
  
  #total score calculation: positive / negative / neutral
  stat <- scores
  stat$created <- stack$created
  stat$created <- as.Date(stat$created)
  stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
  by.tweet <- group_by(stat, tweet, created)
  by.tweet <- summarise(by.tweet, number=n())
  write.csv(by.tweet, file=paste(searchterm, '_count.csv'), row.names=TRUE)
  
  #chart
  ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
    geom_point(aes(group=tweet, color=tweet), size=4) +
    theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
    #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
    ggtitle(searchterm)
  
  ggsave(file=paste(searchterm, '_plot.jpeg'))
  
}
search("American Cocktail") #enter keyword

