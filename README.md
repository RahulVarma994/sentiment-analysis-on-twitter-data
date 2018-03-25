code
#######################################
---
title: "Unstructured_Data"
output: 
  flexdashboard::flex_dashboard:
    orientation: row
    vertical_layout: scroll
---

```{r setup, include=FALSE}
library(flexdashboard)
library(twitteR)
library(tm)
library(dplyr)
library(ggplot2)
library(plotly)
library(wordcloud)
library(RWeka)
library(RSentiment)
library(lubridate)
api_key = "Xc3tVB1RgP111111111111111KaXAUCJS"
api_secret = "zZM9V1111111111SEcQj3gBlgWkjjC5TyoKE7eN4x4sIs"
token = "313285453-que7t11111111113zMFspldB4JUF6E"
token_secret="AhrrBVAwEZyJ2222222222FgTB51eb5"
auth = setup_twitter_oauth(api_key, api_secret, token, token_secret)
tweets = searchTwitter("#padmaavat",n=200,retryOnRateLimit = 120)
padmavat = twListToDF(tweets)
```

ROW
------------
```{r}
#Apply text cleaning process
docs=VCorpus(VectorSource(na.omit(padmavat$text)))
#For removing spaces and make it as single string
apply_regex=function(x) gsub('[^A-Za-z ]','',x) 
corpus_clean=tm_map(docs,content_transformer(apply_regex)) 
#Convert to lower case.
corpus_clean=tm_map(corpus_clean,content_transformer(tolower))
#To remove default Stop words.
corpus_clean=tm_map(corpus_clean,removeWords,stopwords()) 
dtm = DocumentTermMatrix(corpus_clean)
dtm_df= as.data.frame(as.matrix(dtm))
bagofwords = data.frame(sort(colSums(as.matrix(dtm)),decreasing = TRUE))
bagofwords$words = rownames(bagofwords)
names(bagofwords)= c("freq","words")
bag_top=head(bagofwords,50)
wordcloud(bag_top$words,bag_top$freq,colors=bag_top$freq)
```

ROW
-------------

```{r}
padmavat$sentiment=calculate_sentiment(padmavat$text)
padmavat$hour=format(strptime(padmavat$created,"%Y-%m-%d %H:%M:%S"),'%H')
padmavat$sentiments<-padmavat$sentiment$sentiment
for(i in 1:nrow(padmavat))
{
  if(padmavat[i,19]=='Very Positive')
  {
    padmavat[i,19]='Positive'
  }else if(padmavat[i,19]=='Neutral' || padmavat[i,19]=='Very Negative')
  {
    padmavat[i,19]='Negative'
  }
}
padmavat_data<-padmavat%>%group_by(hour,sentiments)%>%summarise(Total_count=n())

```


ROW
-----------------



```{r}
#Identify sentiment for each tweet and plot a stacked bar for the same.
ggplot(data = padmavat_data, aes(x =hour, y = Total_count,fill=sentiments)) + geom_bar(stat='identity')+xlab('Hours')+ylab('Total_count')
```


ROW
--------------

```{r}
#padmavat_Positive<-data.frame(padmavat$text,padmavat$sentiments)

padmaavat_posts = twListToDF(tweets)
#padmaavat_posts=read.csv("D:/unstructured data analysis/padmaavat.csv")

docs=VCorpus(VectorSource(na.omit(padmaavat_posts$text)))
#removing user defined words
custom_stop_words=c("will","rt","#")
corpus_clean=tm_map(corpus_clean,removeWords,custom_stop_words)

#apply regular expression
apply_regex=function(x) gsub('[^a-z#@ ]','',x)
corpus_clean=tm_map(corpus_clean,content_transformer(apply_regex))
#inspec
#corpus_clean=tm_map(docs,content_transformer(tolower)) #convert to lowercase
corpus_clean=tm_map(corpus_clean,removeWords,stopwords()) #removing stop words
score=c()
for(i in 1:length(corpus_clean)){
score[i]=calculate_score(corpus_clean[[i]])
}
df=data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE)
df=cbind(df,score)
df$sentiment=if_else(df$score>0,"positive",if_else(df$score==0,"neutral","negative"))
df=cbind(df,padmaavat_posts$created)
colnames(df)=c("tweet","score","sentiment","created")
df$created=as.Date(df$created)
df$hour=hour(df$created)
```

ROW
----------------------
```{r}
#word cloud based on top 50 words from those rows in which we have positive sentiment
df_sent_positive=df %>% filter(sentiment=="positive")
top_words=sort(na.omit(table(unlist(strsplit(df_sent_positive$tweet,"\\ ")))),decreasing = T) %>% head(50)
top_words_positive=as.data.frame(top_words)
colnames(top_words_positive)=c("words","freq")
wordcloud(words = top_words_positive$words,freq = top_words_positive$freq,colors = top_words_positive$freq,scale = c(9,1))
```



```{r}
#word cloud based on top 50 words from those rows in which we have Negative sentiment
df_sent_negative=df %>% filter(sentiment=="negative")
top_words=sort(na.omit(table(unlist(strsplit(df_sent_negative$tweet,"\\ ")))),decreasing = T) %>% head(50)
top_words_negative=as.data.frame(top_words)
colnames(top_words_negative)=c("words","freq")
wordcloud(words = top_words_negative$words,freq = top_words_negative$freq,colors = top_words_negative$freq,scale = c(9,1))
```

ROW
---------------
```{r}
words=sort(table(unlist(strsplit(na.omit(df$tweet),"\\ "))),decreasing = T)
words=as.data.frame(words)
colnames(words)=c("words","freq")
words$words=gsub("#","NA",words$words)
words$words=gsub(" ","NA",words$words)
words$score=calculate_score(words$words)
words=words[-1,]
words=words %>% mutate(sentiment=if_else(score<0,"negative"," "))
words=words %>% filter(sentiment=="negative")
#Search for negative words in the tweets and display the frequency of those words in a simple bar chart
words %>% head(10) %>% ggplot(aes(x=reorder(words,-freq),y=freq,fill=-freq))+geom_bar(stat = "identity")
```

