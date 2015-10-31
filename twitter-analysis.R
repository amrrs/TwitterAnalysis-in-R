library(twitteR)
library(lubridate)
library(ggplot2)
library(tm)
library(wordcloud)

consumerKey='x'
consumerSecret='x'
accesstoken ='x'
tokensecret = 'x'

#establishing connection with Twitter api

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accesstoken,
                    tokensecret)

#searchTwitter gives only a week's data
#cdTweets <- searchTwitter('from:cardekho', n=5000 ,since = '2014-09-01', until = '2015-10-27')

cdTweets2 <- userTimeline('cardekho',n=3200,includeRts = T)

cdtwt2 <- twListToDF(cdTweets2)

View(cdtwt2)

setwd('E:\\DM\\cardekho')

#write.csv(cdtwt2,'cardekho_tweets.csv',row.names = F)

#cdtwt2 <- read.csv('cardekho_tweets.csv',header = T,stringsAsFactors = F)

#extracting one-year data 

cardekho <- cdtwt2[1:2220,]

#saving a copy

write.csv(cardekho,'cardekho_twt1yr.csv',row.names = F)

View(cardekho)

cardekho <- read.csv('cardekho_twt1yr.csv',header = T,stringsAsFactors = F)

#removing extra columns

cardekho <- cardekho[,c(1,3,4,5,12,13)] 



#date-time changing timezone to IST

cardekho$dt <- with_tz(ymd_hms(cardekho$created),'Asia/Calcutta')


#calculating time difference between two tweets

for(i in 1:2219){
 cardekho$timedif[i] = round(as.numeric(difftime(cardekho$dt[i],cardekho$dt[i+1],units='mins')),2)
 cardekho$timedif[2220] = 0
}

#cardekho <- read.csv('cardekho_update2.csv',header = T)


cardekho$weekday = weekdays(cardekho$dt)

cardekho$weekday = as.factor(cardekho$weekday)

print(levels(cardekho$weekday))

cardekho$weekday = factor(cardekho$weekday,levels(cardekho$weekday)[c(4,2,6,7,5,1,3)])


#extracting time

cardekho$time = hms(sapply(strsplit(as.character(cardekho$dt)," "),'[',2))

#labeling time period of day

cardekho$tinterval[cardekho$time>hms('05:00:01')&cardekho$time<hms('09:30:00')] = 'morning'
cardekho$tinterval[cardekho$time>hms('09:30:01')&cardekho$time<hms('11:30:00')]= 'forenoon'
cardekho$tinterval[cardekho$time>hms('11:30:01')&cardekho$time<hms('13:30:00')] = 'midday'
cardekho$tinterval[cardekho$time>hms('13:30:01')&cardekho$time<hms('16:00:00')]= 'afternoon'
cardekho$tinterval[cardekho$time>hms('16:00:01')&cardekho$time<hms('19:00:00')] = 'evening'
cardekho$tinterval[cardekho$time>hms('19:00:01')&cardekho$time<hms('21:00:00')]= 'late evening'
cardekho$tinterval[cardekho$time>hms('21:00:01')] = 'night'
cardekho$tinterval[cardekho$time<hms('05:00:00')]= 'late night'

cardekho$tinterval = as.factor(cardekho$tinterval)
print(levels(cardekho$tinterval))
cardekho$tinterval = factor(cardekho$tinterval,levels(cardekho$tinterval)[c(7,3,6,1,2,4,8,5)])


#extracting original tweets from overall tweets

cardekho_og <- cardekho[cardekho$isRetweet=='FALSE',]

View(cardekho_og)


#visualization


x = as.data.frame(round(prop.table(table(cardekho$weekday)),2))

ggplot(x)+geom_bar(aes(x= x$Var1,y=x$Freq),stat = "identity") +
        xlab('Weekdays') + ylab('Frequency') + 
        ggtitle('Amount of Tweets in a Week')




weekday_rtcount = aggregate(cardekho_og$retweetCount~cardekho_og$weekday,data=cardekho_og,FUN=mean)


ggplot(weekday_rtcount)+geom_bar(aes(x= weekday_rtcount$`cardekho_og$weekday`,y=weekday_rtcount$`cardekho_og$retweetCount`),stat = "identity") +
        xlab('Day of the Week') + ylab('Avg. no. of RTs per tweet') + 
        ggtitle('RTs per tweet in a Week')


round(prop.table(table(cardekho$tinterval)),3)

y = as.data.frame(round(prop.table(table(cardekho$tinterval)),3))

ggplot(y)+geom_bar(aes(x= y$Var1,y=y$Freq),stat = "identity") +
        xlab('Time in Day') + ylab('Frequency') + 
        ggtitle('Amount of Tweets in a Day')

tint_rtcount = aggregate(cardekho_og$retweetCount~cardekho_og$tinterval,data=cardekho_og,FUN=mean)


ggplot(tint_rtcount)+geom_bar(aes(x= tint_rtcount$`cardekho_og$tinterval`,y=tint_rtcount$`cardekho_og$retweetCount`),stat = "identity") +
        xlab('Time Interval in the day') + ylab('Avg. no. of RTs per tweet') + 
        ggtitle('RTs per tweet')


ggplot(as.data.frame(cardekho$timedif[cardekho$timedif<1200])) + geom_histogram(aes(x=cardekho$timedif[cardekho$timedif<1200],fill = ..count..),binwidth = 5) +
        xlab('Time difference between tweets(in mins)') +
        ylab('No. of tweets') +
        ggtitle('Frequency of Tweets')


ggplot(as.data.frame(cardekho$timedif[cardekho$timedif<120])) + geom_histogram(aes(x=cardekho$timedif[cardekho$timedif<120],fill = ..count..),binwidth = 5) +
        xlab('Time difference between tweets(in mins)') +
        ylab('No. of tweets') +
        ggtitle('Frequency of Tweets (witin 2 hrs)')
    
#visualizing tweet vs RT

ggplot(cardekho) + geom_bar(aes(x=factor(1),y = ((..count..)/sum(..count..))*100,fill=factor(cardekho$isRetweet)),width=1) + 
        coord_polar(theta = 'y') +
        xlab('Percentage') +
        ylab(' ') +
        guides(fill=guide_legend(title='Is Retweet?')) + 
        ggtitle('Original Tweets vs Retweets') 
    

#extracting top-performed original tweets

cardekho_toppers <- cardekho_og[cardekho_og$retweetCount>4,]

#mining the content of top-performing tweets

txt <- Corpus(VectorSource(cardekho_toppers$text))
txt <- tm_map(txt,tolower)
txt <- tm_map(txt,removePunctuation)
txt <- tm_map(txt,removeNumbers)
stpwords <- c(stopwords('english'),'the','\n','us','cardekho')
txt <- tm_map(txt,removeWords,stpwords)
txt <- sapply(1:22, function(x){gsub("http\\w+ *", "", txt[x]$content)})
txt <- gsub("\n\\w+ *", "", txt)
wordcloud(txt,scale = c(3, 0.1),min.freq = 2,colors = c('blue','green','red'),random.color =T,max.words = 50)

cwTweets2 <- userTimeline('carwale',n=3200,includeRts = T)

cwtwt2 <- twListToDF(cwTweets2)

View(cwtwt2)

setwd('E:\\DM\\cardekho')

write.csv(cwtwt2,'carwale_tweets.csv',row.names = F)

cwtwt2 <- read.csv('carwale_tweets.csv',header = T,stringsAsFactors = F)

#extracting one-year data 

carwale <- cwtwt2[1:2560,]

#saving a copy

write.csv(carwale,'carwale_twt1yr.csv',row.names = F)

View(carwale)

carwale <- read.csv('carwale_twt1yr.csv',header = T,stringsAsFactors = F)

#removing extra columns

carwale <- carwale[,c(1,3,4,5,12,13)] 

#date-time changing timezone to IST

carwale$dt <- with_tz(ymd_hms(carwale$created),'Asia/Calcutta')


#calculating time difference between two tweets

for(i in 1:2559){
        carwale$timedif[i] = round(as.numeric(difftime(carwale$dt[i],carwale$dt[i+1],units='mins')),2)
        carwale$timedif[2560] = 0
}




carwale$weekday = weekdays(carwale$dt)

carwale$weekday = as.factor(carwale$weekday)

print(levels(carwale$weekday))

carwale$weekday = factor(carwale$weekday,levels(carwale$weekday)[c(4,2,6,7,5,1,3)])



x = as.data.frame(round(prop.table(table(carwale$weekday)),2))

ggplot(x)+geom_bar(aes(x= x$Var1,y=x$Freq),stat = "identity") +
        xlab('Weekdays') + ylab('Frequency') + 
        ggtitle('Amount of Tweets in a Week - Carwale')




weekday_rtcount = aggregate(carwale$retweetCount~carwale$weekday,data=carwale,FUN=mean)


ggplot(weekday_rtcount)+geom_bar(aes(x= weekday_rtcount$`carwale$weekday`,y=weekday_rtcount$`carwale$retweetCount`),stat = "identity") +
        xlab('Day of the Week') + ylab('Avg. no. of RTs per tweet') + 
        ggtitle('RTs per tweet in a Week - Carwale')


carwale$time = hms(sapply(strsplit(as.character(carwale$dt)," "),'[',2))


carwale$tinterval[carwale$time>hms('05:00:01')&carwale$time<hms('09:30:00')] = 'morning'
carwale$tinterval[carwale$time>hms('09:30:01')&carwale$time<hms('11:30:00')]= 'forenoon'
carwale$tinterval[carwale$time>hms('11:30:01')&carwale$time<hms('13:30:00')] = 'midday'
carwale$tinterval[carwale$time>hms('13:30:01')&carwale$time<hms('16:00:00')]= 'afternoon'
carwale$tinterval[carwale$time>hms('16:00:01')&carwale$time<hms('19:00:00')] = 'evening'
carwale$tinterval[carwale$time>hms('19:00:01')&carwale$time<hms('21:00:00')]= 'late evening'
carwale$tinterval[carwale$time>hms('21:00:01')] = 'night'
carwale$tinterval[carwale$time<hms('05:00:00')]= 'late night'

carwale$tinterval = as.factor(carwale$tinterval)
print(levels(carwale$tinterval))
carwale$tinterval = factor(carwale$tinterval,levels(carwale$tinterval)[c(7,3,6,1,2,4,8,5)])

write.csv(carwale,'carwale_update2.csv',row.names =F)

round(prop.table(table(carwale$tinterval)),3)

y = as.data.frame(round(prop.table(table(carwale$tinterval)),3))

ggplot(y)+geom_bar(aes(x= y$Var1,y=y$Freq),stat = "identity") +
        xlab('Time in Day') + ylab('Frequency') + 
        ggtitle('Amount of Tweets in a Day - Carwale')

tint_rtcount = aggregate(carwale$retweetCount~carwale$tinterval,data=carwale,FUN=mean)


ggplot(tint_rtcount)+geom_bar(aes(x= tint_rtcount$`carwale$tinterval`,y=tint_rtcount$`carwale$retweetCount`),stat = "identity") +
        xlab('Time Interval in the day') + ylab('Avg. no. of RTs per tweet') + 
        ggtitle('RTs per tweet - Carwale')


ggplot(as.data.frame(carwale$timedif[carwale$timedif<1200])) + geom_histogram(aes(x=carwale$timedif[carwale$timedif<1200],fill = ..count..),binwidth = 5) +
        xlab('Time difference between tweets(in mins)') +
        ylab('No. of tweets') +
        ggtitle('Frequency of Tweets - Carwale')


ggplot(as.data.frame(carwale$timedif[carwale$timedif<120])) + geom_histogram(aes(x=carwale$timedif[carwale$timedif<120],fill = ..count..),binwidth = 5) +
        xlab('Time difference between tweets(in mins)') +
        ylab('No. of tweets') +
        ggtitle('Frequency of Tweets (witin 2 hrs) - Carwale')

#visualizing no. of retweets


ggplot(carwale) + geom_bar(aes(x=factor(1),y = ((..count..)/sum(..count..))*100,fill=factor(carwale$isRetweet)),width=1) + 
        coord_polar(theta = 'y') +
        xlab('Percentage') +
        ylab(' ') +
        guides(fill=guide_legend(title='Is Retweet?')) + 
        ggtitle('Original Tweets vs Retweets - Carwale') 



