library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("C://Users//c//OneDrive//Bureau//Media-Shares-Prediction//OnlineNewsPopularity.csv")
summary(data)

newdata = subset(data, select = -c(url,timedelta))
View(newdata)

# Heat Map for Correlation between all variables
cormatrix <- cor(newdata)
heatmap(cormatrix)


##Plot all the variable data by histogram to check the distributions
par(mfrow=c(3,4))
for(i in 1:length(newdata)){
  hist(newdata[,i],xlab=names(newdata[i]))
}

#Plot the target variable
par(mfrow=c(1,1))	  
hist(newdata$shares,main=" Histogram on number of shares ", xlab=" Number of shares ", ylab=" Frequency " ,sub="Before transformation")


#Log transformation for the target variable and plot again

newdata$normalizedshares<-log(newdata$shares)
hist(newdata$normalizedshare,main="Histogram on number of shares",xlab="Normalized number of shares",ylab="Frequency",sub="After transformation")	

#Popularity variable

newdata$popularity <- ""

newdata$popularity[newdata$normalizedshare < 1400] <- "Unpopular"
newdata$popularity[newdata$normalizedshare >= 1400] <- "Popular"

#
MultChoiceCondense<-function(vars,indata){
  tempvar<-matrix(NaN,ncol=1,nrow=length(indata[,1]))
  dat<-indata[,vars]
  for (i in 1:length(vars)){
    for (j in 1:length(indata[,1])){
      if (dat[j,i]==1) tempvar[j]=i
    }
  }
  return(tempvar)
}

#Variable for count
newdata$count=1

#Variable to represent week days
newdata$day_of_week<-MultChoiceCondense(c("weekday_is_monday","weekday_is_tuesday","weekday_is_wednesday","weekday_is_thursday","weekday_is_friday","weekday_is_saturday","weekday_is_sunday"),data)
newdata$day_of_week[data$day_of_week == 1] <- "Monday"
newdata$day_of_week[data$day_of_week == 2] <- "Tuesday"
newdata$day_of_week[data$day_of_week == 3] <- "Wednesday"
newdata$day_of_week[data$day_of_week == 4] <- "Thursday"
newdata$day_of_week[data$day_of_week == 5] <- "Friday"
newdata$day_of_week[data$day_of_week == 6] <- "Saturday"
newdata$day_of_week[data$day_of_week == 7] <- "Sunday"

#Variable to represent channel category
newdata$category<-MultChoiceCondense(c("data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world"),data)
newdata$category[newdata$category == 1] <- "lifestyle"
newdata$category[newdata$category == 2] <- "entertainment"
newdata$category[newdata$category == 3] <- "bus"
newdata$category[newdata$category == 4] <- "socmed"
newdata$category[newdata$category == 5] <- "tech"
newdata$category[newdata$category == 6] <- "world"


            #####statistics on tokens ######

plot(x=newdata$n_tokens_title,y=newdata$normalizedshares)
#normal repartition, the optimal number of words seems centered around 10-11

plot(x=newdata$n_tokens_content,y=newdata$normalizedshares)
#short articles are shared more than long articles

plot(x=newdata$n_unique_tokens,y=newdata$normalizedshares) #nothing to observe

    ###### statistics on Week Days #######

plt1<-ggplot(data=newdata, aes(x=day_of_week, y=count)) 
plt1 + geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

plt2<-ggplot(data=newdata, aes(x=day_of_week, y=newdata$normalizedshares)) 
plt2 +  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

plt3 <- ggplot(data=newdata, aes(x=day_of_week, y=count, fill=popularity)) 
plt3 + geom_bar(stat="identity",position = "fill")  #Less articles are published on weekends but more shares on weekends


     #####   statistics on channel categories    #####

plt4 <- ggplot(data=newdata[newdata$category != NaN,], aes(x=category, y=count)) 
plt4 +geom_bar(stat="identity", fill="steelblue")+ theme_minimal()


# to correct the two plots
plt5 <- ggplot(data=newdata[newdata$category != NaN,], aes(x=category, y=newdata$normalizedshare))
plt5 + geom_bar(stat="identity", fill="steelblue")+ theme_minimal()


plt6 <- ggplot(data=newdata[newdata$category != NaN,], aes(x=category, y=count, fill=popularity))
plt6 + geom_bar(stat="identity",position = "fill")







