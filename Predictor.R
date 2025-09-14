library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(stringr)
library(neuralnet)
library(corrplot)
library(tseries)
library(nnfor)


setwd(file.path(getwd(), "Data"))

file_list <- list.files(path=getwd())
file_list <- rev(file_list)
df <- list()
dates <- seq(as.Date("2017-12-01"), as.Date("2023-01-01"), by = "month")

for (i in 1:length(file_list)){
  df[[i]] <- read_fwf(file_list[i], skip=1, fwf_widths(c(15, 61, 4, 4, 5, 5, 15, 4, 6, 4, 3, 6, 4), col_names = c("ID Number", "Name", "Fed", "Sex", "Tit", "WTit", "OTit", "FOA", "Rating", "Gms", "K", "B-day", "Flag")))
  df[[i]]$Name[df[[i]]$Name == "Harikrishna, P."] <- "Harikrishna, Pentala"
  df[[i]]$Name[df[[i]]$Name == "Ivanchuk, Vassily"] <- "Ivanchuk, Vasyl"
  df[[i]]$Name[df[[i]]$Name == "Shankland, Samuel"] <- "Shankland, Sam"
  df[[i]] <- df[[i]] %>%
    mutate(Date = dates[i])
}

df_top_30 <- list()
df_top_30_whole <- data.frame()
for (i in 1:length(file_list)){
  players <- df[[i]] %>%
    arrange(desc(Rating))%>%
    mutate(Standing = row_number())
  df_top_30[[i]] <- head(select(players, -Fed, -Sex, -Tit, -FOA, -WTit, -OTit, -K, -'B-day', -Flag), 30)
  check <- any(apply(df_top_30[[i]], 2, function(x) any(is.na(x))))
  if (check){
    print("There was a missing value")
    print(i)
  }
  df_top_30_whole <-rbind(df_top_30_whole, df_top_30[[i]])
}
print(tail(df_top_30_whole, 120), n=120)

last_df <- df[[i]] %>%
  arrange(desc(Rating))
last_df_100 <- head(last_df, 100)
last_df <- drop_na(df[[60]], Tit)

#MLP------------------------------------------------------------------------------------------------------------------------------------
training_sequence <- data.frame()
ids <- unique(df_top_30_whole$`ID Number`)


for (i in 1:length(ids)) {
  for (j in 1:length(file_list)) {
    temp <- filter(df[[j]], `ID Number` == ids[i])
    training_sequence <- rbind(training_sequence, temp)
  }
}

training_sequence <- subset(training_sequence, select = c(`ID Number`, Name, Rating, Date))
names <- unique(training_sequence$Name)

#Some players changed their name. They had the same ID still. In order to label players with their names in output table I needed to unify them
length(ids)
length(names)
print(training_sequence %>% group_by(`ID Number`) %>% distinct(Name) %>% tally(), n = 50)
print(filter(training_sequence, `ID Number` == 14100010), n = 50)
print(filter(training_sequence, `ID Number` == 2004887), n = 50)


df_top_30_predicted <- data.frame(`ID Number` = c(), Name = c(), Rating = c(), Date = c())
accuracy_df <- data.frame()
for (i in 1:length(ids)){
  print(ids[i])
  rating <- filter(training_sequence, `ID Number` == ids[i])[3]
  
  rating.ts <- ts(rating, start = c(2017, 11), frequency = 12)
  
  # Delimit training range (75%)
  rating.train <- window(rating.ts, end = c(2022,11))
  
  # Delimit testing range (25%)
  rating.test <- window(rating.ts, start = c(2022,12))
  
  #Checking for seasonality
  if(var(rating.train)[1] == 0) {
    pred_ratings <- rep(rating.train[1], 1)
  }
  else {
    model <- mlp(rating.train, hd.auto.type = "valid")
    #print(model)
    
    forecast <- forecast(model, h = 1)
    
    #plot(forecast,main="Forcasts from MLP",ylab = "Rating",xlab = "Date")
    #lines(rating.test,lty=3)
    
    #autoplot(forecast,main="Forcasts for MLP fit with 5 hidden nodes",ylab ="Rating",xlab = "Date")
    
    accuracy_df <- rbind(accuracy_df, accuracy(forecast,rating.test)[1,1:5])
    pred_ratings <- as.numeric(forecast$mean)
  }
  
  for (j in 1:length(pred_ratings)){
    temp_df <- data.frame(`ID Number` = ids[i], Name = names[i], Rating = ceiling(pred_ratings[j]), Date = dates[j+61])      
    df_top_30_predicted <- rbind(df_top_30_predicted, temp_df)
  }
  
}

df_top_30_predicted <- df_top_30_predicted %>%
  arrange(desc(Rating)) %>%
  arrange(Date)

df_top_10_predicted <- list()
pred_date <- unique(df_top_30_predicted$Date)
for (i in 1:length(pred_date)) {
  df_top_10_predicted[[i]] <- head(filter(df_top_30_predicted, Date == pred_date[i]), 10)%>%
    mutate(Standing = row_number())
}
df_top_10_predicted
head(df_top_30[[62]], 10)
accurate_pred <- list()

accurate_pred <- df_top_10_predicted[[1]][df_top_10_predicted[[1]][2] == head(df_top_30[[62]], 10)[2],]
print(accurate_pred)


colnames(accuracy_df) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")

summary(accuracy_df)
