install.packages("tidyverse")
install.packages("rpart")
install.packages('randomForest')
install.packages("stringr")
install.packages("dplyr")
library(tidyverse)
library(rpart)
library(randomForest)
library(caret)
library(stringr)
library(dplyr)

# setwd("/Users/cooperlogerfo/desktop/R_play")
# d <- read.csv(file="final_data.csv", header=TRUE, sep=",")
d <- read.csv(file="MLBFA_data.csv", header=TRUE, sep=",")
d$race <- as.factor(d$race)
d$BirthPlace <- as.character(d$BirthPlace)

birth = d$BirthPlace
birthCountry <- rep(" ", length(birth))


state_list = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC",  "FL", "GA", "HI", "ID", "IL", 
               "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
               "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", 
               "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "Puerto Rico")

canada = c("ON", "BC")

for (i in 1:length(d$name)){
  for(j in 1:52){
    if(grepl(state_list[j], birth[i]))
      birthCountry[i] <- "US"
  }
  if (grepl("DR", birth[i])){
    birthCountry[i] <- "DR"
  }
  else if (grepl("Dominican Republic", birth[i])){
    birthCountry[i] <- "DR"
  }
  else if (grepl("Venezuela", birth[i])){
    birthCountry[i] <- "Venezuela"
  }
  else if (grepl("Puerto Rico", birth[i])){
    birthCountry[i] <- "Puerto Rico"
  }
  else if (grepl("Mexico", birth[i])){
    birthCountry[i] <- "Mexico"
  }
  else if (grepl("Cuba", birth[i])){
    birthCountry[i] <- "Cuba"
  }
  else if (grepl("Japan", birth[i])){
    birthCountry[i] <- "Japan"
  }
  # else if (grepl("Netherlands", birth[i])){
  #   birthCountry[i] <- "NL"
  # }
  # else if (grepl("Australia", birth[i])){
  #   birthCountry[i] <- "NL"
  # }
  else if (grepl("US", birthCountry[i])){
    #skip
  }
  else if (grepl("CA", birthCountry[i])){
    #skip
  }
  else{
    #print(birth[i])
    birthCountry[i] <- "Other"
  }
}

#I think I actauly want this as.character
d$birthCountry <- as.factor(birthCountry)
d$US <- as.logical(str_detect(d$BirthPlace, paste(state_list, collapse='|')))
d$binaryCol <- as.factor(ifelse(d$college == "None", 0, 1))
d$target.year <- as.factor(d$target.year)

#modify potential response variables
#replacement better than ifelse in this case because we want no "else" just the same
d$contractDuration <- as.factor(ifelse(d$contractDuration == 1, 1, 0))
#log transformation of salary, salary data will naturally be right skewed as this is
aav_hsit <- hist(d$avg.value, main = "Histogram of Salaries", xlab = "AAV", ylab = "Num Contracts")
#Freedman–Diaconis rule
bw <- diff(range(d$avg.value)) / (2 * IQR(d$avg.value) / length(d$avg.value)^(1/3))
n_bin <- length(d$avg.value)/bw
d %>% ggplot(aes(x = avg.value)) +
  geom_histogram(bins = n_bin, fill = "red") +
  labs(title = "Average Annual Salary histogram", xlab = "AAV")


d$avg.value <- log(d$avg.value)

bw <- diff(range(d$avg.value)) / (2 * IQR(d$avg.value) / length(d$avg.value)^(1/3))
n_bin <- length(d$avg.value)/bw
d %>% ggplot(aes(x = avg.value)) +
  geom_histogram(bins = n_bin, fill = "red") +
  labs(title = "log(Average Annual Salary) histogram", xlab = "long(AAV)")


d %>% ggplot(aes(x = avg.value)) +
  geom_histogram(bins = 20, fill = "red") +
  labs(title = "log(Average Annual Salary) histogram", xlab = "long(AAV)")

#store data before reducing
fullData <- d

d <- d %>% dplyr::select(-c(age, experience, college, BirthPlace,
                            contractValue, name))


#select whether we'll use contract value (regerssion) or duration as response variable (classification)
# d <- d%>% dplyr::select(-c(avg.value))
d <- d%>% dplyr::select(-c(contractDuration))


#arrange by year, contract value, performance
d <- d %>% dplyr::arrange(target.year, desc(avg.value), desc(war))

#na.omit() removes too many values ~ 30% of data. so I need to do more cleaning first
#this is because I was cleaning contract value, when I should have been cleaning avg. value
#contract value has ~ 300 "minor" so when I was convertign to numeric it was converting to na


set.seed(101)
split <- sample(2, nrow(d), replace = TRUE, prob = c(0.8, 0.2))
train1 <- d[split == 1, ]
test1 <- d[split == 2,]

set.seed(202)
rf1 <- randomForest(avg.value ~ ., data = train1)
p1 <- predict(rf1, test1)

test_mse1 <- mean((p1 - test1[, "avg.value"])^2)
upper_bound <- test1$avg.value + sqrt(test_mse1)
lower_bound <- test1$avg.value - sqrt(test_mse1)
plot(p1, test1$avg.value)
abline()

#lower Mtry = less correlation between your trees. fou're taking fewer variables and each d tree
#is taking different ones. but each individual tree with be weak, low prediction power
#because it isnt learning the whole data, only taking few variables
#high cor b/w decision trees = larger error.. because they aren't learning anything new 


plot_data <-
  data.frame(
    upperB = p1 + test_mse1,
    lowerB = p1 - test_mse1,
    predicted = p1,
    observed = test1$avg.value
  )

ggplot(plot_data, aes(observed)) + 
  geom_smooth(aes(y = upperB, color = "ub"), se = FALSE) +
  geom_smooth(aes(y = lowerB, color = "lb"), se = FALSE) +
  geom_smooth(aes(y = p1, color = "p1"), se = FALSE) +
  geom_point(aes(y = p1), color = "green")

#plot in logarithmic value
ggplot(plot_data, aes(p1)) + 
  geom_smooth(aes(y = upperB, color = "blue"), se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = lowerB, color = "blue"), se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = observed, color = "obs"), se = FALSE) +
  geom_point(aes(y = observed), color = "green")

#plot in real value
ggplot(plot_data, aes(exp(p1))) + 
  geom_smooth(aes(y = exp(upperB), color = "blue"), se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = exp(lowerB), color = "blue"), se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = exp(observed), color = "obs"), se = FALSE) +
  geom_point(aes(y = exp(observed)), color = "green")


#mtry = 4 minimizes mse =  0.46802
set.seed(303)
rf2 <- randomForest(avg.value ~., mtry = 4, data = train1)
p2 <- predict(rf2, test1)
test_mse2 <- mean((p2 - test1[, "avg.value"])^2)


pitcher_d <- d[d$position == "P",]
player_d <- d[d$position != "P",]

# set.seed(404)
# split <- sample(2, nrow(pitcher_d), replace = TRUE, prob = c(0.7, 0.3))
# train_pitcher <- pitcher_d[split == 1, ]
# test_pitcher <- pitcher_d[split == 2,]
# set.seed(505)
# rf3 <- randomForest(avg.value ~. , data = train_pitcher)
# p3 <- predict(rf3, test_pitcher)
# pitcher_mse <- mean((p3 - test_pitcher[, "avg.value"])^2)
# 
# set.seed(606)
# split <- sample(2, nrow(player_d), replace = TRUE, prob = c(0.7, 0.3))
# train_player <- player_d[split == 1, ]
# test_player <- player_d[split == 2,]
# set.seed(707)
# rf4 <- randomForest(avg.value ~. , data = train_player)
# p4 <- predict(rf4, test_player)
# player_mse <- mean((p4 - test_player[, "avg.value"])^2)



set.seed(404)
split <- sample(2, nrow(d), replace = TRUE, prob = c(0.8, 0.2))
train1 <- d[split == 1, ]
test1 <- d[split == 2,]
#mtry = 4 minimizes mse =  0.46802
set.seed(505)
rf_cd <- randomForest(contractDuration ~., mtry = 4, data = train1)
p_cd <- predict(rf_cd, test1)
test_mse_cd <- mean((p_cd - test1[, "contractDuration"])^2)

rf_cd <- randomForest(factor(contractDuration) ~., data = train1)
p_cd <- predict(rf_cd, test1)
print(rf_cd)
test_mse_cd <- mean((p_cd - test1$contractDuration)^2)

