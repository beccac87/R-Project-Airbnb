#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements!
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)

#Question 1: Read in Data
bnb_data <-read.csv("~/Desktop/FIN454/final_exam_airbnb_data.csv")
dim(bnb_data)
head(bnb_data)

#Create a variable, private_stay_factor, that is private_stay as a factor variable.  private_stay is a binary variable equal to one if the Airbnb listing is for an entire place, for example, an entire cottage, house, condo, etc.  If private_stay equals zero, the Airbnb listing is for a part of a location, for example, a room in a house or part of a house, rather than the entire house.  (Note:  We will use private_stay_factor later in the test but need to create the variable early on.)
bnb_data <- bnb_data %>%
  mutate(private_stay_factor = factor(private_stay))

#Question 2
#A key variable in the data set is city, which is a character variable equal to the name of the city of the listing.  Use the table function to determine how many observations in the data are in each city:
table(bnb_data$city)
SanDiego <- 9656 / (1876 + 173 + 9656 + 4705 + 2473 + 4594 + 1319)
SanDiego

#Question 3
prob_private_stay <- mean(bnb_data$private_stay)
prob_not_private_stay <- 1 - prob_private_stay

entropy_whole_sample <- -1* (prob_private_stay*log(prob_private_stay,base = 2) + 
                               prob_not_private_stay*log(prob_not_private_stay, base = 2))

round(prob_private_stay, digits = 3)
round(entropy_whole_sample, digits = 3)

#city is SD
City_is_SD <- bnb_data %>%
  filter(city == "SanDiego")

prob_city_SD <- mean(City_is_SD$private_stay)
round(prob_city_SD, digits = 3)

prob_city_not_SD <- 1 - prob_city_SD
round(prob_city_not_SD, digits = 3)

entropy_city_SD <- -1 * (prob_city_SD*log(prob_city_SD, base = 2) +
                           prob_city_not_SD*log(prob_city_not_SD, base = 2))
round(entropy_city_SD, digits = 3) 

###not SD:
City_not_SD <- bnb_data %>%
  filter(city != "SanDiego")

prob_not_city_SD <- mean(City_not_SD$private_stay)
round(prob_not_city_SD, digits = 3)

prob_not_city_not_SD <- 1 - prob_not_city_SD
round(prob_not_city_not_SD, digits = 3)

entropy_not_city_SD <- -1 * (prob_not_city_SD*log(prob_not_city_SD, base = 2) +
                              prob_not_city_not_SD*log(prob_not_city_not_SD, base = 2))
round(entropy_not_city_SD, digits = 3)

###weighted average:
nrow(City_is_SD)
nrow(City_not_SD)

entropy_SD_split <- entropy_city_SD*(9656 / (9656 + 15140)) + entropy_not_city_SD*(9656 / (2944 + 15140))
round(entropy_SD_split,digits = 3)

##information gain
info_gain <- entropy_whole_sample - entropy_SD_split
round(info_gain, digits = 3)

#Question 5
set.seed(454001, "Mersenne-Twister", sample.kind = "Rejection")

rows <- sample(nrow(bnb_data), 0.55*nrow(bnb_data))

training_data <- bnb_data[rows,]
test_data <- bnb_data[-rows,]

head(rows, 5)
dim(training_data)
dim(test_data)

#Question 6
tree_1 <- rpart(private_stay ~ accommodates + bedrooms + host_listings_count +
                  minimum_nights + maximum_nights + number_of_reviews +
                  review_scores_rating + city, 
                data = training_data , method = "class", cp = 0.005) 

#print(tree_1)
#rpart.plot(tree_1)

#Question 7
#Use your classification tree and testing data to predict the outcome (whether a listing is private, or private_stay equals 1).  Create a confusion matrix using your actual outcomes and predicted outcomes.  Fill in the blanks:
#predictions from tree:
test_data <- test_data %>%
  mutate(predict_1 =  predict(tree_1, test_data, type = "class"))

confusion_matrix_1 <- table(test_data$predict_1, test_data$private_stay)
confusion_matrix_1

accuracy_1 <- sum(diag(confusion_matrix_1))  / sum(confusion_matrix_1)
accuracy_1

#Confusion Matrix
#Predicted = FALSE      #Predicted = TRUE
#Actual = FALSE    True negatives (TN)     False positives (FP)
#Actual = TRUE     False negatives (FN)    True positives (TP)

#Precision = TP / (TP + FP) 
precision <- 6306 / (6306+886)
round(precision, digits = 3)

#Recall = TP / (TP + FN)
recall <- 6306 / (948 + 6306)
round(recall, digits = 3)

#Question 9
set.seed(454001)

rf <- randomForest(private_stay_factor ~ accommodates + bedrooms + host_listings_count + minimum_nights +
                     maximum_nights + number_of_reviews + review_scores_rating + city,
                   data = training_data, ntree = 245, mtry = 6, proximity = TRUE)

#print(rf) #Make sure the type of forest you want is correct
varImpPlot(rf)

test_data <- test_data %>%
  mutate(rf_predictions = predict(rf, test_data, type = "class"))

confusion_matrix_rf <- table(test_data$private_stay, test_data$rf_predictions)
confusion_matrix_rf

sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)

#Question 11
plot_1 <- ggplot(data = bnb_data, aes(x = listing_price)) +
  geom_histogram(binwidth = 10) +
  theme_bw() +
  labs(x = "listing price", y = "count", title = "Histogram of Listing Price on BnB Data")
plot_1

ggsave(filename = "Desktop/plot_1.pdf",
       plot = plot_1,
       height = 6,
       width = 6,
       units = c("in"))

#Question 12
reg_1 <- lm(listing_price ~ accommodates + bedrooms  +
              host_listings_count + minimum_nights + maximum_nights +
              number_of_reviews + review_scores_rating + city + private_stay, 
            data = training_data)

round(reg_1$coeff, digits = 2)
summary(reg_1)

#Question 13
test_data <- test_data %>%
  mutate(reg_1_predictions = predict(reg_1, test_data)) %>%
  mutate(reg_1_error = listing_price - reg_1_predictions)

round(sqrt(mean((test_data$reg_1_error)^2)), digits = 3)

#Question 15
reg_tree_1 <- rpart(listing_price ~ accommodates + bedrooms  + host_listings_count +
                      minimum_nights + maximum_nights + number_of_reviews +
                      review_scores_rating + city + private_stay, 
                    data = training_data, method = "anova")

rpart.plot(reg_tree_1)

#Question 16
test_data <- test_data %>%
  mutate(reg_tree_1_predictions = predict(reg_tree_1, test_data, type = "matrix")) %>%
  mutate(reg_tree_1_error = listing_price - reg_tree_1_predictions)

length(unique(test_data$reg_tree_1_predictions))
round(sqrt(mean((test_data$reg_tree_1_error)^2)), digits = 3)

#Question 17
plot_2 <- ggplot(data = test_data, aes(x = listing_price, y = reg_tree_1_predictions)) +
  geom_point() + 
  theme_bw() +
  labs(x = "Listing Price",
       y = "Predicted Listing Price", 
       title = "Predicted Listing Prices using a regression tree")
plot_2

ggsave(filename = "Desktop/plot_2.pdf",
       plot = plot_2,
       height = 6,
       width = 6,
       units = c("in"))

#Question 18
set.seed(454001)

rf_1 <- randomForest(listing_price ~ accommodates + bedrooms + host_listings_count + minimum_nights +
                     maximum_nights + number_of_reviews + review_scores_rating + city + private_stay,
                   data = training_data, ntree = 245, mtry = 7, proximity = TRUE)
varImpPlot(rf_1)

#Question 19
test_data <- test_data %>%
  mutate(rf_1_predictions = predict(rf_1, test_data, type = "response")) %>%
  mutate(rf_1_error = listing_price - rf_1_predictions)

round(sqrt(mean((test_data$rf_1_error)^2)), digits = 3)

