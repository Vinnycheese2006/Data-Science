install.packages("readr")
install.packages("data.table")
install.packages("dplyr") 
library(readr)
library(data.table)
library(ggplot2)
library(dplyr)

business<- read.csv("C:/Users/vince/Downloads/OA 11.7 - yelp_academic_dataset_business.json.csv")
head(business)

ggplot(business, aes(x = state)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Number of Businesses by State", x = "State", y = "Count")


star_counts <- table(business$stars)

# Plot pie chart
pie(star_counts, labels = paste(names(star_counts), "\n", star_counts), col = rainbow(length(star_counts)),
    main = "Distribution of Star Ratings for Businesses")

ggplot(business, aes(x = as.factor(stars), y = review_count)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Review Count by Star Rating", x = "Star Rating", y = "Review Count")


subset_data <- business[business$stars %in% c(1.0, 5.0), ]
contingency_table <- table(subset_data$stars)
chi_squared_test <- chisq.test(contingency_table)
print(chi_squared_test)


#USER DATA

user_data <- read.csv("~/School work/OA 11.6 - yelp_academic_dataset_user.json (1).csv")

colnames(user_data)
correlation_matrix <- cor(user_data[, c("cool_votes", "funny_votes", "useful_votes")])

print(correlation_matrix)

lm_model <- lm(funny_votes ~ cool_votes, data = user_data)

# regression summary
summary(lm_model)

# the regression line
ggplot(user_data, aes(x = cool_votes, y = funny_votes)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Linear Regression: Funny Votes vs. Cool Votes", x = "Cool Votes", y = "Funny Votes") +
  annotate("text", x = max(user_data$cool_votes), y = max(user_data$funny_votes), 
           label = paste("y =", round(coef(lm_model)[1], 2), "+", round(coef(lm_model)[2], 2), "* x"), 
           color = "red", hjust = 1, vjust = 1)


# another regression line

lm_useful_fans <- lm(fans ~ useful_votes, data = user_data)

# regression summary
summary(lm_useful_fans)

# Plot the regression line
ggplot(user_data, aes(x = useful_votes, y = fans)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Linear Regression: Useful Votes vs. Fans", x = "Useful Votes", y = "Fans") +
  annotate("text", x = max(user_data$useful_votes), y = max(user_data$fans), 
           label = paste("y =", round(coef(lm_useful_fans)[1], 2), "+", round(coef(lm_useful_fans)[2], 2), "* x"), 
           color = "red", hjust = 1, vjust = 1)



user_data_normalized <- scale(user_data[, c("review_count", "fans")])

# k-means clustering 
set.seed(123)
kmeans_result <- kmeans(user_data_normalized, centers = 3)

# Ploting the clusters
ggplot(user_data, aes(x = review_count, y = fans, color = as.factor(kmeans_result$cluster))) +
  geom_point() +
  labs(title = "K-means Clustering: Review Count vs. Fans", x = "Review Count", y = "Fans") +
  scale_color_manual(values = c("red", "blue", "green"))





user_data_normalized_2 <- scale(user_data[, c("useful_votes", "fans")])

# k-means clustering 
kmeans_result_2 <- kmeans(user_data_normalized_2, centers = 3)

# Ploting the clusters
ggplot(user_data, aes(x = useful_votes, y = fans, color = as.factor(kmeans_result_2$cluster))) +
  geom_point() +
  labs(title = "K-means Clustering: Useful Votes vs. Fans", x = "Useful Votes", y = "Fans") +
  scale_color_manual(values = c("red", "blue", "green"))















