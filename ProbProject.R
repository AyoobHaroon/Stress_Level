# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("colorspace")
install.packages("e1071")
install.packages("psych")


# Load libraries
library(readxl)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
library(colorspace)
library(e1071)
library(psych)


install.packages("readxl")  
library(readxl)


data <- read_excel("C:/Users/ayoob/OneDrive/Desktop/ProbProject.xlsx")  




# View the dataset
head(data)
dim(data)
names(data)


# Check for missing values
sum(is.na(data))
colSums(is.na(data))

# Check for duplicate rows
duplicated_rows <- data[duplicated(data), ]
duplicated_rows
sum(duplicated(data))


# Task-02: Summary Statistics
summary(data)


# Detect outliers using IQR method
find_outliers <- function(data) {
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(data[data < lower_bound | data > upper_bound])
}

# Detect outliers for each variable
outliers_stress <- find_outliers(data$`Your Stress level:(from 1 to 10)`)
outliers_sleep <- find_outliers(data$`Average hours of sleep per night?`)
outliers_activity <- find_outliers(data$`Physical activity level:(from 1 to 10)`)
outliers_stability <- find_outliers(data$`Financial Stability:(from 1 to 10)`)
outliers_prayer <- find_outliers(data$`Relationship with prayer (Namaz):(from 1 to 10)`)

cat("Outliers in Stress Level:", outliers_stress, "\n")
cat("Outliers in Sleep:", outliers_sleep, "\n")
cat("Outliers in Physical Activity:", outliers_activity, "\n")
cat("Outliers in Financial Stability:", outliers_stability, "\n")
cat("Outliers in Prayer:", outliers_prayer, "\n")



# Task-03: Box and Whisker Plots
boxplot(data[, c("Your Stress level:(from 1 to 10)", 
                 "Average hours of sleep per night?", 
                 "Physical activity level:(from 1 to 10)", 
                 "Financial Stability:(from 1 to 10)", 
                 "Relationship with prayer (Namaz):(from 1 to 10)")], 
        col = rainbow(5), 
        main = "Boxplot of Variables", 
        names = c("Stress", "Sleep", "Activity", "Stability", "Prayer"))

# Task-04: Scatter Plot Matrix
pairs(data[, c("Your Stress level:(from 1 to 10)", 
               "Average hours of sleep per night?", 
               "Physical activity level:(from 1 to 10)", 
               "Financial Stability:(from 1 to 10)", 
               "Relationship with prayer (Namaz):(from 1 to 10)")], 
      main = "Scatter Plot Matrix")

# Simple linear regression: Stress level ~ Sleep hours
SLM <- lm(`Your Stress level:(from 1 to 10)` ~ `Average hours of sleep per night?`, data = data)
summary(SLM)


# Task-05: Multiple Linear Regression Model
MLRM <- lm(`Your Stress level:(from 1 to 10)` ~ 
             `Average hours of sleep per night?` + 
             `Physical activity level:(from 1 to 10)` + 
             `Financial Stability:(from 1 to 10)` + 
             `Relationship with prayer (Namaz):(from 1 to 10)`, 
           data = data)

# Display regression summary
summary(MLRM)

# Identify insignificant variables
significant_vars <- summary(MLRM)$coefficients[, 4] < 0.05
print(significant_vars)

# R-squared and Adjusted R-squared
cat("R-squared:", summary(MLRM)$r.squared, "\n")
cat("Adjusted R-squared:", summary(MLRM)$adj.r.squared, "\n")



# Correlation Matrix
cor_matrix <- cor(data[, c("Your Stress level:(from 1 to 10)", 
                           "Average hours of sleep per night?", 
                           "Physical activity level:(from 1 to 10)", 
                           "Financial Stability:(from 1 to 10)", 
                           "Relationship with prayer (Namaz):(from 1 to 10)")])



# Visualizations
# Histogram of Stress Levels
hist(data$`Your Stress level:(from 1 to 10)`, 
     col = "skyblue", 
     xlab = "Stress Level", 
     main = "Histogram of Stress Levels")

# Pie chart of stress levels
stress_counts <- table(data$`Your Stress level:(from 1 to 10)`)
pie(stress_counts, 
    main = "Pie Chart of Stress Levels", 
    col = rainbow(length(stress_counts)))


# Visualize Correlation Matrix as a Heatmap
heatmap(cor_matrix, 
        main = "Heatmap of Correlation Matrix", 
        col = heat.colors(10), 
        symm = TRUE)







# Scatter Plot with Regression Lines
ggplot(data, aes(x = `Average hours of sleep per night?`, y = `Your Stress level:(from 1 to 10)`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Stress vs Sleep", x = "Hours of Sleep", y = "Stress Level")


# Density plots for independent variables
ggplot(data, aes(x = `Average hours of sleep per night?`, fill = as.factor(`Your Stress level:(from 1 to 10)`))) +
  geom_density(alpha = 0.5) +
  ggtitle("Density Plot: Sleep Hours by Stress Level") +
  xlab("Average Sleep Hours") +
  ylab("Density")

# Pairplot for selected variables
pairs(data[, c(4:7)], 
      main = "Pairplot of Selected Variables",
      pch = 19)

# Correlation Panel
pairs.panels(
  data[, 4:7], 
  scale = TRUE, 
  hist.col = 'grey85', 
  bg = c("mediumseagreen", "orange2", "mediumpurple1")[cut(dataset$`Your Stress level:(from 1 to 10)`, breaks = 3)], 
  pch = 21, 
  main = "Correlation Matrix of Data"
)