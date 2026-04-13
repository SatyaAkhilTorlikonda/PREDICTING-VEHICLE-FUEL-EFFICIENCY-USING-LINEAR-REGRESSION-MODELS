# Setting the working directory
setwd("C:/Users/musun/Documents/MATH 7635/Assignments")
# Read CSV File
auto_mpg <- read.csv('auto_mpg.csv', header = TRUE)

head(auto_mpg)
length(unique(auto_mpg$carname))

#Pre-processing of the dataset
# Changing origins from 1,2,3 to USA, Europe and Asia respectively.
auto_mpg$origin[auto_mpg$origin == 1] <- "USA"
auto_mpg$origin[auto_mpg$origin == 2] <- "Europe"
auto_mpg$origin[auto_mpg$origin == 3] <- "Asia"


# Checking the structure of the dataset by using the str method.
str(auto_mpg)

# from this output we can see that the wrong dataypes are assigned to the cylinders,horsepower.we need to change the cylinders to factor type and horsepower to numeric.

#converting the cylinders column to categorical
auto_mpg$cylinders <- as.factor(auto_mpg$cylinders)

#converting the horsepower column to numeric
auto_mpg$horsepower <- as.numeric(auto_mpg$horsepower)



# again checking for the datatypes of each column
str(auto_mpg)

# Checking for NA values in each column 
null_count <- colSums(is.na(auto_mpg))
null_count
colSums(is.na(auto_mpg))
# From this we can see that there are 6 null values in the horsepower column. So we need to remove those null values

#removing null values by using the na.omit method
auto_mpg <- na.omit(auto_mpg)

#Checking for NA values again
nrow(auto_mpg)
names(auto_mpg)
str(auto_mpg)
colSums(is.na(auto_mpg))



# Full Model

M_Full <- lm(mpg~., data = auto_mpg)
summary(M_Full)
bic_full <- BIC(M_Full)
bic_full
adjr2_full <- summary(M_Full)$adj.r.squared
adjr2_full

# Reduced Model
# removing the origin variable

M_reduced1 <- lm(mpg~.-origin, data = auto_mpg)
bic_reduced1 <- BIC(M_reduced1)
bic_reduced1
adjr2_reduced1 <- summary(M_reduced1)$adj.r.squared
summary(M_reduced1)

# removing acceleration in addition to the origin variable

M_reduced2 <- lm(mpg~.-origin-acceleration, data = auto_mpg)
summary(M_reduced2)
bic_reduced2 <- BIC(M_reduced2)
bic_reduced2
adjr2_reduced2 <- summary(M_reduced2)$adj.r.squared
adjr2_reduced2

# Models with Interactions Terms

M_Int1 <- lm(mpg~.-acceleration-origin+weight:horsepower, data = auto_mpg)
bic_int1 <- BIC(M_Int1)
adjr2_int1 <- summary(M_Int1)$adj.r.squared


M_Int2 <- lm(mpg~.-acceleration-origin+ displacement:weight+horsepower:modelyear, data = auto_mpg)
bic_int2 <- BIC(M_Int2)
adjr2_int2 <- summary(M_Int2)$adj.r.squared
bic_int1
bic_int2
adjr2_int1
adjr2_int2

# Models with Higherorder terms using the polym method

mpg <- auto_mpg$mpg
# Combining the input variables and converting to data frame
data <- as.data.frame(cbind(auto_mpg$displacement, auto_mpg$horsepower, auto_mpg$weight, auto_mpg$modelyear))

# Degree 2
modelmat1 <- polym(as.matrix(data),degree = 2,raw = T)
M_High1 <- lm(mpg ~ ., data = modelmat1)
bic_deg2 <- BIC(M_High1)
bic_deg2
adjr2_deg2 <- summary(M_High1)$adj.r.squared

# Degree 3

modelmat1 <- polym(as.matrix(data),degree = 3,raw = T)
M_High2 <- lm(mpg ~ ., data = modelmat1)
bic_deg3 <- BIC(M_High2)
bic_deg3
adjr2_deg3 <- summary(M_High2)$adj.r.squared

# Degree 4

modelmat1 <- polym(as.matrix(data),degree = 4,raw = T)

M_High3 <- lm(mpg ~ ., data = modelmat1)
bic_deg4 <- BIC(M_High3)
bic_deg4
adjr2_deg4 <- summary(M_High3)$adj.r.squared
summary(M_High3)

polymodels = c("Polym(deg=2)", "Polym(deg=3)", "Polym(deg=4)")
poly_adjr2 = round(c(adjr2_deg2,adjr2_deg3,adjr2_deg4),2)
poly_bic = round(c(bic_deg2,bic_deg3,bic_deg4))
# Create a data frame
Poly_data <- data.frame(
  Model = polymodels,
  AdjR2 = poly_adjr2,
  BIC = poly_bic)
Poly_data

### Cross Validation Methods
set.seed(2024)
options(warn = -1)
train_size <- floor(0.8 * nrow(auto_mpg))  # 80% of the data
train_indices <- sample(seq_len(nrow(auto_mpg)), size = train_size)

train_data <- auto_mpg[train_indices, ]
test_data <- auto_mpg[-train_indices, ]

# Check the number of rows in each set
nrow(train_data)   
nrow(test_data)   

library(caret)
#check your data with missing values?
sum(is.na(train_data)) # No missing values


# Using cv
train_control <- trainControl(method = "cv", number = 5)

#Full Model
M_cv_Full <- train(mpg ~., data = train_data, method = "lm",trControl = train_control)
rmse_full <- M_cv_Full$results$RMSE
rmse_full
print(M_cv_Full)

# Reduced Model
M_cv_reduced1 <- train(mpg ~.-acceleration, data = train_data, method = "lm",trControl = train_control)
rmse_reduced1 <- M_cv_reduced1$results$RMSE
print(M_cv_reduced1)
M_cv_reduced2 <- train(mpg ~.-acceleration-origin, data = train_data, method = "lm",trControl = train_control)
rmse_reduced2 <- M_cv_reduced2$results$RMSE
rmse_reduced2
print(M_cv_reduced2)

# Interaction Models
M_cv_Int1 <- train(mpg~.-acceleration+weight:horsepower, data=train_data, method = "lm", trControl = train_control)
rmse_int1 <- M_cv_Int1$results$RMSE
print(M_cv_Int1)

M_cv_Int2 <- train(mpg~.-acceleration+displacement:weight+horsepower:modelyear, data=train_data, method = "lm", trControl = train_control)
print(M_cv_Int2)
rmse_int2 <- M_cv_Int2$results$RMSE

# Models with Higher order terms using the polym method

data_cv <- as.data.frame(cbind(train_data$displacement, train_data$horsepower, train_data$weight, train_data$modelyear))
colnames(data_cv) <- c("displacement","horsepower","weight", "modelyear")
# Degree 2
modelmat1_cv <- as.data.frame(polym(data_cv$displacement, data_cv$horsepower,data_cv$weight, data_cv$modelyear,degree = 2,raw = T))
modelmat1_cv$mpg <- train_data$mpg
M_High2_cv <- train(mpg ~ ., data = modelmat1_cv, method="lm", trControl=train_control)
rmse_deg2 <- M_High2_cv$results$RMSE
rmse_deg2

# Degree 3

modelmat1_cv <- as.data.frame(polym(data_cv$displacement, data_cv$horsepower,data_cv$weight, data_cv$modelyear,degree = 3,raw = T))
modelmat1_cv$mpg <- train_data$mpg

M_High3_cv <- train(mpg ~ ., data = modelmat1_cv, method="lm", trControl=train_control)
rmse_deg3 <- M_High3_cv$results$RMSE
rmse_deg3

# Degree 4

modelmat1_cv <- as.data.frame(polym(data_cv$displacement, data_cv$horsepower,data_cv$weight, data_cv$modelyear,degree = 4,raw = T))
modelmat1_cv$mpg <- train_data$mpg

M_High4_cv <- train(mpg ~ ., data = modelmat1_cv, method="lm", trControl=train_control)
rmse_deg4 <- M_High4_cv$results$RMSE
rmse_deg4

# Predictions and RMSE for Test Data
# Function to calculate RMSE for test data

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Full Model
pred_full <- predict(M_cv_Full, newdata = test_data)
rmse_test_full <- calculate_rmse(test_data$mpg, pred_full)

# Reduced Model1
pred_reduced1 <- predict(M_cv_reduced1, newdata = test_data)
rmse_test_reduced1 <- calculate_rmse(test_data$mpg, pred_reduced1)

# Reduced Model2
pred_reduced2 <- predict(M_cv_reduced2, newdata = test_data)
rmse_test_reduced2 <- calculate_rmse(test_data$mpg, pred_reduced2)

# Interaction Models
pred_int1 <- predict(M_cv_Int1, newdata = test_data)
rmse_test_int1 <- calculate_rmse(test_data$mpg, pred_int1)

pred_int2 <- predict(M_cv_Int2, newdata = test_data)
rmse_test_int2 <- calculate_rmse(test_data$mpg, pred_int2)

# Higher Order Models
# Degree 2
modelmat2_test <- as.data.frame(polym(test_data$displacement, test_data$horsepower, test_data$weight, test_data$modelyear, degree = 2, raw = TRUE))
modelmat2_test$mpg <- test_data$mpg
pred_deg2 <- predict(M_High2_cv, newdata = modelmat2_test)
rmse_test_deg2 <- calculate_rmse(test_data$mpg, pred_deg2)

# Degree 3
modelmat3_test <- as.data.frame(polym(test_data$displacement, test_data$horsepower, test_data$weight, test_data$modelyear, degree = 3, raw = TRUE))
modelmat3_test$mpg <- test_data$mpg
pred_deg3 <- predict(M_High3_cv, newdata = modelmat3_test)
rmse_test_deg3 <- calculate_rmse(test_data$mpg, pred_deg3)

# Degree 4
modelmat4_test <- as.data.frame(polym(test_data$displacement, test_data$horsepower, test_data$weight, test_data$modelyear, degree = 4, raw = TRUE))
modelmat4_test$mpg <- test_data$mpg
pred_deg4 <- predict(M_High4_cv, newdata = modelmat4_test)
rmse_test_deg4 <- calculate_rmse(test_data$mpg, pred_deg4)


# Lasso Model
library(glmnet)
# Train the model
X_train <- as.matrix(subset(train_data, select = -mpg))
y_train <- train_data$mpg
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, nfolds=5)
lasso_model$lambda.min
plot(lasso_model)
# Make predictions
y_pred <- predict(lasso_model, s = "lambda.min", newx = X_train)

# Calculate RMSE
lasso_train <- sqrt(mean((y_train- y_pred)^2))

# Prepare test data
X_test <- as.matrix(subset(test_data, select = -mpg))
y_test <- test_data$mpg
sum(is.na(X_test))
sum(is.na(y_test))
# Make predictions
y_pred <- predict(lasso_model, s = "lambda.min", newx = X_test)

# Calculate RMSE
lasso_test <- sqrt(mean((y_test - y_pred)^2))
lasso_model$lambda.min
lasso_train
lasso_test

# Compile Results
results <- data.frame(
  Model = c("Full Model", "Reduced Model1","Reduced Model2","Interaction1", "Interaction2", "Degree2", "Degree3", "Degree4", "Lasso"),
  RMSE_Train= c(rmse_full, rmse_reduced1,rmse_reduced2,rmse_int1, rmse_int2, rmse_deg2, rmse_deg3, rmse_deg4,lasso_train),
  RMSE_Test = c(rmse_test_full, rmse_test_reduced1,rmse_reduced2,rmse_test_int1, rmse_test_int2, rmse_test_deg2, rmse_test_deg3, rmse_test_deg4,lasso_test)
)

print(results)
# Best and Worst cars based on Average MPG 
car_mpg_summary <- auto_mpg %>%
  group_by(carname, origin) %>%
  summarise(mean_mpg = mean(mpg, na.rm = TRUE)) %>%
  ungroup()

best_car <- car_mpg_summary %>%
  filter(mean_mpg == max(mean_mpg, na.rm = TRUE))

# Find the car with the lowest mean MPG
worst_car <- car_mpg_summary %>%
  filter(mean_mpg == min(mean_mpg, na.rm = TRUE))

# Print the results
print("Best Car by Average MPG:")
print(best_car)

print("Worst Car by Average MPG:")
print(worst_car)


# Best Model Selection (BIC)
model_names <- c("Full Model","Reduced Model1","Reduced Model2","Intearaction Model1","Interaction Model2", "Polym(deg=2)", "Polym(deg=3)", "Polym(deg=4)")
bic_values <- round(c(bic_full,bic_reduced1,bic_reduced2,bic_int1,bic_int2,bic_deg2,bic_deg3,bic_deg4),2)
# Create a data frame
bic_data <- data.frame(
  Model = model_names,
  BIC = bic_values
)


library(ggplot2)

# BIC plot with vertically oriented text labels
ggplot(bic_data, aes(x = Model, y = BIC)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = BIC), vjust = -0.5, angle = 90) +  # Rotate text vertically
  ggtitle("Model Comparison: BIC") +
  xlab("Model") +
  ylab("BIC")

# Best Model Selection(adjr2)

adjr2_values <- round(c(adjr2_full,adjr2_reduced1,adjr2_reduced2,adjr2_int1,adjr2_int2,adjr2_deg2,adjr2_deg3,adjr2_deg4),2)
# Create a data frame
adjr2_data <- data.frame(
  Model = model_names,
  AdjR2 = adjr2_values
)


library(ggplot2)

# AdjR2 plot with vertically oriented text labels

ggplot(adjr2_data, aes(x = Model, y = AdjR2)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = AdjR2), vjust = -0.5, angle = 90) +  # Rotate text vertically
  ggtitle("Model Comparison: AdjR2") +
  xlab("Model") +
  ylab("AdjR2")

# Best Model Selection RMSE

# Create a data frame
data <- data.frame(
  Model = c("Full Model", "Reduced Model1", "Reduced Model2", 
            "Interaction1", "Interaction2", "Degree2", "Degree3", "Degree4"),
  RMSE_Train = c(rmse_full, rmse_reduced1,rmse_reduced2,rmse_int1, rmse_int2, rmse_deg2, rmse_deg3, rmse_deg4),
  RMSE_Test = c(rmse_test_full, rmse_test_reduced1,rmse_reduced2,rmse_test_int1, rmse_test_int2, rmse_test_deg2, rmse_test_deg3, rmse_test_deg4)
)

# Convert data to long format for ggplot
data_long <- reshape2::melt(data, id.vars = "Model", variable.name = "Metric", value.name = "RMSE")

# Plot the bar graph
ggplot(data_long, aes(x = Model, y = RMSE, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(RMSE, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5) + 
  theme_minimal() +
  labs(title = "Comparison of RMSE_Train and RMSE_Test",
       x = "Model",
       y = "RMSE",
       fill = "Metric") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


