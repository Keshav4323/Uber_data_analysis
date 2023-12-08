# Load required libraries
library(randomForest)

# Create the dataset
data <- data.frame(
  start_date = c("01-01-2016 21:11", "01-02-2016 01:25", "01-02-2016 20:25", "01-05-2016 17:31", "01-06-2016 14:42", "01-06-2016 17:15", "01-06-2016 17:30", "01-07-2016 13:27", "01-10-2016 08:05", "01-10-2016 12:17"),
  end_date = c("01-01-2016 21:17", "01-02-2016 01:37", "01-02-2016 20:38", "01-05-2016 17:45", "01-06-2016 15:49", "01-06-2016 17:19", "01-06-2016 17:35", "01-07-2016 13:33", "01-10-2016 08:25", "01-10-2016 12:44"),
  category = c("Business", "Business", "Business", "Business", "Business", "Business", "Business", "Business", "Business", "Business"),
  start = c("Fort Pierce", "Fort Pierce", "Fort Pierce", "Fort Pierce", "Fort Pierce", "West Palm Beach", "West Palm Beach", "Cary", "Cary", "Jamaica"),
  stop = c("Fort Pierce", "Fort Pierce", "Fort Pierce", "Fort Pierce", "West Palm Beach", "West Palm Beach", "Palm Beach", "Cary", "Morrisville", "New York"),
  miles = c(5.1, 5.0, 4.8, 4.7, 63.7, 4.3, 7.1, 0.8, 8.3, 16.5),
  purpose = c("Meal/Entertain", "Meal/Entertain", "Errand/Supplies", "Meeting", "Customer Visit", "Meal/Entertain", "Meeting", "Meeting", "Meeting", "Customer Visit")
)

# Display the structure of the loaded data
str(data)

# Ensure the columns exist in the dataset
if (all(c("start", "stop") %in% names(data))) {
  # Preprocessing: Convert date columns to proper date format
  data$start_date <- as.POSIXct(data$start_date, format = "%m-%d-%Y %H:%M")
  data$end_date <- as.POSIXct(data$end_date, format = "%m-%d-%Y %H:%M")
  
  # Feature engineering: Calculate duration of travel
  data$duration <- as.numeric(difftime(data$end_date, data$start_date, units = "mins"))
  
  # Select relevant features for modeling
  features <- c("start", "stop", "duration", "miles") # Include the target variable
  
  # Subset data with selected features
  data <- data[features]
  
  # Remove rows with missing values
  data <- na.omit(data)
  
  # Splitting the data into training and testing sets
  set.seed(123)  # for reproducibility
  trainIndex <- sample(1:nrow(data), 0.8 * nrow(data))
  train_data <- data[trainIndex, -which(names(data) == "miles")] # Exclude the target variable
  test_data <- data[-trainIndex, -which(names(data) == "miles")] # Exclude the target variable
  train_label <- data[trainIndex, "miles"]
  test_label <- data[-trainIndex, "miles"]
  
  # Model training: Random Forest
  model <- randomForest(x = train_data, y = train_label)
  
  # Model evaluation
  predictions <- predict(model, newdata = test_data)
  accuracy <- sqrt(mean((predictions - test_label)^2))
  print(paste("Root Mean Squared Error:", accuracy))
} else {
  print("Columns 'start', 'stop', 'duration' do not exist in the dataset.")
}
