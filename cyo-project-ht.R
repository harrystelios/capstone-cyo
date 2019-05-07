#############################################################
# DOWNLOAD DATA, GENERATE MODELS AND EVALUATION METRICS
#############################################################

# Load libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Download Indian Liver Patient Dataset:
# http://archive.ics.uci.edu/ml/datasets/ILPD+%28Indian+Liver+Patient+Dataset%29#
# http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv

dl <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv", dl)

#Create dataframe object

dat <- read.csv(dl, col.names = c("age", "gender", "tBilirubin", "dBilirubin",
                                  "aAP", "sgptAA",
                                  "sgotAA", "tProtiens",
                                  "Albumin", "AlbuminGRatio", "Selector"))

#Check for missing values

colSums(is.na(dat))

#Remove records with the four missing values for AlbuminGRatio

dat <- dat[-which(is.na(dat$AlbuminGRatio)), ]

#Table of correlations among diagnostic test results

dat %>% select("tBilirubin", "dBilirubin",
               "aAP", "sgptAA",
               "sgotAA", "tProtiens",
               "Albumin", "AlbuminGRatio") %>% cor() %>% round(., 2)

#Trim features by correlation and convert field classifying patients by presence of liver disease into Boolean

dat <- dat %>% select("age", "gender", "tBilirubin", "aAP", "sgptAA", "tProtiens",
                      "AlbuminGRatio", "Selector") %>%
  mutate(Disease = as.factor(ifelse(Selector == 1, 0, 1))) %>% select(-Selector)

#Create training and test sets

set.seed(2019)
test_index <- createDataPartition(y = dat$Disease, times = 1, p = 0.2, list = FALSE)
train_set <- dat[-test_index, ]
test_set <- dat[test_index, ]

#Train knn model using default cross validation for a range of values for k

train_knn <- train(Disease ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(5, 71, 2)))

#View knn training cross validation results

ggplot(train_knn, highlight = TRUE)

# Train rpart model using default cross validation for a range of values for the complexity parameter.

train_rpart <- train(Disease ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.025, len = 25)),
                     data = train_set)

#View rpart training cross validation results

ggplot(train_rpart)

#View rpart training cross validation results

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

#Evaluation metrics for knn model

confusionMatrix(predict(train_knn, test_set), test_set$Disease)

#Evaluation metrics for rpart model

confusionMatrix(predict(train_rpart, test_set), test_set$Disease)


