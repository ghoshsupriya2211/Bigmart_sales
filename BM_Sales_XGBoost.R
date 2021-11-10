

#install.packages("data.table")
library(data.table)     # used for reading and manipulation of data
library(cowplot)
train <- fread("train_v9rqX0R.csv") # reading train data
test <- fread("test_AbJTz2l.csv") # reading test data
submission <- fread("sample_submission_8RXa3c6.csv")
# checking dimensions
dim(train)
dim(test)
#Checking for features of data
names(train) # Item_Outlet_Sales is present in train data but not in test data
names(test) # Item_Outlet_Sales is the target variable to be predicted.

# Structure of data (short summary of features of dataframe)
str(train) # there seems to be 4 numeric and 7 categorical variables
str(test)

#Combine train and test data for combined computations/modifications
test[,Item_Outlet_Sales := NA]
combi <- rbind(train,test)
dim(combi)

#Exploratory data analysis
#Univariate Analysis
#Target Variable "Item_Outlet_Sales" Histogram
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100,
                               fill = "green", xlab = "Item_Outlet_Sales")
# The above histogram seems to be right skewed and would need some 
# data transformation to treat its skewness.

#Independent variables (numeric)
p1 <- ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5,fill="blue") # no clear cut 
# pattern seen in the distribution
p2 <- ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005,fill="blue") # dist.
# is right skewed and should be transformed to curb skewness.
p3 <- ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1,fill="blue") # 4 diff. dist. 
# seen.Interesting insight. 
#install.packages("cowplot")
#library(cowplot)
plot_grid(p1,p2,p3,nrow=1)

# Independent variables (categorical)
#Plot for Item Fat COntent
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count),stat = "identity",fill = "coral1")# LF, low fat and 
# Low Fat are one category.Also reg and Regular are one category so needs to be combined together.
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count),stat = "identity",fill = "coral1")

# Plot for Item Type
p4 <- ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count),stat = "identity",fill = "coral1") + xlab("") +
  geom_label(aes(Item_Type, Count, label = Count),vjust = 0.5) + theme(axis.text.x = 
  element_text(angle = 45,hjust = 1)) + ggtitle("Item_Type")

# Plot for Outlet Identifier
p5 <- ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count),stat = "identity",fill = "coral1") + xlab("") +
  geom_label(aes(Outlet_Identifier, Count, label = Count),vjust = 0.5) + theme(axis.text.x = 
 element_text(angle = 45,hjust = 1)) + ggtitle("Outlet_Identifier")

# Plot for Outlet Size
p6 <- ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count),stat = "identity",fill = "coral1") + xlab("") +
  geom_label(aes(Outlet_Size, Count, label = Count),vjust = 0.5) + theme(axis.text.x = 
element_text(angle = 45,hjust = 1)) + ggtitle("Outlet_Size") # Outlet_Size seems 
# missing for 4016 observations
second_row <- plot_grid(p5,p6,nrow = 1)
plot_grid(p4, second_row,ncol = 1 )

# Plot for Outlet Establishment Year
p7 <- ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Establishment_Year, Count),stat = "identity",fill = "coral1") +
  geom_label(aes(Outlet_Establishment_Year, Count, label = Count),vjust = 0.5) + 
  xlab("Outlet_Establishment_Year") + theme(axis.text.x = element_text(size = 8.5))

# Plot for Outlet Type
p8 <- ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count),stat = "identity",fill = "coral1") +
  geom_label(aes(Outlet_Type, Count, label = Count),vjust = 0.5) + 
  xlab("Outlet_Type") + theme(axis.text.x = element_text(size = 8.5))
plot_grid(p7,p8,ncol = 2)

# Plot for Outlet Location Type
p9 <- ggplot(combi %>% group_by(Outlet_Location_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Location_Type, Count),stat = "identity",fill = "coral1") +
  geom_label(aes(Outlet_Location_Type, Count, label = Count),vjust = 0.5) + 
  xlab("Outlet_Location_Type") + theme(axis.text.x = element_text(size = 8.5))

# Bivariate Analysis
train <- combi[1:nrow(train)] # extracting train data from combined dataset
str(train)
# Target variable vs Independent numarical variable
# Item_Weight vs Item_Outlet_Sales
x1 <- ggplot(train) + geom_point(aes(Item_Weight,Item_Outlet_Sales), colour = "violet",alpha = 0.3)+
  theme(axis.title = element_text(size = 0.5)) # Removed 1463 rows containing missing values
# Item_Outlet_Sales seems to be well distributed across entire range of Item_Weight
# without any obvious pattern. 

# Item_Visibility vs Item_Outlet_Sales
x2 <- ggplot(train) + geom_point(aes(Item_Visibility,Item_Outlet_Sales), colour = "violet",alpha = 0.3)+
  theme(axis.title = element_text(size = 0.5)) # In Item_Visibility vs Item_Outlet_Sales, 
# there is a string of points of Item_Visibility at 0 which is strange as Item_Visibility 
# cannot be completely 0.we will deal with the issue later.

# Item_MRP vs Item_Outlet_Sales
x3 <- ggplot(train) + geom_point(aes(Item_MRP,Item_Outlet_Sales), colour = "violet",alpha = 0.3)+
  theme(axis.title = element_text(size = 0.5)) # in this we clearly see 4 segment of prices 
# which can be used in feature engineering to create new variable.  

second_row_2 <- plot_grid(x2,x3,ncol = 2)
plot_grid(x1,second_row_2,nrow = 2 )

#Target variable vs Categorical independent variable
# Item_Type vs Item_Outlet_Sales
x4 <- ggplot(train) + geom_violin(aes(Item_Type,Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales
x5 <- ggplot(train) + geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales
x6 <- ggplot(train) + geom_violin(aes(Outlet_Identifier,Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8.5))

#observations - 1. distribution of Item_Outlet_Sales across Item_Type is not very distinct 
# and same is the case with Item_Fat_Content.
# 2. distribution for OUT010 and OUT019 categories of Outlet_Identifier are very similar 
# but different from rest of the categories of Outlet_Identifier.

# Outlet_Size vs Item_Outlet_Sales
x7 <- ggplot(train) + geom_violin(aes(Outlet_Size,Item_Outlet_Sales), fill = "magenta")
# distribution of small Outlet_Size is almost identical to the distribution of blank category of 
# Outlet_Size hence we can substitute blanks in Outlet_Size with small.

# Outlet_Location_Type vs Item_Outlet_Sales
x8 <- ggplot(train) + geom_violin(aes(Outlet_Location_Type,Item_Outlet_Sales), fill = "magenta")

# Outlet_Type vs Item_Outlet_Sales
x9 <- ggplot(train) + geom_violin(aes(Outlet_Type,Item_Outlet_Sales), fill = "magenta")

plot_grid(x8,x9,nrow = 2)
#Observations -Tier1 & Tier3 locations of Outlet_Location_Type looks similar.In the Outlet_Type plot,
#Grocery Store has most of its data points around lower sales values compated to other categories.

summary(combi)
# Missing value treatment
sum(is.na(combi$Item_Weight))
#Imputing missing values
missing_index <- which(is.na(combi$Item_Weight))
for (i in missing_index) {item <- combi$Item_Identifier[i]
combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item],na.rm = TRUE)
}
sum(is.na(combi$Item_Weight))
# Replacing 0's in Item_Visibility variable
zero_index <- which(combi$Item_Visibility == 0)
for (i in zero_index) {item <- combi$Item_Identifier[i]
combi$Item_Visibility[i] <- mean(combi$Item_Visibility[combi$Item_Identifier == item],na.rm = TRUE)
}
# Plotting histograms after replacement of NA's and 0.
ggplot(combi) + geom_histogram(aes(Item_Weight),bins = 100)
ggplot(combi) + geom_histogram(aes(Item_Visibility),bins = 100)

# Feature engineering
# creating new features to better understand and improve the model
table(combi$Item_Type)
perishable <- c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable <- c("Baking Goods", "Canned", "Frozen Foods","Hard Drinks","Health and Hygiene",
                    "Household","Soft Drinks")
# creating new feature "Item_Type_new"
combi[,Item_Type_new :=ifelse(Item_Type %in% perishable , "perishable",
                              ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure") )]

table(combi$Item_Type,substr(combi$Item_Identifier,1,2))

# creating new variable "Item_Category"
combi[, Item_Category := substr(combi$Item_Identifier,1,2)]
combi$Item_Fat_Content[combi$Item_Category == "NC"] = "Non-edible"
# creating "Outlet_Years" for operation
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year <- as.factor(combi$Outlet_Establishment_Year)
# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

# creating new independent variable - "Item_MRP_Clusters"
combi[, Item_MRP_Clusters := ifelse(Item_MRP < 69, "1st", ifelse(Item_MRP>=69 & Item_MRP < 136,
                                                                 "2nd", ifelse(Item_MRP>=136 & Item_MRP < 203, "3rd", "4th")))]

#Encoding categorical variables as models give better results with numerical data
# Label encoding for ordinal variables(variables with some order/ranking)
# Outlet_Size and Outlet_Location_Type
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1,2))]
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0, 
                                          ifelse(Outlet_Location_Type == "Tier 2", 1,2))]
# removing categorical variables "Outlet_Size and Outlet_Location_Type from dataset
combi[,c("Outlet_Size","Outlet_Location_Type") := NULL]

# Hot encoding for converting variables into binary values
# Outlet_Establishment_Year
ohe <- dummyVars("~.", data = combi[,-c("Item_Identifier","Outlet_Establishment_Year","Item_Type")],
                 fullRank = TRUE)
ohe_df<-data.table(predict(ohe,combi[,-c("Item_Identifier","Outlet_Establishment_Year","Item_Type")]))
combi <- cbind(combi[,"Item_Identifier"],ohe_df)
str(combi)

# Data Preprocessing
#Treating data for removing skewness
combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by 0.
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]


#Scaling numeric predictors
num_vars <- which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names <- names(num_vars)
combi_numeric <- combi[,setdiff(num_vars_names,"Item_Outlet_Sales"),with = F]
library(caret)
prep_num <- preProcess(combi_numeric, method = c("center","scale"))
combi_numeric_norm <- predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names,"Item_Outlet_Sales"):=NULL] # removing numeric 
# independent  variables
combi <- cbind(combi,combi_numeric_norm)

# splitting combi data back into train and test set
train <- combi[1:nrow(train)]
test <- combi[(nrow(train)+1):nrow(combi)]
test[,Item_Outlet_Sales := NULL] #removing Item_Outlet_Sales as it contains only NA for test dataset
str(train)
str(test)
cor_train <- cor(train[,-c("Item_Identifier")])
library(corrplot)
library(dplyr)
corrplot(cor_train,method = "pie",type = "lower",tl.cex = 0.9) # bluish pie represents 
# positive correlation and reddish represents negative correlation
#variables proce_per_unit_weight is highly correlated with Item_Weight and also 
# proce_per_unit_weight is highly correlated with Item_MRP.

#install.packages("xgboost")
library(xgboost)
# XGBoost model building
param_list <- list(objective =  "reg:linear",eta = 0.01,gamma = 1, max_depth = 6,
                   subsample = 0.8,colsample_bytree = 0.5)
x_Train <- train[, -c("Item_Identifier", "Item_Outlet_Sales")]
y_train <- train$Item_Outlet_Sales
x_test <- test[,-c("Item_Identifier")]

  
dtrain <- xgb.DMatrix(data = as.matrix(sapply(x_Train, as.numeric)), label = y_train)

# XGBoost model prediction
dtest <- xgb.DMatrix(data = as.matrix(sapply(x_test,as.numeric))

# cross validation with XGBoost 
set.seed(112)
xgbcv <- xgb.cv(data = dtrain,level = y_train, params = param_list,
                nrounds = 1000, nfold = 5, print_every_n = 10,
                early_stopping_rounds = 30, maximize = FALSE)
# Best RMSE achieved is rmse 1093.201587+30.175631 at iteration 449

# Building model using best nrounds identified above
xgb_model <- xgb.train(data = dtrain, params = param_list, nrounds = 449)

# Variable importance
var_imp <- xgb.importance(feature_names = setdiff(names(train),
             c("Item_Identifier","Item_Outlet_Sales")), model = xgb_model)
xgb.plot.importance(var_imp)
# again the features price_per_unit_wt", "Item_MRP_Clusters" and "Outlet_Years" are
#amongst the top important variables.

# XGBoost Predictions (making predictions on test data)
submission$Item_Outlet_Sales <- predict(xgb_model, dtest)

write.csv(submission, "xgb__Reg_submit.csv", row.names = FALSE)


