setwd("C:\\Supriya_docs\\OneDrive - GrayMars IoT Solutions Pvt Ltd\\Supriya\\Rprog_samples\\linear_regression\\Analytics_Vidhya_Hackathon_MLR")
#install.packages("data.table")
library(data.table)     # used for reading and manipulation of data
library(cowplot)
library(ggplot2)
library(caret)
library(dplyr)
train <- fread("train_v9rqX0R.csv") # reading train data
test <- fread("test_AbJTz2l.csv") # reading test data
submission <- fread("sample_submission_8RXa3c6.csv")
str(train)
str(test)
test[,Item_Outlet_Sales := NA]
combi <- rbind(train,test)
str(combi)
#Target Variable "Item_Outlet_Sales" Histogram
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100,
                               fill = "green", xlab = "Item_Outlet_Sales") #Right skewed Histogram
#Independent variables (numeric)
p1 <- ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5,fill="blue")# No distinct pattern
sum(is.na(combi$Item_Weight)) 
p2 <- ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005,fill="blue") # Right 
# skewed with some values as 0.
prop.table(table(combi$Item_Visibility == 0)) # 6 % of the data is 0
p3 <- ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1,fill="blue") # 4 distinct segments
#Plot for Item Fat COntent
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count),stat = "identity",fill = "coral1")
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count),stat = "identity",fill = "coral1")+
  geom_label(aes(Item_Fat_Content, Count, label = Count))
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
element_text(angle = 45,hjust = 1)) + ggtitle("Outlet_Size") # missing values
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
# Plot for Outlet Location Type
p9 <- ggplot(combi %>% group_by(Outlet_Location_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Location_Type, Count),stat = "identity",fill = "coral1") +
  geom_label(aes(Outlet_Location_Type, Count, label = Count),vjust = 0.5) + 
  xlab("Outlet_Location_Type") + theme(axis.text.x = element_text(size = 8.5))

train <- combi[1:nrow(train)] # extracting train data from combined dataset
# Item_Weight vs Item_Outlet_Sales
x1 <- ggplot(train) + geom_point(aes(Item_Weight,Item_Outlet_Sales), colour = "violet",alpha = 0.3)+
  theme(axis.title = element_text(size = 0.5))
# Item_Visibility vs Item_Outlet_Sales
x2 <- ggplot(train) + geom_point(aes(Item_Visibility,Item_Outlet_Sales), colour = "violet",alpha = 0.3)+
  theme(axis.title = element_text(size = 0.5))
# Item_MRP vs Item_Outlet_Sales
x3 <- ggplot(train) + geom_point(aes(Item_MRP,Item_Outlet_Sales), colour = "violet",alpha = 0.3)+
  theme(axis.title = element_text(size = 0.5))
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
# Outlet_Size vs Item_Outlet_Sales
x7 <- ggplot(train) + geom_violin(aes(Outlet_Size,Item_Outlet_Sales), fill = "magenta")
# Outlet_Location_Type vs Item_Outlet_Sales
x8 <- ggplot(train) + geom_violin(aes(Outlet_Location_Type,Item_Outlet_Sales), fill = "magenta")

# Outlet_Type vs Item_Outlet_Sales
x9 <- ggplot(train) + geom_violin(aes(Outlet_Type,Item_Outlet_Sales), fill = "magenta")
# Missing value treatment
sum(is.na(combi$Item_Weight))
missing_index <- which(is.na(combi$Item_Weight))
for(i in missing_index) {item <- combi$Item_Identifier[i]
combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item],na.rm = TRUE)}
sum(is.na(combi$Item_Weight))
zero_index <- which(combi$Item_Visibility == 0)
for (i in zero_index) {item <- combi$Item_Identifier[i]
combi$Item_Visibility[i] <- mean(combi$Item_Visibility[combi$Item_Identifier == item],na.rm = TRUE)
}
which(combi$Item_Visibility == 0)
ggplot(combi) + geom_histogram(aes(Item_Weight),bins = 100)
ggplot(combi) + geom_histogram(aes(Item_Visibility),bins = 100)
# Feature engineering



 

