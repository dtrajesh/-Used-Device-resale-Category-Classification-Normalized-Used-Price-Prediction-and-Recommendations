#---------------------------------Importing Libraries------------------------------------------
library(tidyverse)
library(dplyr)
library(caret)
library(fastDummies)
library(ggplot2)
library(gridExtra)
library(GGally)
library(MASS)
library(C50)
require(randomForest)
library(e1071)
library(rpart.plot)
library(psych)
library(plotly)
library(randomForest)
library(gains)
library(RWeka)
library(gplots)
library(plotly)
library(VIM)
library(class)
library(e1071)
library(rvest)	
library(matrixStats)
require(cluster)
library(caret)
#--------------------------------Step 1 Collecting data-----------------------------------------------
#loading the data
used_phone.df <- read.csv("used_device_data.csv")
#--------step 2 exploring and preparing data----------------------------------------------
#viewing first 6 rows of the data
head(used_phone.df)

#Getting dimensions of the data
dim(used_phone.df) #3454 rows and 15 columns

#Getting the structure of the data
str(used_phone.df)

#Creating new column resale_category
used_phone.df <- used_phone.df %>%
  mutate(resale_category = ifelse(normalized_used_price > mean(normalized_used_price, na.rm = TRUE), "high_resale", "low_resale"))

#Getting the structure of the data
str(used_phone.df)

#Making factors for target column resale_category
used_phone.df = used_phone.df %>% mutate(resale_category=factor(resale_category, 
                                                      levels=c("high_resale","low_resale"),
                                                      labels=c("high_resale","low_resale")),
                                         device_brand= as.factor(device_brand),
                                         os = as.factor(os),
                                         X4g = as.factor(X4g),
                                         X5g = as.factor(X5g))


#Check for NA values in each column
colSums(is.na(used_phone.df))

#check for zero values
colSums(used_phone.df==0,na.rm = TRUE)

#replacing the missing values using knn
set.seed(1)
used_phone.df <- kNN(used_phone.df,imp_var = FALSE)


#Summary of the numerical columns
summary(used_phone.df[,c(-1,-2,-4,-5,-16)])

#Device_Brand Column

#Device_brand vs resale_category
barplot(table(used_phone.df$resale_category,used_phone.df$device_brand),
        legend.text = TRUE,
        beside = TRUE,
        main = "Device_Brand vs resale_category",
        xlab = "device_brand",
        ylab = "resale_category")

#Device_brand vs normalized_used_price
plot_ly(used_phone.df, x = ~normalized_used_price, color = ~device_brand, type = "box")


#OS Column

#os vs resale_category
barplot(table(used_phone.df$resale_category,used_phone.df$os),
        legend.text = TRUE,
        beside = TRUE,
        main = "os vs resale_category",
        xlab = "os",
        ylab = "resale_category")

#OS vs normalized_used_price
plot_ly(used_phone.df, x = ~normalized_used_price, color = ~os, type = "box")



#Screen_size Column

#Screen_size vs resale_category
plot_ly(used_phone.df, x = ~screen_size, color = ~resale_category, type = "box")
#Screen_size vs normalized_used_price
plot_ly(data = used_phone.df, x = ~screen_size, y = ~normalized_used_price)



#X4g column
#X4g vs resale_category
barplot(table(used_phone.df$resale_category,used_phone.df$X4g),
        legend.text = TRUE,
        beside = TRUE,
        main = "X4g vs resale_category",
        xlab = "X4g",
        ylab = "resale_category")

#X4g vs normalized_used_price
plot_ly(used_phone.df, x = ~normalized_used_price, color = ~X4g, type = "box")


#X5g column
#X5g vs resale_category
barplot(table(used_phone.df$resale_category,used_phone.df$X5g),
        legend.text = TRUE,
        beside = TRUE,
        main = "X5g vs resale_category",
        xlab = "X5g",
        ylab = "resale_category")

#X5g vs normalized_used_price
plot_ly(used_phone.df, x = ~normalized_used_price, color = ~X5g, type = "box")

#rear_camera_mp Column

#rear_camera_mp vs resale_category
plot_ly(used_phone.df, x = ~rear_camera_mp, color = ~resale_category, type = "box")

#rear_camera_mp vs normalized_used_price
plot_ly(data = used_phone.df, x = ~rear_camera_mp, y = ~normalized_used_price)

#front_camera_mp Column

#front_camera_mp vs resale_category
plot_ly(used_phone.df, x = ~front_camera_mp, color = ~resale_category, type = "box")
#front_camera_mp vs normalized_used_price
plot_ly(data = used_phone.df, x = ~front_camera_mp, y = ~normalized_used_price)

#internal_memory Column

#internal_memory vs resale_category
plot_ly(used_phone.df, x = ~internal_memory, color = ~resale_category, type = "box")

#internal_memory vs normalized_used_price
plot_ly(data = used_phone.df, x = ~internal_memory, y = ~normalized_used_price)


#ram Column
#ram vs resale_category
plot_ly(used_phone.df, x = ~ram, color = ~resale_category, type = "box")

#ram vs normalized_used_price
plot_ly(data = used_phone.df, x = ~ram, y = ~normalized_used_price)



#battery Column
#battery vs resale_category
plot_ly(used_phone.df, x = ~battery, color = ~resale_category, type = "box")

#battery vs normalized_used_price
plot_ly(data = used_phone.df, x = ~battery, y = ~normalized_used_price)

#weight Column
#weight vs resale_category
plot_ly(used_phone.df, x = ~weight, color = ~resale_category, type = "box")

#weight vs normalized_used_price
plot_ly(data = used_phone.df, x = ~weight, y = ~normalized_used_price)

#release_year Column
#release_year vs resale_category
plot_ly(used_phone.df, x = ~release_year, color = ~resale_category, type = "box")

#release_year vs normalized_used_price
plot_ly(data = used_phone.df, x = ~release_year, y = ~normalized_used_price)

#days_used Column
#days_used vs resale_category
plot_ly(used_phone.df, x = ~days_used, color = ~resale_category, type = "box")

#days_used vs normalized_used_price
plot_ly(data = used_phone.df, x = ~days_used, y = ~normalized_used_price)

#normalized_new_price Column
#normalized_new_price vs resale_category
plot_ly(used_phone.df, x = ~normalized_new_price, color = ~resale_category, type = "box")

#days_used vs normalized_used_price
plot_ly(data = used_phone.df, x = ~normalized_new_price, y = ~normalized_used_price)


#Correlation  Analysis for numerical columns

cor(used_phone.df[,c(3,6:13,15,14)])

#first 6 rows of the data
head(used_phone.df)
################################################################################################################
#setting seed value for repetition
set.seed(1)

# partitioning into training (60%), validation (20%) and test (20%)

train.rows <- sample(rownames(used_phone.df),size = nrow(used_phone.df)*0.6)
valid.rows <- sample(setdiff(rownames(used_phone.df), train.rows),
                     size =nrow(used_phone.df)*0.20)
test.rows <- setdiff(rownames(used_phone.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.df <- used_phone.df[train.rows, ]
valid.df <- used_phone.df[valid.rows, ]
test.df <- used_phone.df[test.rows, ]

#Getting the dimensions of the partition data

dim(train.df)
dim(valid.df)
dim(test.df)



###################################################################################################################
#modelling for classification

#Decison tree
decision.tree.model <- C5.0(train.df[,c(-14:-16)], train.df$resale_category)
summary(decision.tree.model)

# ploting the tree
plot(decision.tree.model)

#Evaluating model performance 
#predicting on valid data
decision.tree.model.valid.pred <- predict(decision.tree.model, valid.df[,c(-14:-16)])
#confusin matrix
confusionMatrix(decision.tree.model.valid.pred,valid.df$resale_category,positive = "high_resale")



#Decison tree with trail 15
decision.tree.model.trail <- C5.0(train.df[,c(-14:-16)], train.df$resale_category,trials=15)
summary(decision.tree.model.trail)

# ploting the tree
plot(decision.tree.model.trail)

#Evaluating model performance 
#predicting on valid data
decision.tree.trail.model.valid.pred <- predict(decision.tree.model.trail, valid.df[,c(-14:-16)])
#confusin matrix
confusionMatrix(decision.tree.trail.model.valid.pred,valid.df$resale_category,positive = "high_resale")

#Logistic Regression

#Converting high_resale  to 1 and low_resale  to 0
train.df <- train.df %>% mutate( resale_category_num= ifelse(resale_category == 'high_resale', 1, 0))
valid.df <- valid.df %>% mutate( resale_category_num= ifelse(resale_category == 'high_resale', 1, 0))
test.df <- test.df %>% mutate( resale_category_num= ifelse(resale_category == 'high_resale', 1, 0))

logis.model <- glm(resale_category_num ~ .,data=train.df[,c(-14:-16)],family = "binomial")
#summary of the model
summary(logis.model)

#Predicting the validation data
model.logistic.valid.pred <- predict(logis.model, valid.df)

confusionMatrix(factor(ifelse(model.logistic.valid.pred > 0.5,'high_resale','low_resale'),labels = c('high_resale','low_resale'),levels = c('high_resale','low_resale')
),factor(valid.df$resale_category,labels = c('high_resale','low_resale'),levels = c('high_resale','low_resale')),positive = 'high_resale')

#backward selection model

#Setting full and null model
fit.null <- glm(resale_category_num ~ 1, data = train.df[,c(-14:-16)],family = "binomial")
fit.full <- glm(resale_category_num ~ . ,data = train.df[,c(-14:-16)], family = "binomial")

#Step wise logistic regression backward
model.logistic.backward <- step(fit.full, direction = "backward")

#Summary of the backward model
summary(model.logistic.backward)

#Predicting the validation data
model.logistic.backward.validation.pred <- predict(model.logistic.backward, valid.df[,c(-14,-16)], type = "response")

confusionMatrix(factor(ifelse(model.logistic.backward.validation.pred > 0.5,'high_resale','low_resale'),labels = c('high_resale','low_resale'),levels = c('high_resale','low_resale')
),factor(valid.df$resale_category,labels = c('high_resale','low_resale'),levels = c('high_resale','low_resale')),positive = 'high_resale')



#######################################################################################################################
#Modelling for regression Task

#Fitting linear model
model.linear <- lm(normalized_used_price ~ ., data = train.df[,c(-15,-16,-17)])

#Summary of the model
summary(model.linear)
#Predicting 
linear.pred.valid <- predict(model.linear, valid.df[,c(-15,-16,-17)])
#Rmse
caret::RMSE(linear.pred.valid, valid.df$normalized_used_price)

#Improving the model performance 
#Setting full and null model
linear.fit.null <- lm(normalized_used_price ~ 1, data = train.df[,c(-15,-16,-17)])
linear.fit.full <- lm(normalized_used_price ~ . ,data = train.df[,c(-15,-16,-17)])

#Step wise logistic regression backward
model.linear.backward <- step(linear.fit.full, direction = "backward")

#Summary of the backward model
summary(model.linear.backward)

#Predicting the validation data 
linear.backward.pred.valid <- predict(model.linear.backward, valid.df[,c(-15,-16,-17)])
#Rmse
caret::RMSE(linear.backward.pred.valid, valid.df$normalized_used_price)

#Decision Tree for regression 
model.regression.rpart <-rpart(normalized_used_price ~ ., data = train.df[,c(-15,-16,-17)])
#summary of the tree
summary(model.regression.rpart)

#plotting the tree
rpart.plot(model.regression.rpart)
#Rules
rpart.rules(model.regression.rpart)
#Predicting the validation data
model.rpart.valid.pred <- predict(model.regression.rpart, valid.df[,c(-15,-16,-17)])

#Rmse
caret::RMSE(model.rpart.valid.pred, valid.df$normalized_used_price)

#Improving Performance 
Rmse_list <- list()
cp_values_list <- list()
cp_values <- seq(0.02, 0.1, by = 0.01)
for (cp in cp_values) {
  model.regression.rpart.cp <-rpart(normalized_used_price ~ ., data = train.df[,c(-15,-16,-17)],cp=cp)
  #Predicting the validation data
  model.rpart.valid.pred <- predict(model.regression.rpart.cp, valid.df[,c(-15,-16,-17)])
  #Rmse
  rmse <-caret::RMSE(model.rpart.valid.pred, valid.df$normalized_used_price)
  Rmse_list <- c(Rmse_list, rmse)}

#Getting the minimum Rmse value
Rmse_list

#Training the model with best cp 

model.regression.rpart.cp <-rpart(normalized_used_price ~ ., data = train.df[,c(-15,-16,-17)],cp =0.02 )

#plotting the tree
rpart.plot(model.regression.rpart.cp)
#Rules
rpart.rules(model.regression.rpart.cp)

#Predicting the validation data
model.rpart.valid.pred <- predict(model.regression.rpart.cp, valid.df[,c(-15,-16,-17)])
#Rmse
caret::RMSE(model.rpart.valid.pred, valid.df$normalized_used_price)


###############################################################################################################
#MOdelling for clustering

#Filtering high resale value mobiles in train data 
used_phone.df.high.reslae.filtered <- used_phone.df[used_phone.df$resale_category == 'high_resale', ]
dim(used_phone.df.high.reslae.filtered)

#Converting x4g and x5g columns to 0's and 1's
used_phone.df.high.reslae.filtered <- used_phone.df.high.reslae.filtered %>% mutate( X4g= ifelse(X4g == 'yes', 1, 0))
used_phone.df.high.reslae.filtered <- used_phone.df.high.reslae.filtered %>% mutate( X5g= ifelse(X5g == 'yes', 1, 0))
#Modelling for clustering
#Creating dummy columns for categorical column excluding the device_brand, 
used_phone.df.dummy <- dummy_cols(used_phone.df.high.reslae.filtered[,c(2:13)],remove_selected_columns = TRUE)

#Scaling the data 
used_phone.df.dummy.scale <- scale(used_phone.df.dummy)


#Applying K means clustering using initial k = 2
set.seed(1)
k_means.cluster.k.2<-kmeans(used_phone.df.dummy.scale,2)
summary(k_means.cluster.k.2)

#Cluster within-sum of cluster square
k_means.cluster.k.2$withinss
#Clustering Centers
k_means.cluster.k.2$size

#Bar plots 
df <- as.data.frame(t(k_means.cluster.k.2$centers))
rowNames <- rownames(df)
colnames(df) <- paste0("Cluster",c(1:2))
library(plotly)
plot_ly(df, x = rownames(df), y = ~Cluster1, type = 'bar', name = 'Cluster1') %>% 
  add_trace(y = ~Cluster2, name = 'Cluster2') %>% 
  layout(title="Explicating Derived Cluster Labels",
         yaxis = list(title = 'Cluster Centers'), barmode = 'group')

#Improving clusters performance 
dis = dist(used_phone.df.dummy.scale)
wss <- (nrow(used_phone.df.dummy.scale)-1)*mean(apply(used_phone.df.dummy.scale,2,var))
set.seed(1)
for (i in 1:30) wss[i] <- mean(kmeans(used_phone.df.dummy.scale, centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters (k)",
     ylab="Average Within−Cluster Squared Distance")

#retraining the model with best Cluster number

#Applying K means clustering using initial k = 6
set.seed(1)
k_means.cluster.k.6<-kmeans(used_phone.df.dummy.scale,6)
summary(k_means.cluster.k.6)

#Cluster within-sum of cluster square
k_means.cluster.k.6$withinss
#Clustering Centers
k_means.cluster.k.6$size

#Bar plots 
df <- as.data.frame(t(k_means.cluster.k.6$centers))
rowNames <- rownames(df)
colnames(df) <- paste0("Cluster",c(1:6))
library(plotly)
plot_ly(df, x = rownames(df), y = ~Cluster1, type = 'bar', name = 'Cluster1') %>% 
  add_trace(y = ~Cluster2, name = 'Cluster2') %>% 
  add_trace(y = ~Cluster3, name = 'Cluster3') %>% 
  add_trace(y = ~Cluster4, name = 'Cluster4') %>% 
  add_trace(y = ~Cluster5, name = 'Cluster5') %>% 
  add_trace(y = ~Cluster6, name = 'Cluster6') %>% 
  layout(title="Explicating Derived Cluster Labels",
         yaxis = list(title = 'Cluster Centers'), barmode = 'group')

# Assign cluster labels to your original data set
used_phone.df.high.reslae.filtered $ Cluster <- k_means.cluster.k.6$cluster

str(used_phone.df.high.reslae.filtered)

used_phone.df.high.reslae.filtered <- used_phone.df.high.reslae.filtered %>% mutate(Cluster = as.factor(Cluster))
used_phone.df.high.reslae.filtered.dummy <- dummy_cols(used_phone.df.high.reslae.filtered[,c(-1,-14:-17)],remove_selected_columns = TRUE)
used_phone.df.high.reslae.filtered.dummy$Cluster <- used_phone.df.high.reslae.filtered$Cluster

set.seed(1)
##Partition the data 
idx <- createDataPartition(used_phone.df.high.reslae.filtered.dummy$Cluster,p=0.90,list=FALSE)
train.clu.df <- used_phone.df.high.reslae.filtered.dummy[idx,]
test.clu.df <- used_phone.df.high.reslae.filtered.dummy[-idx,]
dim(train.clu.df)
dim(test.clu.df)

#Training the model
trControl <- trainControl(method="cv", number=10, allowParallel=TRUE)
model.knn <- train(Cluster ~ ., data=train.clu.df,
               method="knn",
               preProcess=c("center", "scale"),
               tuneGrid=expand.grid(k=seq(3, 13, 2)),
               trControl=trControl)
model.knn

#Predecting the test data
model.knn.pred <- predict(model.knn, test.clu.df)

#Confusion matrix 
confusionMatrix(model.knn.pred,test.clu.df$Cluster)


#Predicting the Test data
test.df$predicted_resale_ctegory <- predict(model.logistic.backward, test.df[,c(-14:-16)])
test.df$predicted_normalized_used_price <- predict(model.regression.rpart, test.df[,c(-15,-16,-17)])
test.df <- test.df %>% mutate(X4g= ifelse(X4g == 'yes', 1, 0))
test.df <- test.df %>% mutate(X5g= ifelse(X5g == 'yes', 1, 0))
test.df$Cluster <- predict(model.knn, dummy_cols(test.df[2:13],remove_selected_columns = TRUE))
#Converting probability to class and recoverting x4g and x5g to yes and no
test.df <- test.df %>% mutate(predicted_resale_ctegory= ifelse(predicted_resale_ctegory > 0.5,'high_resale','low_resale'))
test.df<- test.df %>% mutate(X4g=ifelse(X4g == 1,'yes','no'))
test.df<- test.df %>% mutate(X5g=ifelse(X5g == 1,'yes','no'))
head(test.df,10)

