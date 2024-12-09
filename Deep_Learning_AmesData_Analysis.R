#Machine Learning Project for Portfolio

########################################################################################
#“live with your data before you plunge into modeling” (Breiman and others 2001, 201). 
######################################################################################


#next time, from 3.3.2 Imputation of "hands-on machine learning with R"

# My plan for this portfolio
# 1) Read "Hands on machine learning with r" and refresh my understanding. 
# 2)(project1) Using dataset in the book(ames housing dataset), try to reproduce STAT 577 homework project, 
#(follow "homework1" example as it is dealing with a regression problem), 
# from linear regression, PLS, PCA, ...... all the way to Deep Learning...
# If I do all the way through Deep learning along with all other possible ML algorithm,, this will be a solid project!!
# Then in the sample project, also add "classification" problem all the way from linear regreesion to deep learning.
# Adding all these together will make up very solid portfolio project.  
# 3)(project2) Landsat image classification project using various machine learning algorithms
# https://rstudio-conf-2020.github.io/dl-keras-tf/notebooks/01-ames.nb.html. <- refer to this for deep learning and data processing
# https://rpubs.com/ryanward2102/1020667 <- refer to this for data preprocessing. 
# https://jse.amstat.org/v19n3/decock/DataDocumentation.txt
library(dplyr) 
library(ggplot2)
library(rsample)
library(caret)
library(h2o)
library(visdat)
library(recipes)
library(modeldata)
library(ggplot2)
library(plyr)
library(PupillometryR)
library(leaps)   
library(MASS)
library(car)
library(glmnet)
library(vip)
library(pls)
library(keras)
library(tfruns)
library(tfestimators)
library(tensorflow)
library(reticulate)
library(remotes)
library(recipes)
library(randomForest)
library(tidyverse)


#######################
### Data Cleanin Up ###
#######################

#Importing Data

#importing original data from AmesHousing library
ames <- AmesHousing::make_ames()
dim(ames)

#1) Identify ordinal variables and encode them properly; turn this into integer from factor'
#2) Identify factors and one-hot encode
#3) Identify numeric variables to center and scale along with ordinal variable

#identify ordinal variables and turn them into proper ordinal levels

ames_ordinal <- ames %>% dplyr::select(Lot_Shape, Land_Slope, 
                                      Overall_Qual, Overall_Cond, 
                                      Exter_Qual, Exter_Cond,
                                      Bsmt_Qual, Bsmt_Cond, Bsmt_Exposure, 
                                      BsmtFin_Type_1, BsmtFin_Type_2,
                                      Heating_QC, Electrical, Kitchen_Qual,
                                      Functional, Fireplace_Qu, Garage_Finish,
                                      Garage_Qual,Garage_Cond, 
                                      Paved_Drive, Pool_QC, Fence)


#Lot_Shape
Qual.levels <- c('Irregular', 'Moderately_Irregular', 'Slightly_Irregular','Regular')
ames_ordinal$Lot_Shape <- factor(ames_ordinal$Lot_Shape, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Lot_Shape <- as.integer(ames_ordinal$Lot_Shape)

table(ames_ordinal$Lot_Shape)
table(ames$Lot_Shape)

#Land_Slope 
Qual.levels <- c('Gtl', 'Mod', 'Sev')
ames_ordinal$Land_Slope <- factor(ames_ordinal$Land_Slope, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Land_Slope <- as.integer(ames_ordinal$Land_Slope)

table(ames_ordinal$Land_Slope)
table(ames$Land_Slope)

#Overall_Qual
Qual.levels <- c('Very_Poor', 'Poor', 'Fair', 
                 'Below_Average', 'Average', 'Above_Average', 
                 'Good', 'Very_Good', 'Excellent', 
                 'Very_Excellent')

ames_ordinal$Overall_Qual <- factor(ames_ordinal$Overall_Qual, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Overall_Qual <- as.integer(ames_ordinal$Overall_Qual)

table(ames_ordinal$Overall_Qual)
table(ames$Overall_Qual)


#Overall_Cond
#Qual.level is same as above
ames_ordinal$Overall_Cond <- factor(ames_ordinal$Overall_Cond, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Overall_Cond <- as.integer(ames_ordinal$Overall_Cond)

table(ames_ordinal$Overall_Cond)
table(ames$Overall_Cond)

#Exter_Qual
Qual.levels <- c('Fair', 'Typical', 'Good', 'Excellent')

ames_ordinal$Exter_Qual <- factor(ames_ordinal$Exter_Qual, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Exter_Qual <- as.integer(ames_ordinal$Exter_Qual)

table(ames_ordinal$Exter_Qual)
table(ames$Exter_Qual)

#Exter_Cond
Qual.levels <- c('Poor','Fair', 'Typical', 'Good', 'Excellent')

ames_ordinal$Exter_Cond <- factor(ames_ordinal$Exter_Cond, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Exter_Cond <- as.integer(ames_ordinal$Exter_Cond)

table(ames_ordinal$Exter_Cond)
table(ames$Exter_Cond)

#Bsmt_Qual 

Qual.levels <- c('No_Basement','Poor','Fair', 'Typical', 'Good', 'Excellent')

ames_ordinal$Bsmt_Qual <- factor(ames_ordinal$Bsmt_Qual, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Bsmt_Qual <- as.integer(ames_ordinal$Bsmt_Qual)

table(ames_ordinal$Bsmt_Qual)
table(ames$Bsmt_Qual)

#Bsmt_Cond 
Qual.levels <- c('No_Basement','Poor','Fair', 'Typical', 'Good', 'Excellent')

ames_ordinal$Bsmt_Cond <- factor(ames_ordinal$Bsmt_Cond, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Bsmt_Cond <- as.integer(ames_ordinal$Bsmt_Cond)

table(ames_ordinal$Bsmt_Cond)
table(ames$Bsmt_Cond)

#Bsmt_Exposure 
Qual.levels <- c('No_Basement','No','Mn', 'Av', 'Gd')

ames_ordinal$Bsmt_Exposure <- factor(ames_ordinal$Bsmt_Exposure, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Bsmt_Exposure <- as.integer(ames_ordinal$Bsmt_Exposure)

table(ames_ordinal$Bsmt_Exposure)
table(ames$Bsmt_Exposure)

#BsmtFin_Type_1
Qual.levels <- c('No_Basement','Unf','LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ')

ames_ordinal$BsmtFin_Type_1 <- factor(ames_ordinal$BsmtFin_Type_1, levels = Qual.levels, ordered = TRUE)
ames_ordinal$BsmtFin_Type_1 <- as.integer(ames_ordinal$BsmtFin_Type_1)

table(ames_ordinal$BsmtFin_Type_1)
table(ames$BsmtFin_Type_1)

#BsmtFin_Type_2
Qual.levels <- c('No_Basement','Unf','LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ')

ames_ordinal$BsmtFin_Type_2 <- factor(ames_ordinal$BsmtFin_Type_2, levels = Qual.levels, ordered = TRUE)
ames_ordinal$BsmtFin_Type_2 <- as.integer(ames_ordinal$BsmtFin_Type_2)

table(ames_ordinal$BsmtFin_Type_2)
table(ames$BsmtFin_Type_2)

#Heating_QC
Qual.levels <- c('Poor','Fair', 'Typical', 'Good', 'Excellent')
ames_ordinal$Heating_QC <- factor(ames_ordinal$Heating_QC, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Heating_QC <- as.integer(ames_ordinal$Heating_QC)

table(ames_ordinal$Heating_QC)
table(ames$Heating_QC)

#Electrical
Qual.levels <- c('Mix', 'FuseP', 'FuseF', 'FuseA', 'SBrkr')
ames_ordinal$Electrical[ames$Electrical == 'Unknown'] <- 'SBrkr'
ames_ordinal$Electrical <- factor(ames_ordinal$Electrical, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Electrical <- as.integer(ames_ordinal$Electrical)

table(ames_ordinal$Electrical)
table(ames$Electrical)

#Kitchen_Qual
Qual.levels <- c('Poor','Fair', 'Typical', 'Good', 'Excellent')
ames_ordinal$Kitchen_Qual <- factor(ames_ordinal$Kitchen_Qual, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Kitchen_Qual <- as.integer(ames_ordinal$Kitchen_Qual)

table(ames_ordinal$Kitchen_Qual)
table(ames$Kitchen_Qual)

#Functional
Qual.levels <- c('Sal','Sev','Maj2','Maj1','Mod','Min2','Min1','Typ')
ames_ordinal$Functional <- factor(ames_ordinal$Functional, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Functional <- as.integer(ames_ordinal$Functional)

table(ames_ordinal$Functional)
table(ames$Functional)

#Fireplace_Qu 
Qual.levels <- c('No_Fireplace','Poor','Fair', 'Typical', 'Good', 'Excellent')
ames_ordinal$Fireplace_Qu <- factor(ames_ordinal$Fireplace_Qu, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Fireplace_Qu <- as.integer(ames_ordinal$Fireplace_Qu)

table(ames_ordinal$Fireplace_Qu)
table(ames$Fireplace_Qu)

#Garage_Finish
Qual.levels <- c('No_Garage','Unf','RFn', 'Fin')
ames_ordinal$Garage_Finish <- factor(ames_ordinal$Garage_Finish, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Garage_Finish <- as.integer(ames_ordinal$Garage_Finish)

table(ames_ordinal$Garage_Finish)
table(ames$Garage_Finish)

#Garage_Qual
Qual.levels <- c('No_Garage','Poor','Fair', 'Typical', 'Good', 'Excellent')
ames_ordinal$Garage_Qual <- factor(ames_ordinal$Garage_Qual, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Garage_Qual <- as.integer(ames_ordinal$Garage_Qual)

table(ames_ordinal$Garage_Qual)
table(ames$Garage_Qual)

#Garage_Cond 
Qual.levels <- c('No_Garage','Poor','Fair', 'Typical', 'Good', 'Excellent')
ames_ordinal$Garage_Cond <- factor(ames_ordinal$Garage_Cond, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Garage_Cond <- as.integer(ames_ordinal$Garage_Cond)

table(ames_ordinal$Garage_Cond)
table(ames$Garage_Cond)

#Paved_Drive
Qual.levels <- c('Dirt_Gravel','Partial_Pavement','Paved')
ames_ordinal$Paved_Drive <- factor(ames_ordinal$Paved_Drive, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Paved_Drive <- as.integer(ames_ordinal$Paved_Drive)

table(ames_ordinal$Paved_Drive)
table(ames$Paved_Drive)

#Pool_QC
Qual.levels <- c('No_Pool','Poor','Fair', 'Typical', 'Good', 'Excellent')
ames_ordinal$Pool_QC <- factor(ames_ordinal$Pool_QC, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Pool_QC <- as.integer(ames_ordinal$Pool_QC)

table(ames_ordinal$Pool_QC)
table(ames$Pool_QC)

#Fence
Qual.levels <- c('No_Fence','Minimum_Wood_Wire','Good_Wood', 'Minimum_Privacy', 'Good_Privacy')
ames_ordinal$Fence <- factor(ames_ordinal$Fence, levels = Qual.levels, ordered = TRUE)
ames_ordinal$Fence <- as.integer(ames_ordinal$Fence)

table(ames_ordinal$Fence)
table(ames$Fence)

str(ames_ordinal)

#now all the ordinal features are properly processed


##########################################################################

ames_numeric <- ames %>% dplyr::select(Lot_Frontage, Lot_Area, Year_Built, 
                                       Year_Remod_Add, Mas_Vnr_Area, BsmtFin_SF_1,
                                       BsmtFin_SF_2, Bsmt_Unf_SF, Total_Bsmt_SF, 
                                       First_Flr_SF, Second_Flr_SF, Low_Qual_Fin_SF, 
                                       Gr_Liv_Area, Garage_Area, Wood_Deck_SF, 
                                       Open_Porch_SF, Enclosed_Porch, Three_season_porch, 
                                       Screen_Porch, Pool_Area, Misc_Val)


ames_discrete <- ames %>% dplyr::select(Bsmt_Full_Bath, Bsmt_Half_Bath, Full_Bath, Half_Bath,
                                        Bedroom_AbvGr, Kitchen_AbvGr,
                                        TotRms_AbvGrd,  Fireplaces,Garage_Cars,
                                        Mo_Sold, Year_Sold)
                                        

ames_categorical <- ames %>% dplyr::select(MS_SubClass, MS_Zoning, Street, 
                                           Alley,  Land_Contour,
                                           Lot_Config,                #Utilities, <- I am removing this 
                                           Neighborhood, Condition_1, Condition_2,
                                           Bldg_Type, House_Style, 
                                           Roof_Matl, Exterior_1st,
                                           Exterior_2nd, Mas_Vnr_Type, 
                                           Foundation,
                                           Heating, 
                                           Central_Air, 
                                           Garage_Type,   
                                           Misc_Feature, 
                                           Sale_Type, Sale_Condition)

ames_response <- ames %>% dplyr::select(Sale_Price)

ames_numeric <- cbind(ames_ordinal, ames_numeric, ames_discrete)

dim(ames_numeric)

n= nrow(ames_numeric)
train_index = sample(1:n, n*0.7, replace = FALSE)


#creating one-hot encoded matrix for categorical variables.

#First method using recipe library
#blueprint <- recipe(~., data=ames_categorical) %>%
#  step_nzv(all_nominal()) %>% # removing any zero-variance features
#  step_other(all_nominal(), threshold = 0.1, other="other") %>% #condensing infrequent values into other category
#  step_dummy(all_nominal(), one_hot = TRUE) #one-hot encoding for categorical variables

#prepare <- prep(blueprint, training = ames_categorical)
#prepare

#ames_categorical_train <- bake(prepare, new_data = ames_categorical[train_index,])
#ames_categorical_test <- bake(prepare, new_data = ames_categorical[-train_index,])


# Second, process numeric features; 
# 1) divide the dataset into training and testing dataset
# 2) calculating mean and standard deviation using only training data
# 3) Scale traring and testing data separately using only mean and std of training dataset. 
ames_numeric_train <- ames_numeric[train_index, ]
ames_numeric_test <- ames_numeric[-train_index, ]

rm(mean)
rm(sd)
mean_train <- apply(ames_numeric_train, 2, mean) #calculating mean from training data.. never use testing data for this... no info leakage
sd_train <- apply(ames_numeric_train, 2, sd) #calculating standard deviation from training data.

ames_numeric_train <- scale(ames_numeric_train, center=mean_train, scale=sd_train)
ames_numeric_test <- scale(ames_numeric_test, center=mean_train, scale=sd_train) #now numeric data is ready

dim(ames_numeric_train)
dim(ames_numeric_test)

#The following code chunk shows a different way to create 
dummy_formula <- dummyVars(~., data=ames_categorical)
ames_categorical_one_hot <- predict(dummy_formula, newdata=ames_categorical)
ames_categorical_train <- ames_categorical_one_hot[train_index,]
ames_categorical_test <- ames_categorical_one_hot[-train_index,]

#Putting numerical and categorical variables together for Deep Neural Network
ames_train_targets <- ames$Sale_Price[train_index] 
ames_test_targets <- ames$Sale_Price[-train_index]
ames_DN_train <- cbind(ames_numeric_train, ames_categorical_train) 
ames_DN_test <- cbind(ames_numeric_test, ames_categorical_test)



#https://rpubs.com/ryanward2102/1020667 <- refer to this for data preprocessing. 
#https://rstudio-conf-2020.github.io/dl-keras-tf/notebooks/01-ames.nb.html


table(ames$Utilities)


#from here tomorrow(2024/05/31)...1)do some exploratory analysis for scaled data to check if they have been properly transformed
# 2)learning DN model first.
# 3) learning ML models with pre-processed data and compare the results
# use the following websites for reference... and delete them... tomorrow
# https://rstudio-conf-2020.github.io/dl-keras-tf/notebooks/01-ames.nb.html
# https://jse.amstat.org/v19n3/decock/DataDocumentation.txt
# https://rpubs.com/ryanward2102/1020667

#######################################################################################

#################################
### Exploratory Data Analysis ###
#################################


options(scipen=999)
ggplot(ames, aes(x=Sale_Price)) + geom_histogram(bins=50, col="white")

######################
## Ordinal variables##

#Lot_Shape, Land_Slope, 
#Overall_Qual, Overall_Cond, 
#Exter_Qual, Exter_Cond,
#Bsmt_Qual, Bsmt_Cond, Bsmt_Exposure, 
#BsmtFin_Type_1, BsmtFin_Type_2,
#Heating_QC, Electrical, Kitchen_Qual,
#Functional, Fireplace_Qu, Garage_Finish,
#Garage_Qual,Garage_Cond, 
#Paved_Drive, Pool_QC, Fence

ggplot(ames_all, aes(x=Fence, y=Sale_Price)) + geom_point() + #xlim(0, 100000) +
  geom_smooth( se=TRUE)

ggplot(ames, aes(x=Bsmt_Exposure, y=Sale_Price)) + geom_point() + #xlim(0, 100000) +
  geom_smooth( se=TRUE)

## Numeric Variable
#select(Lot_Frontage, Lot_Area, Year_Built, 
#       Year_Remod_Add, Mas_Vnr_Area, BsmtFin_SF_1,
#       BsmtFin_SF_2, Bsmt_Unf_SF, Total_Bsmt_SF, 
#       First_Flr_SF, Second_Flr_SF, Low_Qual_Fin_SF, 
#       Gr_Liv_Area, Garage_Area, Wood_Deck_SF, 
#       Open_Porch_SF, Enclosed_Porch, Three_season_porch, 
#       Screen_Porch, Pool_Area, Misc_Val)

ggplot(ames_all, aes(x=Misc_Val, y=Sale_Price)) + geom_point() + #xlim(0, 100000) +
  geom_smooth( se=TRUE)

ggplot(ames, aes(x=Misc_Val, y=Sale_Price)) + geom_point() + #xlim(0, 100000) +
  geom_smooth( se=TRUE)





#visualizing distribution
#Variable 1: MS_SubClass
ggplot(data = ames) + geom_bar(mapping = aes(y = MS_SubClass))
sum(is.na(ames$MS_SubClass))
boxplot(ames$Sale_Price ~ ames$MS_SubClass)
ggplot(ames, aes(x=MS_SubClass, y=Sale_Price)) + geom_boxplot() + coord_flip()


ggplot(ames, aes(MS_SubClass, Sale_Price)) +
  geom_flat_violin(aes(fill=MS_SubClass)) +
  geom_boxplot(width=0.1, alpha=0.5) +
  coord_flip() + theme(legend.position = "none")


ggplot(ames, aes(MS_SubClass, Sale_Price)) +
  geom_violin(aes(fill=as.factor(MS_SubClass))) +
  geom_boxplot(width=0.1, alpha=0.5)+
  coord_flip() + theme(legend.position = "none")

#Variable 2: MS Zoning (Nominal)

summary(ames$MS_Zoning)
table(ames$MS_Zoning)

ggplot(data=ames) + geom_bar(mapping = aes(y = MS_Zoning))


ggplot(ames, aes(MS_Zoning, Sale_Price)) +
  geom_violin(aes(fill=as.factor(MS_Zoning))) +
  geom_boxplot(width=0.1, alpha=0.5)+
  coord_flip() + theme(legend.position = "none")

#Variable 3: Lot Frontage (continuous): linear feet of street connected to property

summary(ames$Lot_Frontage)
ames_all$Lot_Frontage
ggplot(data=ames) + geom_boxplot(mapping = aes(y = Lot_Frontage))
ggplot(ames_all, aes(x=Lot_Frontage, y=Sale_Price)) + geom_point() + #xlim(0, 200) +
geom_smooth(se=TRUE)

sum(is.na(ames$Lot_Frontage))

# Variable 4: Lot Area (Continuous): Lot size in square feet
summary(ames$Lot_Area)

ggplot(data=ames, aes(x=Lot_Area)) + geom_histogram(bins=80) + xlim(0, 25000)
ggplot(data=ames_all, aes(x=Lot_Area)) + geom_histogram(bins=80) 

ggplot(ames, aes(x=Lot_Area, y=Sale_Price)) + geom_point() + xlim(0, 100000) +
  geom_smooth( se=TRUE)

ggplot(ames_all, aes(x=Lot_Area, y=Sale_Price)) + geom_point() + xlim(0, 5) +
  geom_smooth( se=TRUE)

sub <- ames %>% filter(Lot_Area <= 25000)

cor(sub$Lot_Area, sub$Sale_Price)

#mapping is also possible as this ames dataset from "model data" includes lon, lat as well.
#and sales price was right skewewd, therefore they did log transformation. 


#################################################
### Multiple Linear Regression (MLR) Modeling ###
#################################################

#removing "Longitude" & "Latitude" columns

ames <-  AmesHousing::make_ames()


#data splitting

train_index <- sample(nrow(ames), nrow(ames)*0.7, replace=FALSE)
ames_train <- ames[train_index,]
ames_test <- ames[-train_index,] 

# * Gr_Liv_Area: above ground square footage
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)
summary(model1)


#RMSE
sigma(model1) 

#MSE
sigma(model1)^2

#confidence interval 
confint(model1, level=.95)


#multiple linear regression
model2 <- update(model1,.~.+Year_Built) #we can add additional variable to model1 using "update" function
summary(model2)
confint(model2, level=0.95)

model3 <- lm(Sale_Price ~., data=ames_train) #using all variables
summary(model3)

sigma(model3)

#train model using 10-fold cross validation 
#first using model1
set.seed(123)

cv_model1 <- caret::train(form = Sale_Price ~ Gr_Liv_Area, 
                   data = ames_train, method="lm", trControl = trainControl(method = "cv", number = 10))

cv_model1

#second using model2

cv_model2 <- caret::train(form = Sale_Price ~ Gr_Liv_Area + Year_Built, 
                          data = ames_train, method = "lm", trControl = trainControl(method = "cv", number = 10))

cv_model2


#third using model3

cv_model3 <- caret::train(form = Sale_Price ~ ., 
                   data = ames_train, method = "lm", trControl = trainControl(method = "cv", number = 10))

cv_model3



#extract out of sample performance measure

summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2, 
  model3 = cv_model3
)))


##########################################################################################
### Dimension Reduction Methods: Principal Component & Partial Least Square Regression ###
##########################################################################################

#because PCR methods works best wih numerical data, we decided to drop all factor variables from our training dataset
#numeric_cols = sapply(ames, is.numeric)
#ames_numeric = ames[,numeric_cols]
#train_index = sample(1:nrow(ames), nrow(ames)*0.7, replace = FALSE)
#ames_train_pcr = ames_numeric[train_index,]
#ames_test_pcr = ames_numeric[-train_index,]

#Principal Component Regression 
set.seed(123)
cv_model_pcr <- caret::train(
  
  Sale_Price ~ ., 
  
  data = ames_train, 
  
  method = "pcr",
  
  trControl = trainControl(method = "cv", number=10),
  
  preprocess = c("zv", "center", "scale"),
  
  tuneLength = 100
  
)



#model with lowest RMSE
cv_model_pcr$bestTune

##  ncomp
## 33    33

#results for model with lowest RMSE
cv_model_pcr$results %>% dplyr::filter(ncomp==pull(cv_model_pcr$bestTune))

# plot cross-validated RMSE
ggplot(cv_model_pcr)

#Partial Least Square Regression
set.seed(1234)
cv_model_pls <- caret::train(
  Sale_Price ~., 
  data = ames_train,
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 30
)


cv_model_pls$bestTune

## ncomp 
## 6  6 

#results for model with lowest RMSE

cv_model_pls$results %>% dplyr::filter(ncomp == pull(cv_model_pls$bestTune))

ggplot(cv_model_pls)

#Feature interpretation

vip(cv_model_pls, num_features = 20, method="model")

###################################################################
### Doing the same thing but using loop this time as in STAT577 ###
###################################################################

#as of 05/10/2024 

# 1) I need to figure out how to preprocess categorical variables presented in the AMES dataset for Deep Learning.(done)
# 2) And run deep learning model to compare with other ML models. (done)
# 3) can I include other regression models such as random forest or regression tree or boosting, etc for comparison? (done)

#(remaining things to do as of 05/10/2024)
# 3) also refer to "deep learning" section of Hands-on ML with R to improve the model(using drop out, or  batch normalization)
#-- start from deep learning fine tuning....!(done) 
# 4) choose few best performing model and go into details (as in Hands on ML with R "regularization" section)(done)
# 5) also I found AMES descriptive analysis that used a map visualization, refer to it and include it for analysis!
# 6) spam data classification ? using ML and DL??

#data for machine learning models 
ames <- AmesHousing::make_ames()

n = nrow(ames)
rep = 15

mse.train.model1 = mse.train.model2 = mse.train.model3 = 
  mse.train.lasso = mse.train.ridge = mse.train.enet = mse.train.rf= mse.train.bag =
  mse.train.pcr = mse.train.plsr = rep(0,15) #creating empty vector to save training error for each model

mse.test.model1 = mse.test.model2 = mse.test.model3 = 
  mse.test.lasso = mse.test.ridge = mse.test.enet = mse.test.rf = mse.test.bag =
  mse.test.pcr = mse.test.plsr = rep(0,15) #creating empty vector to save training error for each model

#loop
set.seed(1234)

for(i in 1:rep){
  cat("Processing loop #", i, "\n")
  train_index = sample(1:n, n*0.7, replace = FALSE)
  ames_train = ames[train_index, ]
  ames_test = ames[-train_index, ]
  X_train = model.matrix(Sale_Price ~ ., ames_train)[,-1]
  Y_train = ames_train$Sale_Price
  X_test = model.matrix(Sale_Price ~., ames_test)[,-1]
  Y_test = ames_test$Sale_Price
  # X_train = ames_ml_X[train_index, ]
  # Y_train = ames_ml_Y$Sale_Price[train_index]
  # X_test = ames_ml_X[-train_index,]
  # Y_test = ames_ml_Y$Sale_Price[-train_index]
  
  #model1
  model1 = lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)
  pred.train.model1 = predict(model1)
  pred.test.model1 = predict(model1, newdata=ames_test[,!names(ames_test)%in%c("Sale_Price")])
  mse.train.model1[i] = MAE(pred.train.model1, ames_train$Sale_Price)
  #sqrt(sum((ames_train$Sale_Price - pred.train.model1)^2)/length(ames_train$Sale_Price))
  mse.test.model1[i] = MAE(pred.test.model1, ames_test$Sale_Price)
  #sqrt(sum((ames_test$Sale_Price - pred.test.model1)^2)/length(ames_test$Sale_Price))
  
  #model2
  model2 = lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
  pred.train.model2 = predict(model2)
  pred.test.model2 = predict(model2, newdata=ames_test[,!names(ames_test)%in%c("Sale_Price")])
  mse.train.model2[i] = MAE(pred.train.model2, ames_train$Sale_Price)
  #sqrt(sum((ames_train$Sale_Price - pred.train.model2)^2)/length(ames_train$Sale_Price))
  mse.test.model2[i] = MAE(pred.test.model2, ames_test$Sale_Price)
  #sqrt(sum((ames_test$Sale_Price - pred.test.model2)^2)/length(ames_test$Sale_Price))
  
  #model3
  model3 <- glmnet(x=X_train, y=Y_train, lambda = 0, alpha=0) 
  pred.train.model3 <- predict(model3, X_train)
  pred.test.model3 <- predict(model3, X_test) 
  mse.train.model3[i] <- MAE(pred.train.model3, ames_train$Sale_Price)
  mse.test.model3[i] <- MAE(pred.test.model3, ames_test$Sale_Price)
  
  #lasso
  cvfitl = cv.glmnet(x=X_train,y=Y_train,family="gaussian",alpha=1,standardize=TRUE)
  pred.train.l = predict(cvfitl, newx = X_train, s = "lambda.min") 
  pred.test.l = predict(cvfitl, newx = X_test, s = "lambda.min") 
  mse.train.lasso[i] = MAE(pred.train.l, Y_train)
  mse.test.lasso[i] = MAE(pred.test.l, Y_test)
  
  #ridge
  cvfitr = cv.glmnet(x=X_train,y=Y_train,family="gaussian",alpha=0,standardize=TRUE)
  pred.train.r = predict(cvfitr, newx = X_train, s = "lambda.min") 
  pred.test.r = predict(cvfitr, newx = X_test, s = "lambda.min") 
  mse.train.ridge[i] = MAE(pred.train.r, Y_train)
  mse.test.ridge[i] = MAE(pred.test.r, Y_test)
  
  #ENET
  cvfiten = cv.glmnet(x=X_train,y=Y_train,family="gaussian",alpha=0.5,standardize=TRUE)
  pred.train.en = predict(cvfiten, newx = X_train, s = "lambda.min") 
  pred.test.en = predict(cvfiten, newx = X_test, s = "lambda.min") 
  mse.train.enet[i] = MAE(pred.train.en, Y_train)
  mse.test.enet[i] = MAE(pred.test.en, Y_test)
  
  #PCR
  numeric_cols = sapply(ames, is.numeric)
  ames_numeric = ames[,numeric_cols]
  ames_train_pcr = ames_numeric[train_index,]
  ames_test_pcr = ames_numeric[-train_index,]
  pcr <- pcr(Sale_Price ~., data=ames_train_pcr, scale=TRUE, ncomp=30) #ncomp has been selected through cross-validation <- need to do it again
  pcr.predict.train = predict(pcr, ncomp=30)
  pcr.predict.test = predict(pcr, newdata=ames_test_pcr, ncomp=30)
  mse.train.pcr[i] = MAE(pcr.predict.train, ames_train_pcr$Sale_Price)
  mse.test.pcr[i] = MAE(pcr.predict.test, ames_test_pcr$Sale_Price)
  
  #PLSR
  plsr <- pls::plsr(Sale_Price ~., data=ames_train_pcr, scale=TRUE, ncomp=6) #ncomp has been selected through cross-validation
  plsr.predict.train = predict(plsr, ncomp=6)
  plsr.predict.test = predict(plsr, newdata=ames_test_pcr, ncomp=6)
  mse.train.plsr[i] = MAE(plsr.predict.train, ames_train_pcr$Sale_Price)
  mse.test.plsr[i] = MAE(plsr.predict.test, ames_test_pcr$Sale_Price)
  
  #Random Forest
  rf <- randomForest(Sale_Price~., data=ames_train, mtry=8)
  rf.predict.train = predict(rf)
  rf.predict.test =predict(rf, newdata = ames_test[,!names(ames_test)%in%c("Sale_Price")])
  mse.train.rf[i] = MAE(rf.predict.train, ames_train$Sale_Price)
  mse.test.rf[i] = MAE(rf.predict.test, ames_test$Sale_Price)
  
  #bagging
  bag <- randomForest(Sale_Price~., data=ames_train, mtry=57)
  bag.predict.train = predict(bag)
  bag.predict.test =predict(bag, newdata = ames_test[,!names(ames_test)%in%c("Sale_Price")])
  mse.train.bag[i] = MAE(bag.predict.train, ames_train$Sale_Price)
  mse.test.bag[i] = MAE(bag.predict.test, ames_test$Sale_Price)
  
}


mse.train.model1
mse.train.model2
mse.train.model3
mse.train.lasso
mse.train.ridge
mse.train.enet
mse.train.pcr
mse.train.plsr
mse.test.model1
mse.test.model2
mse.test.model3
mse.test.lasso
mse.test.ridge
mse.test.enet
mse.test.pcr
mse.test.plsr
mse.test.bag

### Boxplot for mse test
mse.test.combined <- c(mse.test.model3, mse.test.lasso, mse.test.ridge, 
                       mse.test.enet, mse.test.pcr, mse.test.plsr, 
                       mse.test.rf,mse.test.bag, ames_dn_test_results)

index <- rep(c("test.model3", "test.lasso", "test.ridge", 
               "test.enet", "test.pcr", "test.plsr", 
               "test.rf","test.bag","test.DN"), 
                each=15)

mse.test.combined <- cbind(mse.test.combined, index)
mse.test.combined <- as.data.frame(mse.test.combined)
mse.test.combined$mse.test.combined <- as.numeric(mse.test.combined$mse.test.combined) 

ggplot(data=mse.test.combined, aes(x=index, y=mse.test.combined)) + geom_boxplot()

### Boxplot for mse training
mse.train.combined <- c(mse.train.model1, mse.train.model2, mse.train.model3, 
                        mse.train.lasso, mse.train.ridge, mse.train.enet, 
                        mse.train.pcr, mse.train.plsr, mse.train.rf)

index <- rep(c("train.model1", "train.model2", "train.model3", 
               "train.lasso", "train.ridge", "train.enet", 
               "train.pcr","train.plsr", "train.rf"), each=20)

mse.train.combined <- cbind(mse.train.combined, index)
mse.train.combined <- as.data.frame(mse.train.combined)
mse.train.combined$mse.train.combined <- as.numeric(mse.train.combined$mse.train.combined) 

ggplot(data=mse.train.combined, aes(x=index, y=mse.train.combined)) + geom_boxplot()

##########################################
### Deep Neural Network with Ames Data ###
##########################################

#install tensorflow with GPU support in R

ames_train_targets <- ames$Sale_Price[train_index] 
ames_test_targets <- ames$Sale_Price[-train_index]
ames_DN_train <- cbind(ames_numeric_train, ames_categorical_train) 
ames_DN_test <- cbind(ames_numeric_test, ames_categorical_test)

#remotes::install_github(sprintf("rstudio/%s", c("reticulate", "tensorflow", "keras")))
#reticulate::miniconda_uninstall()
#reticulate::install_miniconda()
#install_tensorflow(version = "2.15.0")
#install_keras(tensorflow="gpu")
#tf$config$list_physical_devices('GPU') #to check if the tensorflow in R can access to GPU
#keras::install_keras()

## Remaining things to do as of 05/12/2024
# 1) do it again "data preprocessing" : ordinal data(NA --> none if necessary), 
# 2) numeric data, factor data, ...then take care of NA 
# https://rstudio-conf-2020.github.io/dl-keras-tf/notebooks/01-ames.nb.html. <- refer to this for deep learning and data processing
# https://rpubs.com/ryanward2102/1020667 <- refer to this for data preprocessing. 

#1) separate numeric from categorical data 
#2) normalize numeric data (only using training data's mean and sd) 
#3) create one-hot vector from categorical variables
#4) combine 



## Building Deep Learning model
build_model <- function(){ #because we need to instantiate the same model multiple times, we use function to construct it.
  
  model <- keras_model_sequential() %>%
  layer_dense(128, activation = "relu", kernel_regularizer = regularizer_l2(0.001),input_shape = ncol(ames_DN_train)) %>%
  #layer_batch_normalization() %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(128, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  # layer_batch_normalization() %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(1)

  #for further refine model I could try 1) batch_normalization 2)regularization
  #3) dropout 4) adjust learning rate

  #layer_dense(128, activation = "relu", input_shape = ncol(ames_DN_train) ,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_batch_normalization() %>%
  #layer_dropout(rate=0.2) %>%
  #layer_dense(128, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_batch_normalization() %>%
  #layer_dropout(rate=0.2) %>%
  # layer_dense(256, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  # layer_batch_normalization() %>%
  # layer_dropout(rate=0.2) %>%
  #layer_dense(1)

  model %>% compile(optimizer = optimizer_adam(), 
                    #optimizer_rmsprop(learning_rate = 0.01),
                    loss = "mse",
                    metrics = "mae")
  model
  }


#Let's try training the model a bit longer: 500 epochs
#k-fold validation
k <- 5
fold_id <- sample(rep(1:k, length.out = nrow(ames_DN_train)))
num_epochs <- 2000
all_mae_histories <- list()

for(i in 1:k) {
  cat("Processing fold #", i, "\n")
  val_indices <- which(fold_id == i)
  val_data <- ames_DN_train[val_indices, ] #prepare the validation data 
  val_targets <- ames_train_targets[val_indices]
  partial_train_data <- ames_DN_train[-val_indices,]
  partial_train_targets <- ames_train_targets[-val_indices]
  
  model <- build_model()

  history <- model %>% fit(
    partial_train_data,
    partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs,
    batch_size = 32,
    # callbacks = list(
    #   #callback_early_stopping(patience = 5),
    #   callback_reduce_lr_on_plateau(factor = 0.05)
    # ),
    verbose = 0
  )
  
  mae_history <- history$metrics$val_mae
  
  all_mae_histories[[i]] <- mae_history
  
}

all_mae_histories <- do.call(cbind, all_mae_histories)
plot(history)
average_mae_history <- rowMeans(all_mae_histories) #calculating average per epoch
plot(average_mae_history, xlab="epoch",  type = 'l')


truncated_mae_history <- average_mae_history[-(1:200)]
plot(average_mae_history, xlab="epoch",  type = 'l', ylim = range(truncated_mae_history))

min <-which.min(average_mae_history)
average_mae_history[min] #901 = 13907.79

### Best model chosen by grid search
#min<- 950
model <- build_model()

history <- model %>%
  fit(ames_DN_train, ames_train_targets, epoch = min*1.2, batch_size = 32) 
      #validation_split = 0.2)

result <- model %>% evaluate(ames_DN_test, ames_test_targets)

result["mae"] 


ames_dn_test_results <- rep(0, 15)

for(i in 1:15) {
  cat("Processing Loop #", i, "\n")
  model <- build_model()
  model %>% fit(ames_DN_train, ames_train_targets, #train it on the entirety of the data
                epochs = min*1.2, batch_size = 32, verbose = 0)
  result <- model %>% evaluate(ames_DN_test, ames_test_targets)
  ames_dn_test_results[i] <- result["mae"] 
}

ames_dn_test_results
#[1] 15286.94 15094.63 15230.13 15084.27 15239.37 15230.49 15329.19 15168.20 15234.59 15150.51 15193.56 15201.99 15162.02 15099.38 15341.54
mean(ames_dn_test_results)

#after hyperparameter tuning the following has the minimum validation loss
#layer_dense(256, 128, 1), layer_dropout(0.4, 0.2), batch_size = 16, epoch = 79   <-- for this model I am getting [1] 16044.37 test error
#Another  best model was layer_dense(128, 128, 1), layer_dropout(0.4, 0.2), batch_size = 16, epoch = 113 <-- [1] 15695.08 test error
#layer_dense(128, 128, 1), layer_dropout(0.4, 0.2), batch_size = 16 , L2 regularization <-- [1] 15786.56
#layer_dense(128, 128, 1), layer_dropout(0.4, 0.2), batch_size = 16 , L2 regularization, epoch = 1000,optimizer = optimizer_adam() <-- [1] 15209.05
#layer_dense(128, 128, 1), layer_dropout(0.4, 0.2), batch_size = 32 , L2 regularization, optimizer = optimizer_adam() <-- [1] 15209.14
#optimizer_adam() looks a lot more stable than optimizer_rmsprop() so far: in other world, predictions made from the former vary a lot less
#this time, everything else is same but Y being log-transformed log(ames_train_target)


# Layer_dense(64,64,1)
#1) layer_dense(64,64,1), batch_size = 32, epoch=500  --> [1] 15690.8
#2) layer_dense(64,64,1), batch_size = 64, epoch=500  --> [1] 16374.07
#3) layer_dense(64,64,1), batch_size = 128, epoch=500 --> [1] 16218.48  # doesn't show clear overfitting pattern
#4) layer_dense(64,64,1), batch_size = 256, epoch=500 --> [1] 16074.23  # doesn't show clear overfitting pattern
#5) layer_dense(64,64,1), batch_size = 256, epoch = 1,000 --> [1] 15975.95
#6) layer_dense(64,64,1), batch_size = 128, epoch = 1,000 --> [1] 16220.99 #now it is showing overfitting pattern

#Layer_dense
#7) layer_dense(128,128,1), batch_size = 16, best_epoch = 50 --> [1] 14490.25 (the mean of 15 loops) 
#8) layer_dense(128,128,1), batch_size = 32, best_epoch = 50 --> [1] 15480.52 (the mean of 15 loops) 
#8) layer_dense(128,128,1), batch_size = 64, best_epoch = 88 --> [1] 16088.86 (the mean of 15 loops)
#8) layer_dense(128,128,1), batch_size = 128, best_epoch = 170 --> [1] 16164.65 (the mean of 15 loops)
#8) layer_dense(128,128,1), batch_size = 256, best_epoch = 340 --> [1] 17150.43 (the mean of 15 loops)

# so far, layer_dense(128,128,1), batch_size = 16, best_epoch = 50 works best  <- so I am going to apply some regularization techniques to this model
# batch_normalization, regularization, drop_out etc.,
# 1) with batch normalization for each layer <--  [1] 177239.3
# 2) with layer_dropout(rate=0.2) [1] 14677.28
# batch = 256 with drop_out(0.3) & batch_normalization  & epoch = 1000 <-- [1] 16009.38
# batch = 256 with drop_out(0.2) & batch_normalization  & epoch = 1000 <-- [1] 15855.7
# batch = 256 with drop_out(0.2) & L2 Regularization  & epoch = 1000 <-- [1] 15875.02
# batch = 256 with drop_out(0.2) & L2 Regularization  & batch_normalization & epoch = 1000 <-- [1] 15692.49
# 2) with layer_dropout(rate=0.3)   <-- [1] 15808.75
# grid search with batch_size(32,64,128), layer_dropout(0.2,0.3,0.4), & nodes unit size(32,64,128)
#training the final model

model <- build_model() 

model %>% fit(ames_DN_train, ames_train_targets, #train it on the entirety of the data
              epochs = min, batch_size = 512, verbose = 0)

result <- model %>% evaluate(ames_DN_test, ames_test_targets)

result["mae"] 



#k-fold validation
k <- 10
fold_id <- sample(rep(1:k, length.out = nrow(ames_DN_train)))
num_epochs <- 100
all_scores <- numeric()


for(i in 1:k) {
  cat("Processing fold #", i, "\n")
  val_indices <- which(fold_id == i)
  val_data <- ames_DN_train[val_indices, ] #prepare the validation data 
  val_targets <- ames_train_targets[val_indices]
  
  partial_train_data <- ames_DN_train[-val_indices,]
  partial_train_targets <- ames_train_targets[-val_indices]
  
  model <- build_model()
  
  model %>% fit(
    partial_train_data,
    partial_train_targets,
    epochs = num_epochs,
    batch_size = 16,
    verbose = 0
  )
  
  results <- model %>% 
    evaluate(val_data, val_targets, verbose = 0)
  all_scores[[i]] <- results[['mae']]
}

# I must do all things for the glory of God alone
#running this with num_epochs = 100 yields the following results

mae.test.dn <- c(all_scores, all_scores)

#[1] 2.473027 2.599495 2.334684 2.218129
mean(all_scores)


model <- build_model()


history <- model %>%
  fit(ames_DN_train, ames_train_targets, epoch = 500, batch_size = 64, 
      validation_split = 0.2)

plot(history)

########################################
#### Deep Learning Model fine tuning ###
########################################

# for newer grid search,,, batch_size, drop_out, nodes size.... 

FLAGS <- flags(
  # Nodes
  flag_numeric("nodes1", 256),
  flag_numeric("nodes2", 128),
  flag_numeric("nodes3", 64),
  # Dropout
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  flag_numeric("dropout3", 0.2),
  # Learning paramaters
  flag_string("optimizer", "rmsprop"),
  flag_numeric("lr_annealing", 0.1)
)


model <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$nodes1, activation = "relu", input_shape = ncol(ames_DN_train)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = FLAGS$nodes2, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(units = FLAGS$nodes3, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = FLAGS$dropout3) %>%
  layer_dense(units = 1) %>%
  compile(
    loss = "mse",
    metrics = "mae",
    optimizer = FLAGS$optimizer
  ) %>%
  fit(
    x = ames_DN_train,
    y = ames_train_targets,
    epochs = 1000,
    batch_size = 64,
    validation_split = 0.2,
    callbacks = list(
      callback_early_stopping(patience = 5),
      callback_reduce_lr_on_plateau(factor = FLAGS$lr_annealing)
    ),
    verbose = FALSE
  )


# runs <- tuning_run(file = "C:/Users/NET02/Desktop/업무/2024년/Artificial_Intteligence_연구/coding/R/scripts/ames_grid_search.R", 
#       
#                    flags = list(
#                      nodes1 = c(64, 128, 256),
#                      nodes2 = c(64, 128, 256),
#                      nodes3 = c(64, 128, 256),
#                      
#                      dropout1 = c(0.2, 0.3, 0.4),
#                      dropout2 = c(0.2, 0.3, 0.4),
#                      dropout3 = c(0.2, 0.3, 0.4),
#                      optimizer = c("rmsprop", "adam"),
#                      lr_annealing = c(0.1, 0.05)
#                      
#                    ),
#                    sample=0.05
#                    )
#sample = 1)

runs <- tuning_run(file = "C:/Users/NET02/Desktop/업무/2024년/Artificial_Intteligence_연구/coding/R/scripts/ames_grid_search.R", 
                   
                   flags = list(
                     nodes1 = c(64, 128, 256),
                     nodes2 = c(64, 128, 256),
                    
                     
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     
                     batch_size = c(16,32,64,128)
                   ),
                   sample=0.7
)


# toexecute the grid search I used tfrun::tuning_run(). Since our grid search assesses 2,916 combinations, 
# we perform a random grid search and assess only 5% of the total models(sample = 0.05, which equates to 145 models)

runs %>% filter(metric_val_loss == min(metric_val_loss)) %>%
  glimpse()

setwd("C:/Users/NET02/Desktop/업무/2024년/Artificial_Intteligence_연구/coding/R")
# write.csv(runs, file = "hyperparameter_tuning_result.csv")
tuning_results <- read.csv("hyperparameter_tuning_result.csv")

tuning_results %>% filter(metric_val_mae == min(metric_val_mae)) %>%
  glimpse()

Rows: 1
Columns: 28
$ X                <int> 129
$ run_dir          <chr> "runs/2024-06-04T05-23-16Z"
$ metric_loss      <int> 653320960
$ metric_mae       <dbl> 17894.76
$ metric_val_loss  <int> 613711040
$ metric_val_mae   <dbl> 14674.82
$ flag_nodes1      <int> 128
$ flag_nodes2      <int> 128
$ flag_dropout1    <dbl> 0.4
$ flag_dropout2    <dbl> 0.2
$ flag_batch_size  <int> 16
$ epochs           <int> 500
$ epochs_completed <int> 113
$ metrics          <chr> "runs/2024-06-04T05-23-16Z/tfruns.d/metrics.json"
$ model            <chr> "Model: \"sequential\"\n________________________________________________________________________________\n Layer (type)        …
$ loss_function    <chr> "mse"
$ optimizer        <chr> "<keras.src.optimizers.rmsprop.RMSprop object at 0x000002A99E9CFBD0>"
$ learning_rate    <dbl> 0.01
$ script           <chr> "ames_grid_search.R"
$ start            <chr> "2024-06-04 05:23:17.35663"
$ end              <chr> "2024-06-04 05:23:40.85267"
$ completed        <lgl> TRUE
$ output           <chr> "\n> #source : https://bradleyboehmke.github.io/HOML/deep-learning.html\n> \n> # FLAGS <- flags(\n> #   # Nodes\n> #   flag_num…
$ source_code      <chr> "runs/2024-06-04T05-23-16Z/tfruns.d/source.tar.gz"
$ context          <chr> "local"
$ type             <chr> "training"



##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################



######################################################
### Doing the same thing using Boston Housing data ###
######################################################

boston <- Boston
str(Boston)
dim(boston)
n = nrow(boston)
rep = 20


mse.train.model1 = mse.train.model2 = mse.train.model3 = 
  mse.train.lasso = mse.train.ridge = mse.train.enet = rep(0,20) #creating empty vector to save training error for each model

mse.test.model1 = mse.test.model2 = mse.test.model3 = 
  mse.test.lasso = mse.test.ridge = mse.test.enet = rep(0,20) #creating empty vector to save training error for each model

#loop
set.seed(1234)

for(i in 1:rep){
  cat("Processing Loop #", i, "\n\n")
  train_index = sample(1:n, n*0.7, replace = FALSE)
  boston_train = boston[train_index, ]
  boston_test = boston[-train_index, ]
  X_train = model.matrix(medv ~ ., boston_train)[,-1]
  Y_train = boston_train$medv
  X_test = model.matrix(medv ~., boston_test)[,-1]
  Y_test = boston_test$medv
  
  #model1
  model1 = lm(medv ~ rm, data = boston_train)
  pred.train.model1 = predict(model1)
  pred.test.model1 = predict(model1, newdata=boston_test[,!names(boston_test)%in%c("medv")])
  mse.train.model1[i] = MAE(pred.train.model1, boston_train$medv) 
  #sqrt(sum((boston_train$medv - pred.train.model1)^2)/length(boston_train$medv))
  mse.test.model1[i] = MAE(pred.test.model1, boston_test$medv)
  #sqrt(sum((boston_test$medv - pred.test.model1)^2)/length(boston_test$medv))
  
  #model2
  model2 = lm(medv ~ rm + tax + ptratio + black + chas, data = boston_train)
  pred.train.model2 = predict(model2)
  pred.test.model2 = predict(model2, newdata=boston_test[,!names(boston_test)%in%c("medv")])
  mse.train.model2[i] = MAE(pred.train.model2, boston_train$medv)
  #sqrt(sum((boston_train$medv - pred.train.model2)^2)/length(boston_train$medv))
  mse.test.model2[i] = MAE(pred.test.model2, boston_test$medv)
  #sqrt(sum((boston_test$medv - pred.test.model2)^2)/length(boston_test$medv))
  
  #model3
  model3 <- glmnet(x=X_train, y=Y_train, lambda = 0, alpha=0)
  pred.train.model3 <- predict(model3, X_train)
  pred.test.model3 <- predict(model3, X_test) 
  mse.train.model3[i] <- MAE(pred.train.model3, boston_train$medv)
  mse.test.model3[i] <- MAE(pred.test.model3, boston_test$medv)
  
  #lasso
  cvfitl = cv.glmnet(x=X_train,y=Y_train,family="gaussian",alpha=1,standardize=TRUE)
  pred.train.l = predict(cvfitl, newx = X_train, s = "lambda.min") 
  pred.test.l = predict(cvfitl, newx = X_test, s = "lambda.min") 
  mse.train.lasso[i] = MAE(pred.train.l, Y_train)
  mse.test.lasso[i] = MAE(pred.test.l, Y_test)

  #ridge
  cvfitr = cv.glmnet(x=X_train,y=Y_train,family="gaussian",alpha=0,standardize=TRUE)
  pred.train.r = predict(cvfitr, newx = X_train, s = "lambda.min") 
  pred.test.r = predict(cvfitr, newx = X_test, s = "lambda.min") 
  mse.train.ridge[i] = MAE(pred.train.r, Y_train)
  mse.test.ridge[i] = MAE(pred.test.r, Y_test)
  
  #ENET
  cvfiten = cv.glmnet(x=X_train,y=Y_train,family="gaussian",alpha=0.5,standardize=TRUE)
  pred.train.en = predict(cvfiten, newx = X_train, s = "lambda.min") 
  pred.test.en = predict(cvfiten, newx = X_test, s = "lambda.min") 
  mse.train.enet[i] = MAE(pred.train.en, Y_train)
  mse.test.enet[i] = MAE(pred.test.en, Y_test)
  
}

mse.train.model1
mse.train.model2
mse.train.model3
mse.train.lasso
mse.train.ridge
mse.train.enet


mse.test.model1
mse.test.model2
mse.test.model3
mse.test.lasso
mse.test.ridge
mse.test.enet

### Boxplot for mse test
mse.test.combined <- c(mse.test.model1, mse.test.model2, mse.test.model3, mse.test.lasso, mse.test.ridge, mse.test.enet)
index <- rep(c("test.model1", "test.model2", "test.model3", "test.lasso", "test.ridge", "test.enet"), each=20)
mse.test.combined <- cbind(mse.test.combined, index)
mse.test.combined <- as.data.frame(mse.test.combined)
mse.test.combined$mse.test.combined <- as.numeric(mse.test.combined$mse.test.combined) 

ggplot(data=mse.test.combined, aes(x=index, y=mse.test.combined)) + geom_boxplot()

### Boxplot for mse training
mse.train.combined <- c(mse.train.model1, mse.train.model2, mse.train.model3, mse.train.lasso, mse.train.ridge, mse.train.enet)
index <- rep(c("train.model1", "train.model2", "train.model3", "train.lasso", "train.ridge", "train.enet"), each=20)
mse.train.combined <- cbind(mse.train.combined, index)
mse.train.combined <- as.data.frame(mse.train.combined)
mse.train.combined$mse.train.combined <- as.numeric(mse.train.combined$mse.train.combined) 

ggplot(data=mse.train.combined, aes(x=index, y=mse.train.combined)) + geom_boxplot()

############################################
### Deep Neural Network with Boston Data ###
############################################

#Deep Learning...with Boston data first ->, then move on to using AMES dataset. This may more complicated process
#because AMES dataset contains a lot of categorical variable. I need to make sure how I should preprocess all those variables. 
boston <- Boston
names(boston)
predictors <- as.matrix(boston[,as.numeric(1:13)])
targets <- as.matrix(boston[ ,14])
n <- nrow(boston)
set.seed(1234)

train_index <- sample(1:n, 404, replace=FALSE)
train_data <- predictors[train_index,]
train_targets <- targets[train_index]
test_data <- predictors[-train_index,] 
test_targets <- targets[-train_index]

mean <- apply(train_data, 2, mean)
sd <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = sd)
test_data <- scale(test_data, center =mean, scale = sd)


build_model <- function(){ #because we need to instantiate the same model multiple times, we use function to construct it.
  model <- keras_model_sequential() %>%
    layer_dense(64, activation = "relu") %>%
    #layer_dropout(rate = 0.2) %>%
    #layer_batch_normalization() %>%
    layer_dense(64, activation = "relu") %>%
    #layer_dropout(rate = 0.2) %>%
    #layer_batch_normalization() %>%
    layer_dense(1)

    #, kernel_regularizer = regularizer_l2(0.001)
  model %>% compile(optimizer = "rmsprop",
                    loss = "mse",
                    metrics = "mae")

  model
  
}

#k-fold validation
k <- 4
fold_id <- sample(rep(1:k, length.out = nrow(train_data)))
num_epochs <- 100
all_scores <- numeric()

for(i in 1:k) {
  cat("Processing fold #", i, "\n")
  val_indices <- which(fold_id == i)
  val_data <- train_data[val_indices, ] #prepare the validation data 
  val_targets <- train_targets[val_indices]

  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]

  model <- build_model()

  model %>% fit(
    partial_train_data,
    partial_train_targets,
    epochs = num_epochs,
    batch_size = 16,
    verbose = 0
  )
  
  results <- model %>% 
  evaluate(val_data, val_targets, verbose = 0)
  all_scores[[i]] <- results[['mae']]
  
}

# I must do all things for the glory of God alone
#running this with num_epochs = 100 yields the following results

mae.test.dn <- all_scores
#[1] 2.473027 2.599495 2.334684 2.218129\
mean(all_scores)


#Let's try training the model a bit longer: 500 epochs
#k-fold validation
k <- 4
fold_id <- sample(rep(1:k, length.out = nrow(train_data)))
num_epochs <- 500
all_mae_histories <- list()

for(i in 1:k) {
  cat("Processing fold #", i, "\n")
  val_indices <- which(fold_id == i)
  val_data <- train_data[val_indices, ] #prepare the validation data 
  val_targets <- train_targets[val_indices]
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]

  model <- build_model()

  history <- model %>% fit(
    partial_train_data,
    partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs,
    batch_size = 16,
    verbose = 0
  )

  mae_history <- history$metrics$val_mae
  all_mae_histories[[i]] <- mae_history
 }

all_mae_histories <- do.call(cbind, all_mae_histories)
plot(history)
average_mae_history <- rowMeans(all_mae_histories) #calculating average per epoch
plot(average_mae_history, xlab="epoch",  type = 'l')

truncated_mae_history <- average_mae_history[-(1:20)]
plot(average_mae_history, xlab="epoch",  type = 'l',
     ylim = range(truncated_mae_history))

min = which.min(average_mae_history)
#training the final model
model <- build_model() 

history <- model %>% fit(train_data, train_targets, #train it on the entirety of the data
              epochs = min, batch_size = 16)

result <- model %>% evaluate(test_data, test_targets)
result["mae"]

DN.test.mae <- rep(0,20)


for(i in 1:20){
  cat("Loop #", i, "\n")
  model <- build_model() 
  model %>% fit(train_data, train_targets, #train it on the entirety of the data
                epochs = min*1.2, batch_size = 16, verbose = 0)
  
  result <- model %>% evaluate(test_data, test_targets)

  DN.test.mae[i] <- result["mae"]

}


DN.test.mae
mean(DN.test.mae)
### Boxplot for mse test
mse.test.combined <- c(mse.test.model1, mse.test.model2, mse.test.model3, 
                       mse.test.lasso, mse.test.ridge, mse.test.enet, DN.test.mae)

index <- rep(c("test.model1", "test.model2", "test.model3", "test.lasso", "test.ridge", "test.enet", "test.DN"), each=20)

mse.test.combined <- cbind(mse.test.combined, index)
mse.test.combined <- as.data.frame(mse.test.combined)
mse.test.combined$mse.test.combined <- as.numeric(mse.test.combined$mse.test.combined) 

ggplot(data=mse.test.combined, aes(x=index, y=mse.test.combined)) + geom_boxplot()


# resources
# https://bradleyboehmke.github.io/HOML/linear-regression.html#assessing-model-accuracy
# https://www.tmwr.org/ames
# https://jse.amstat.org/v19n3/decock/DataDocumentation.txt.   <- data description




