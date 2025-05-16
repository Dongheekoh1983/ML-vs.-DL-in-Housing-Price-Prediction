
###########################################
### Setting Up Environment for Analysis ###
###########################################

#Bringing necessary library
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
library(ggmap)
library(sf)
library(RColorBrewer)
library(patchwork)
library(corrplot)

#Brining Ames Housing dataset
ames <- AmesHousing::make_ames()
dim(ames)

#######################################
### Data Clean Up for Deep Learning ###
#######################################

#identifying ordinal variables from Ames dataset and re-order them properly as they are defined in the data dictionary; 
#data dictionary link -->  https://jse.amstat.org/v19n3/decock/DataDocumentation.txt

#1) Extracting and separating variables from Ames dataset according to it types (numeric, discrete, categorical, ordinal, & response(Sale_Price))

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


ames_ordinal <- ames %>% dplyr::select(Lot_Shape, Land_Slope, 
                                       Overall_Qual, Overall_Cond, 
                                       Exter_Qual, Exter_Cond,
                                       Bsmt_Qual, Bsmt_Cond, Bsmt_Exposure, 
                                       BsmtFin_Type_1, BsmtFin_Type_2,
                                       Heating_QC, Electrical, Kitchen_Qual,
                                       Functional, Fireplace_Qu, Garage_Finish,
                                       Garage_Qual,Garage_Cond, 
                                       Paved_Drive, Pool_QC, Fence)


ames_response <- ames %>% dplyr::select(Sale_Price)

#2)Properly re-ordering all ordinal variables

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

str(ames_ordinal) #making sure that the job is properly done; when done properly, all ordinal variables are supposed to be integers.


#3) Preprocessing numeric variables 
# - dividing them into training and testing data 
# - scaling them using the mean and standard deviation of training dataset only; 
ames_numeric <- cbind(ames_ordinal, ames_numeric, ames_discrete)

n= nrow(ames_numeric)
train_index = sample(1:n, n*0.7, replace = FALSE)

ames_numeric_train <- ames_numeric[train_index, ]
ames_numeric_test <- ames_numeric[-train_index, ]

rm(mean)
rm(sd)
mean_train <- apply(ames_numeric_train, 2, mean) #calculating mean from training data only.. no info leak prior to model training 
sd_train <- apply(ames_numeric_train, 2, sd) #calculating standard deviation from training data.

ames_numeric_train <- scale(ames_numeric_train, center=mean_train, scale=sd_train)
ames_numeric_test <- scale(ames_numeric_test, center=mean_train, scale=sd_train) #now numeric data are scaled properly

#4) Preprocessing categorical variables (one-hot-vector encoding)
dummy_formula <- dummyVars(~., data=ames_categorical)
ames_categorical_one_hot <- predict(dummy_formula, newdata=ames_categorical)
ames_categorical_train <- ames_categorical_one_hot[train_index,]
ames_categorical_test <- ames_categorical_one_hot[-train_index,]

#5) Finally, combining all pre-processed data for Deep Neural Net training
ames_train_targets <- ames$Sale_Price[train_index] 
ames_test_targets <- ames$Sale_Price[-train_index]
ames_DN_train <- cbind(ames_numeric_train, ames_categorical_train) 
ames_DN_test <- cbind(ames_numeric_test, ames_categorical_test)

#################################
### Exploratory Data Analysis ###
#################################


### Preparing Base Map 

#### Ames Data Mapping Analysis
display.brewer.all() #displying color brewer palette. 

ames_road <- st_read('/Users/dongheekoh/Documents/Data Science Training/portfolio/projects/stat577_ML_project/machine_learning_project/tl_2023_19169_roads', 
                     quiet=TRUE, layer="tl_2023_19169_roads") #file path for mac (home computer)

ames_road <- st_read('C:/Users/NET02/Desktop/tasks/2024/Artificial_Intteligence/coding/R/DNN_project', 
                     quiet=TRUE, layer="tl_2023_19169_roads") # file path for windows (workplace computer)


nb.cols <- 29 
myColors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
names(myColors) <- levels(ames$Neighborhood)
custom_colors <- scale_colour_manual(name = "Neighborhood", values = myColors)

xmin <- -93.70
xmax <- -93.60
ymin <- 41.99
ymax <- 42.06

background <- ggplot(ames_road) + geom_sf(color="grey") +
  xlim(xmin, xmax) + ylim(ymin, ymax)

background + 
  geom_point(data=ames,mapping = aes(x=Longitude, y=Latitude, colour = Neighborhood), size=1.2) + 
  custom_colors + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(1,1.6,1,0.1), "cm"))

#setwd for mac(home computer)
setwd("/Users/dongheekoh/Documents/Data Science Training/portfolio/projects/stat577_ML_project/machine_learning_project/images")

#setwd for windows(workplace computer)
setwd("C:/Users/NET02/Desktop/tasks/2024/Artificial_Intteligence/coding/R/DNN_project/image")

#saving output image
ggsave(filename = "ames_neighborhoods.png", width=10, height=9, dpi=300)

##########################
#### descriptive analysis  

#### Sale_Price
options(scipen=999)
summary(ames$Sale_Price)
hist_saleprice <- ggplot(ames, aes(x=Sale_Price)) + geom_histogram(bins=50, col="white") #regular histogram

#### Descriptive analysis for each variable
ames$Price_Category <- cut(ames$Sale_Price, breaks = c(-Inf, 100000, 200000, 300000, 400000, Inf),
                           label = c("Very Low", "Low", "Medium", "High", "Very High"))

custom_palette <- c("Very Low"="blue", "Low"="lightblue", "Medium"="yellow", "High"="orange", "Very High"="red")

map_saleprice<-
  background + 
  geom_point(data=ames, mapping = aes(x=Longitude, y=Latitude, colour = Price_Category), size=0.8) + 
  scale_color_manual(values = custom_palette) +
  guides(colour=guide_legend(nrow=2)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 9))

hist_saleprice + map_saleprice + plot_layout(ncol = 2)

ggsave(filename = "ames_saleprice.png", width=10, height=6, dpi=300)

#################
#### Lot_Area
################

hist_lot <- ggplot(ames, aes(x=Lot_Area)) + 
  geom_histogram(bins=50, col="white") + 
  xlim(0, 50000) #Lot_Area (Continuous): Lot size in square feet

mean(ames$Lot_Area)
sd(ames$Lot_Area)
max(ames$Lot_Area)
summary(ames$Lot_Area)
#-majority of them centered around the mean... sd is not that big,, but there are few exception, the largest size being 215,245.

lot_largest <- ames %>% dplyr::filter(Lot_Area > 20000)

background + 
  geom_point(data=lot_largest, mapping = aes(x=Longitude, y=Latitude), size=2, colour = "blue") 

lot_price_scatter <-
  ggplot(ames, aes(x=Lot_Area, y=Sale_Price)) + geom_point(size=0.8) + 
  xlim(0, 30000) +
  geom_smooth(se=TRUE)

hist_lot + lot_price_scatter + plot_layout(ncol = 2)

ggsave(filename = "ames_price_lotsize_scatter.png", width=10, height=6, dpi=300)


#this shows that there is a relationship between sale_price & lot_size, but it is certainly not linear. And outlier is affecting much. 

###################
#### Year_Built ###
##################

hist_year_built <- ggplot(ames, aes(x=Year_Built)) + geom_histogram(bins=45, col="white") +
  theme(plot.margin = ggplot2::margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"))

year_built_saleprice_scatter <- 
  ggplot(ames, aes(x=Year_Built, y=Sale_Price)) + geom_point(size=0.8) +
  geom_smooth(se=TRUE) + #overall, newer the house, higher the price; but there are exceptions to this. 
  theme(plot.margin = ggplot2::margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"))
  
#Geogrpahical distribution?
ames$year_built_category <- cut(ames$Year_Built, breaks = c(-Inf, 1900, 1930, 1960, 1990, 2000, Inf),
                           label = c("before_1900", "00_30", "30_60", "60_90", "90_20", "after_20"))

custom_palette <- c("before_1900"="#ffffcc", "00_30"="#c7e9b4", "30_60"="#7fcdbb", "60_90"="#41b6c4", "90_20"="#2c7fb8", "after_20"="#253494")

map_year_built<- 
  background + 
  geom_point(data=ames, mapping = aes(x=Longitude, y=Latitude, colour = year_built_category), size=0.8) + 
  scale_color_manual(values = custom_palette) +
  theme(legend.position = "bottom",
        plot.margin = ggplot2::margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 9))
#The results indicates that development started from the center, and it spread out toward outer rim...also price goes up as well. 

hist_year_built + year_built_saleprice_scatter + map_year_built + plot_layout(ncol=3)

ggsave(filename = "ames_price_yearbuilt_eda.png", width=15, height=6, dpi=300)


#### Total_Bsmt_SF: Total square feet of basement area
ggplot(ames, aes(x=Total_Bsmt_SF)) + geom_histogram(bins=45, col="white") + xlim(0, 3000)
ggplot(ames, aes(x=Total_Bsmt_SF, y=Sale_Price)) + geom_point() + xlim(0, 3000) + geom_smooth(se=TRUE) 
#as the total bsmt increases, sale_price tends to increase significantly. 




###################################################################
#### Gr_Liv_Area: Above grade (ground) living area square feet
###################################################################

Gr_Liv_hist <- ggplot(ames, aes(x=Gr_Liv_Area)) + geom_histogram(bins=45, col="white")
  #theme(plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  
Gr_Liv_SalePrice_scatter <- ggplot(ames, aes(x=Gr_Liv_Area, y=Sale_Price)) + geom_point(size=0.8) + 
  geom_smooth(se=TRUE) 

#### Descriptive analysis for each variable
ames$Gr_Liv_Area_category <- cut(ames$Gr_Liv_Area, breaks = c(0, 500, 1500, 2000, 2500, 5642),
                                 labels = c("< 500", "500-1499", "1500-1999", "2000-2499", "2500+"),
                                 include.lowest = TRUE,
                                 right=FALSE)

custom_palette <- c("< 500" = "blue", "500-1499" = "lightblue", "1500-1999" = "yellow", "2000-2499" = "orange", "2500+" = "red")


map_Gr_Liv_Area<-
  background + 
  geom_point(data=ames, mapping = aes(x=Longitude, y=Latitude, colour = Gr_Liv_Area_category), size=0.8) + 
  scale_color_manual(values = custom_palette) +
  guides(colour=guide_legend(nrow=2)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 9))

map_Gr_Liv_Area


Gr_Liv_SalePrice_scatter + map_Gr_Liv_Area + plot_layout(ncol = 2)

ggsave(filename = "ames_price_liv_area_figure.png", width=10, height=6, dpi=300)


# Gr_Liv_Area also displays stronger correlation
#ggplot(ames, aes(x=Gr_Liv_Area, y=Total_Bsmt_SF)) + geom_point() + xlim(0, 5000) + geom_smooth(se=TRUE) 
# Gr_Liv_Area & Total_Bmst_SF are also strongly correlated one another. 

#### Sale_Price, Lot_Area, Year_Built, Total_Bsmt_SF, Gr_Liv_Area
numeric_temp <- ames %>% dplyr::select(Sale_Price, Lot_Area, Year_Built, Total_Bsmt_SF, Gr_Liv_Area)
corrplot(cor(numeric_temp), col=rev(COL2('RdYlBu', 50)), tl.col="darkgrey")


corr_matrix <- cor(numeric_temp)

corrplot(corr_matrix, method="circle")
corrplot(corr_matrix, method="color", col = COL2('BrBG'))

#######################
####overall_condition
#######################

boxplot_condition_price <- ggplot(ames, aes(x=Overall_Qual, y=Sale_Price)) + 
  geom_boxplot(aes(colour=Overall_Qual), show.legend = FALSE) +
  labs(x=NULL, y="Sale_Price")+
  theme(
        axis.text.x = element_text(size=8,angle=70, vjust=0.5),
        axis.text.y = element_text())


map_condition <- 
background + 
  geom_point(data=ames, mapping = aes(x=Longitude, y=Latitude, colour = Overall_Qual), size=0.8) +
  scale_color_brewer(palette = "Spectral") +
  guides(color = guide_legend(title = NULL))
  #theme(legend.position = "bottom")


boxplot_condition_price + map_condition + plot_layout(ncol=2)
ggsave(filename = "ames_price_condition_figure.png", width=10, height=4.5, dpi=300)

####Bsmt_Exposure
ggplot(ames, aes(x=Bsmt_Exposure, y=Sale_Price)) + geom_boxplot(aes(colour=Bsmt_Exposure), show.legend = FALSE)
#geom_boxplot(outlier.color = "red", outlier.shape=1) + #xlim(0, 100000) <- ????
background + 
  geom_point(data=ames, mapping = aes(x=Longitude, y=Latitude, colour = Bsmt_Exposure), size=1.4) +
  scale_color_brewer(palette = "Spectral")

################
####Exter_Cond
################
boxplot_external_qual_price <- ggplot(ames, aes(x=Exter_Qual, y=Sale_Price)) + geom_boxplot(aes(colour=Exter_Qual), show.legend = FALSE)
#geom_boxplot(outlier.color = "red", outlier.shape=1) + #xlim(0, 100000) <- ????

map_external_qual <- 
background + 
  geom_point(data=ames, mapping = aes(x=Longitude, y=Latitude, colour = Exter_Qual), size=0.8) +
  scale_color_brewer(palette = "Spectral")+
  guides(color = guide_legend(title = NULL))

boxplot_external_qual_price + map_external_qual + plot_layout(ncol=2)
ggsave(filename = "ames_price_ext_quality_figure.png", width=10, height=4.5, dpi=300)

######################################
#### Variable 2: MS Zoning (Nominal)
summary(ames$MS_Zoning)
table(ames$MS_Zoning)

ggplot(ames, aes(MS_Zoning, Sale_Price)) +
  geom_violin(aes(fill=as.factor(MS_Zoning))) +
  geom_boxplot(width=0.1, alpha=0.5)+
  coord_flip() + theme(legend.position = "none")

zoning_temp <- ames %>% filter(MS_Zoning=="Floating_Village_Residential")
background + 
  geom_point(data=zoning_temp, mapping = aes(x=Longitude, y=Latitude, colour = Exter_Qual), size=1.4, col="#253494") +
  scale_color_brewer(palette = "Spectral")


###########################################################################################
### Performance Comparison of Machine Learning Models for Ames Housing Price Prediction ###
###########################################################################################

#data for machine learning models 
ames <- AmesHousing::make_ames()

n = nrow(ames)
rep = 15

#creating empty vector to save training & testing error for each model
mse.train.model3 = mse.train.lasso = mse.train.ridge = 
mse.train.enet = mse.train.rf= mse.train.bag =
mse.train.pcr = mse.train.plsr = rep(0,15) 

mse.test.model3 = mse.test.lasso = mse.test.ridge = 
mse.test.enet = mse.test.rf = mse.test.bag =
mse.test.pcr = mse.test.plsr = rep(0,15) 

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
  
  #model3 - linear regression model using all variables
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

### Boxplot for mse test
mse.test.combined <- c(mse.test.model3, mse.test.lasso, mse.test.ridge, 
                       mse.test.enet, mse.test.pcr, mse.test.plsr, 
                       mse.test.rf,mse.test.bag)

index <- rep(c("test.model3", "test.lasso", "test.ridge", 
               "test.enet", "test.pcr", "test.plsr", 
               "test.rf","test.bag"), each=15)

mse.test.combined <- cbind(mse.test.combined, index)
mse.test.combined <- as.data.frame(mse.test.combined)
mse.test.combined$mse.test.combined <- as.numeric(mse.test.combined$mse.test.combined) 

ml_test1 <- ggplot(data=mse.test.combined, aes(x=index, y=mse.test.combined)) + 
  geom_boxplot() + xlab("Testing MSE") + theme(axis.title.y=element_blank())

### Boxplot for mse training
mse.train.combined <- c(mse.train.model3, mse.train.lasso, mse.train.ridge, 
                        mse.train.enet, mse.train.pcr, mse.train.plsr, 
                        mse.train.rf, mse.train.bag)

index <- rep(c("train.model3", "train.lasso", "train.ridge", 
               "train.enet", "train.pcr","train.plsr", 
               "train.rf", "train.bag"), each=15)

mse.train.combined <- cbind(mse.train.combined, index)
mse.train.combined <- as.data.frame(mse.train.combined)
mse.train.combined$mse.train.combined <- as.numeric(mse.train.combined$mse.train.combined) 

ml_train1 <- ggplot(data=mse.train.combined, aes(x=index, y=mse.train.combined)) + 
  geom_boxplot() + xlab("Training MSE") + theme(axis.title.y = element_blank())      

ml_train1 + ml_test1 + plot_layout(ncol = 1)

ggsave(filename = "ML_Models_Training_Testing_Performance.png", width=10, height=7, dpi=300)
############################
### Model Interpretation ###
############################
# for example, include 1) regression coefficient interpretation 2) variable importance plot etc...
# refer to the following link https://bradleyboehmke.github.io/HOML/linear-regression.html





###############################################################
### Predicting Ames Housing Price using Deep Neural Network ###
###############################################################

## Initial Deep Learning Model
## Initial model looks like the following 
build_model <- function(){ #because we need to instantiate the same model multiple times, we use function to construct it.
  
  model <- keras_model_sequential() %>%
    layer_dense(64, activation = "relu", input_shape = ncol(ames_DN_train)) %>%
    layer_dense(64, activation = "relu") %>%
    layer_dense(1)
  
  model %>% compile(optimizer = optimizer_rmsprop(), 
                    loss = "mse",
                    metrics = "mae")
  
  model
}

## I went through model fine tuning process, 1) first by manually tweaking hyperparameters 2) second by utilizing grid search
FLAGS <- flags(
  # Nodes
  flag_numeric("nodes1", 256),
  flag_numeric("nodes2", 128),
  # Dropout
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  #learning parameters
  flag_string("optimizer", "rmsprop"),
  # batch_size
  flag_numeric("batch_size", 64)
)

model <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$nodes1, activation = "relu", input_shape = ncol(ames_DN_train)) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = FLAGS$nodes2, activation = "relu") %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(units = 1) %>%
  compile(
    loss = "mse",
    metrics = "mae",
    optimizer = FLAG$optimizer
  ) %>%
  fit(
    x = ames_DN_train,
    y = ames_train_targets,
    epochs = 500,
    batch_size = FLAGS$batch_size,
    validation_split = 0.2,
    callbacks = list(
      callback_early_stopping(patience = 25)
    ),
    verbose = 0
  )

runs <- tuning_run(file = "C:/Users/NET02/Desktop/??????/2024???/Artificial_Intteligence_???구/coding/R/scripts/ames_grid_search.R", 
                   flags = list(
                     nodes1 = c(64, 128, 256),
                     nodes2 = c(64, 128, 256),
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     optimizer = c("rmsprop", "adam"),
                     batch_size = c(16,32,64,128)
                    ),
                   sample=0.7
                   )

# Rows: 1
# Columns: 28
# $ X                <int> 129
# $ run_dir          <chr> "runs/2024-06-04T05-23-16Z"
# $ metric_loss      <int> 653320960
# $ metric_mae       <dbl> 17894.76
# $ metric_val_loss  <int> 613711040
# $ metric_val_mae   <dbl> 14674.82
# $ flag_nodes1      <int> 128
# $ flag_nodes2      <int> 128
# $ flag_dropout1    <dbl> 0.4
# $ flag_dropout2    <dbl> 0.2
# $ flag_batch_size  <int> 16
# $ epochs           <int> 500
# $ epochs_completed <int> 113
# $ metrics          <chr> "runs/2024-06-04T05-23-16Z/tfruns.d/metrics.json"
# $ model            <chr> "Model: \"sequential\"\n________________________________________________________________________________\n Layer (type)        ???
# $ loss_function    <chr> "mse"
# $ optimizer        <chr> "<keras.src.optimizers.rmsprop.RMSprop object at 0x000002A99E9CFBD0>"
# $ learning_rate    <dbl> 0.01
# $ script           <chr> "ames_grid_search.R"
# $ start            <chr> "2024-06-04 05:23:17.35663"
# $ end              <chr> "2024-06-04 05:23:40.85267"
# $ completed        <lgl> TRUE
# $ output           <chr> "\n> #source : https://bradleyboehmke.github.io/HOML/deep-learning.html\n> \n> # FLAGS <- flags(\n> #   # Nodes\n> #   flag_num???
#   $ source_code      <chr> "runs/2024-06-04T05-23-16Z/tfruns.d/source.tar.gz"
# $ context          <chr> "local"
# $ type             <chr> "training"



## Building Deep Learning model; after hyperparameter fine tuning, I found the following best performing DN Model
## As can be seen, I, aided by grid search above, have added l2_regularization, layer_dropout, and adjusted unit number for each layer.
## lastly, I used adam optimizer instead of rmsprop. <-- in the portfolio, I need to briefly explain how each parameter helps. 

build_model <- function(){ #because we need to instantiate the same model multiple times, we use function to construct it.
  
  model <- keras_model_sequential() %>%
    layer_dense(128, activation = "relu", kernel_regularizer = regularizer_l2(0.001),input_shape = ncol(ames_DN_train)) %>%
    layer_dropout(rate=0.4) %>%
    layer_dense(128, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dropout(rate=0.2) %>%
    layer_dense(1)
  
  model %>% compile(optimizer = optimizer_adam(), 
                    loss = "mse",
                    metrics = "mae")
  
  model

 }

#Training the final deep neural network model for 500 epochs using k-fold cross validation
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
#[1] 936
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

#result from baseline model #this looks better than the model below hmm...
ames_dn_test_results <- c(15005.39, 14925.67, 14870.62, 
                          14981.25, 14965.23, 14908.94,
                          15131.90, 14907.56, 15028.89,
                          15025.54, 14952.75, 14925.85,
                          14945.20, 15219.53, 15198.08)

#final tuned model results with layer_dense(64,64,1), l2 regularization, layer_dropout, adam, etc
ames_dn_test_results <-
      c(15286.94, 15094.63, 15230.13, # results from initial training
        15084.27, 15239.37, 15230.49,
        15329.19, 15168.20, 15234.59,
        15150.51, 15193.56, 15201.99,
        15162.02, 15099.38, 15341.54)

mean(ames_dn_test_results)


# ensemble of DN and RF? ###
mse.test.rf
ames_dn_test_results

mse.ensemble <- 0.8*ames_dn_test_results + 0.2*mse.test.rf

### After Deep Learning Include the following for the final model comparison
mse.test.combined <- c(mse.test.model3, mse.test.lasso, mse.test.ridge, 
                       mse.test.enet, mse.test.pcr, mse.test.plsr, 
                       mse.test.rf,ames_dn_test_results, mse.ensemble) #mse.test.bag,

index <- rep(c("test.model3", "test.lasso", "test.ridge", 
               "test.enet", "test.pcr", "test.plsr",   #"test.bag",
               "test.rf","test.DN", "test.ensemble"), 
                each=15)

mse.test.combined <- cbind(mse.test.combined, index)
mse.test.combined <- as.data.frame(mse.test.combined)
mse.test.combined$mse.test.combined <- as.numeric(mse.test.combined$mse.test.combined) 

ggplot(data=mse.test.combined, aes(x=index, y=mse.test.combined)) + geom_boxplot()


mean(mse.ensemble)
mean(ames_dn_test_results)

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



##################################
##### Extra code just in case ####

# width cumulative curve, but for this project I am using a regular histogram
mean_price <- mean(ames$Sale_Price)
sd_price <- sd(ames$Sale_Price)
ggplot(ames, aes(x=Sale_Price)) + geom_histogram(aes(y=..density..),bins=50, col="white") +   # with norm curve
  stat_function(fun=dnorm, args=list(mean=mean_price, sd=sd_price), color="skyblue", size=0.6)



##########################################################
###### Exporting DiagramR workflow into image ############
##########################################################


graph <- DiagrammeR::grViz("               
digraph surveillance_diagram {    # 'digraph' means 'directional graph', then the graph name 

  # graph statement
  graph [layout = dot,
         rankdir = LR,            # layout left-to-right
         fontsize = 10]

  # nodes (circles)
  node [shape = circle,           # shape = circle
       fixedsize = true
       width = 1.3]                      

  # individual component
  ames_data  [label = 'Ames\nhousing data', shape = cylinder, height=1.3, fontcolor='#f6d55c', color='#f6d55c'] 
  divide_data [label = 'Divide Ames\ndata by types', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  numeric [label = 'Numeric \n variables', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  discrete [label = 'Discrete \n variables', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  ordinal [label = 'Ordinal \n variables', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  categorical [label = 'Categorical \n variables', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  reorder [label = 'Reorder\nordinal\nvariables', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  one_hot [label = 'One-Hot\nEncoding', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  training_set [label = '70%\n training nset', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  testing_set  [label = '30% \ntesting\n/validation set', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  standardize [label = 'standardize\nthe dataset', shape = rect, height = 1.1, fontcolor='#3caea3' color='#3caea3']
  eda [label = 'EDA', shape=oval, height=1.1, fontcolor='#20639b', color='#20639b']
  gis [label = 'GIS\nvisualization', shape=oval, height=1.1, fontcolor='#20639b', color='#20639b']
  machine_learning [label = 'ML\nmodels', shape=octagon, height=1.2, fontcolor = '#ed553b', color='#ed553b']
  deep_learning [label = 'Deep\nNeural\nNetwork', shape=doubleoctagon, height=1.2, fontcolor = '#ed553b', color='#ed553b']
  hyper_tuning [label = 'hyperparameter\ntuning', shape=octagon, height=1.2, fontcolor = '#ed553b', color='#ed553b']
  performance [label = 'Performance\nComparison', height=1.5,fontcolor = '#ed553b', color='#ed553b']
  best_model [label = 'Select\nbest performing\nmodel', shape=tripleoctagon, height=1.3, fontcolor = '#173F5F', color='#173F5F']
  
  # Machine Learning Models
  mlr [label = 'Multiple\nLinear\nRegression', height=1.5, fontcolor = '#ed553b', color='#ed553b']
  regularization [label = 'Regularization:\nLasso\nRidge\nENET', height=1.5, fontcolor = '#ed553b', color='#ed553b']
  tree_based [label = 'Tree-based:\nRandom Forest\nBagging',  height=1.5, fontcolor = '#ed553b', color='#ed553b']
  dim_reduction [label = 'Dimension\nreduction:\nPCR\nPLSR', height=1.5 ,fontcolor = '#ed553b', color='#ed553b']
  
  #linking the components:
  ames_data -> divide_data -> {numeric discrete ordinal categorical}[style = dashed, color = '#3caea3']
  ordinal -> reorder[style = dashed, color = '#3caea3']
  categorical -> one_hot[style = dashed, color = '#3caea3']
  {one_hot reorder discrete numeric} -> training_set[style = dashed, color = '#3caea3']
  {one_hot reorder discrete numeric} -> testing_set[style = dashed, color = '#3caea3']
  {training_set testing_set} -> standardize[style = dashed, color = '#3caea3']
  standardize -> {eda gis}[style = dashed, color = '#20639b']
  {eda gis} -> machine_learning -> {mlr regularization tree_based dim_reduction}[style = dashed, color = '#ed553b']
  {eda gis} -> hyper_tuning -> deep_learning[style = dashed, color = '#ed553b']
  {mlr regularization tree_based dim_reduction deep_learning} -> performance -> best_model[style = dashed, color = '#ed553b']
  
 }
")

setwd("/Users/dongheekoh/Documents/Data Science Training/portfolio/projects/stat577_ML_project/machine_learning_project/images")

graph_svg <- export_svg(graph)
rsvg::rsvg_png(charToRaw(graph_svg), "high_resolution_workflow1.png", width=5500, height=3000)


```{r, echo=FALSE, out.width="250%", fig.align='center'}
knitr::include_graphics("C:/Users/NET02/Desktop/tasks/2024/Artificial_Intteligence/coding/R/DNN_project/image/high_resolution_workflow1.png")
```






