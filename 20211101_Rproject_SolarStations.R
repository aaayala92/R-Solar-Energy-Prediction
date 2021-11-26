#### R Group Assignment  - Group H ####

#### Paths ####

folder_path <- "/Users/artur/OneDrive/Documentos/IE/Term 2/R/Group Project/" 

path_main_dataset <- file.path(folder_path, "solar_dataset.RData")
path_station_info <- file.path(folder_path, "station_info.csv")
path_add_vars <- file.path(folder_path, "additional_variables.RData")
path_add_fun <- file.path(folder_path, "Additional Functions.R")

#### Loading libraries ####

library(tidyverse)
library(corrplot)
library(plotly)
library(ggcorrplot)
library(ggiraph)
library(shiny)
library(leaflet)
library(lubridate)
library(plyr)
library(dplyr)
library(outliers)
library(ggplot2)
library(reshape)
library(data.table)
library(tidyr)
library(ggridges)
library(e1071)
library(foreach)
library(doParallel)


#### Source Additional Functions ####

source(path_add_fun)

#### Reading the Data ####

# Link to Kaggle competition: https://www.kaggle.com/c/ams-2014-solar-energy-prediction-contest/data

main_dataset <- readRDS(path_main_dataset)
dim(main_dataset)
head(main_dataset)

station_info <- read.csv(path_station_info)
dim(station_info)
head(station_info)

add_vars <- readRDS(path_add_vars)
dim(add_vars)
head(add_vars)

#### EDA ####

#### EDA - 1. Fix the formatting of the dates ####
main_dataset$Date <- as.Date(main_dataset$Date, format = "%Y%m%d")
main_dataset$Date
add_vars$Date <- as.Date(add_vars$Date, format = "%Y%m%d")
add_vars$Date

#### EDA - 2. Define the target variables. Train - test split ####
# Target variables - the stations: ACME:WYNO

main_train <- main_dataset[!is.na(main_dataset$ACME),]
main_train
nrow(main_train)

main_test <- main_dataset[is.na(main_dataset$ACME),c(1, 100:456)]
main_test
nrow(main_test)

#### EDA - 3. Counting NAs in the data ####

colSums(is.na(main_dataset))
# No missing values
colSums(is.na(main_train))
colSums(is.na(main_test))
# NAs in target variable match the length of the test dataset = 1796

colSums(is.na(station_info))
# No missing values

colSums(is.na(add_vars))
# Many missing values

#### EDA - 4. Outlier Analysis ####
#Finding values that can be potential outliers
possible_outliers1 <- outlier(main_train[,2:98], opposite = TRUE, logical = FALSE)
possible_outliers2 <- outlier(main_train[,2:98], opposite = FALSE, logical = FALSE)

possible_outliers1
possible_outliers2

#Station columns - overview 
summary(main_train[,2:98])


#Reshaping the data 
meltTrainData <- melt(main_train[,1:15], id="Date")

#Displaying station boxplots. Conclusion: The graphs didn't show outliers
ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplots: ACME -BUFF") +
  theme_light()+
  xlab("Stations")+
  ylab("Values")


meltTrainData <- melt(main_train[,c(1,16:31)], id="Date")

ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplots: BURB - ELRE") +
  theme_light()+
  xlab("Stations")+
  ylab("Values")


meltTrainData <- melt(main_train[,c(1,32:46)], id="Date")

ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplots: ERIC - IDAB") +
  theme_light()+
  xlab("Stations")+
  ylab("Values")


meltTrainData <- melt(main_train[,c(1,47:62)], id="Date")

ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplots: JAYX - NEWK") +
  theme_light()+
  xlab("Stations")+
  ylab("Values")


meltTrainData <- melt(main_train[,c(1,63:77)], id="Date")

ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplots: NINN - SEIL") +
  theme_light()+
  xlab("Stations")+
  ylab("Values")


meltTrainData <- melt(main_train[,c(1,78:92)], id="Date")

ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplots: SHAW - WATO") +
  theme_light()+
  xlab("Stations")+
  ylab("Values")


meltTrainData <- melt(main_train[,c(1,93:98)], id="Date")

ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplots: WAUR - WOOD") +
  theme_light()+
  xlab("Stations")+
  ylab("Values")

#Although the previous boxplots did not show outliers, we want to evaluate the station IDAB in detail, 
#since it presents higher values compared to the other stations. 
#Conclusion: There is one observation far away from the others.


meltTrainData <- melt(main_train[,c(1,46)], id="Date")

ggplot(meltTrainData) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplot: IDAB") +
  theme_light()+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  xlab("Stations")+
  ylab("Values")

#### EDA - 5. Statistics by Column ####

#Station columns - overview 
sapply(main_train[,2:98], summary)

#Additional variables - overview
sapply(add_vars[,2:101], summary)

#### EDA - 6. Correlation Analysis ####
abs(cor(main_train[,-1], method = "pearson")) > 0.5
abs(cor(main_train[,100:456], method = "pearson")) > 0.5

stations <- main_train[,2:99]
components <- main_train[,100:456]

## Looking at the correlations between the first 5 stations and the first 5 components

# Basic Plot

c.mat <- correl_matrix(stations, components, from_stat = 1, to_stat = 5, from_var = 1, to_var = 5); c.mat
p.mat <- sig_matrix(stations, components, from_stat = 1, to_stat = 5, from_var = 1, to_var = 5); p.mat

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr = c.mat, method="color", col=col(200), type="full", order="original", addCoef.col = "black", tl.col="black", tl.srt=45, p.mat = p.mat, sig.level = 0.05, insig = "blank", diag = TRUE)

## Zoom in interactive plot

g <- ggcorrplot(corr = c.mat, method="square", colors=c("#BB4444", "#FFFFFF", "#4477AA"), type="full", lab = TRUE, lab_col = "black", tl.col="black", tl.srt=45, p.mat = p.mat, sig.level = 0.05, insig = "blank", show.diag = TRUE)

ggplotly(g)


# Plot for all Stations and PCs (Interactive correlation plot with sliders)

c.mat <- correl_matrix(stations, components)


ui <- fluidPage(
  titlePanel("Correlation between Stations and PCs"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "slide0",
                  label = "Choose the number of componenets",
                  min = 1, 
                  max = 357,
                  value = c(1,357),
                  step = 1
      ),
      sliderInput(inputId = "slide1",
                  label = "Choose the number of stations",
                  min = 1, 
                  max = 98,
                  value = c(1,98),
                  step = 1
      )
    ),
    mainPanel(
      plotOutput(outputId = "pearson")
    )
  )
)

shinyApp(ui=ui,server=server1)

# Correlations with the additional variables
sum(add_vars[1:5113, 1] == main_train[1:5113,1])
train_add_vars <- add_vars[1:5113, ]
train_add_vars_fixed <- train_add_vars[,-1]

# Will have to ensure impute all NAs or drop the rows with NAs to run the correlation matrix
cor(train_add_vars[,-1], use = "complete.obs")

# Interactive plot with all additional variables
ui <- fluidPage(
  titlePanel("Correlation between Additional Variables"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "slide0",
                  label = "Choose the number of variables",
                  min = 2, 
                  max = 100,
                  value = c(2,100),
                  step = 1
      )
    ),
    mainPanel(
      plotOutput(outputId = "pearson")
    )
  )
)

shinyApp(ui=ui,server=server2)

#### EDA - 7. Clustering ####

test<-main_train[, 1:99]
test2<-as.data.frame(test) %>%  gather(key="station",value="solar", 2:99)
test3<-test2 %>% spread(key=Date,value=solar)
rownames(test3)<-test3$station
station_kmean2<-kmeans(test3[, names(test3) != "station"], 8, nstart = 10)
station_kmean2$cluster

stat_clust <- cbind(test3, cluster = station_kmean2$cluster)
head(stat_clust)

final_clust=merge(station_info,stat_clust,by.x="stid",by.y="station")
head(final_clust)

final_clust %>% ggplot(aes(elon,nlat, color=as.factor(cluster))) +geom_point(size=2)
#longitude seems more discriminatory than lattitude

wssplot(test3[, names(test3) != "station"])

#### EDA - 8. Distribution Plots ####

test<-main_train[, 1:99]
head(main_train)
head(test)
test2<-as.data.frame(test) %>%  gather(key="station",value="solar", 2:99)
head(stat_clust)


stationbox<-merge(test2,stat_clust,by="station")
head(stationbox)

stationbox  %>% ggplot(aes(reorder(station,solar_value,FUN=median),solar_value,fill=as.factor(cluster))) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

#### EDA - 9. Maps ####

# Basic Map

station_info$cluster <- stat_clust$cluster
station_info

m <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=station_info[,3], lat=station_info[,2],
    label = station_info[,1],
    radius = get_radius(station_info),
    popup = get_pop_up_info(station_info),
    color = getColor(station_info),
    fillOpacity = 0.6)
m

# Map for the last year in the data
max(main_train$Date)
sum(grepl("2007", main_train$Date))

train_07 <- main_train[grepl("2007", main_train$Date), 1:99]
train_07_month <- train_07
train_07_month$Date <- as.character(train_07_month$Date)

search_pattern <- c("2007-01-", "2007-02-", "2007-03-", "2007-04-", "2007-05-", "2007-06-", "2007-07-", "2007-08-", "2007-09-", "2007-10-", "2007-11-", "2007-12-")

for (i in 1:12) {
  train_07_month$Date[grepl(search_pattern[i], train_07_month$Date)]<- rep(month.name[i], sum(grepl(search_pattern[i], train_07_month$Date)))
}

df_07_map <- add_my_rows(train_07_month)
df_07_map[2:ncol(df_07_map)] <- lapply(df_07_map[2:ncol(df_07_map)], as.integer)

df_07_map <- merge(df_07_map, station_info, by.x = "Station", by.y = "stid")
df_07_map

Jan <- leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(
      lng=df_07_map$elon, lat=df_07_map$nlat,
      label = df_07_map$Station,
      radius = get_radius(df_07_map),
      popup = get_pop_up_info2(df_07_map, "January"),
      color = getColor2(df_07_map, "January"),
      fillOpacity = 0.6)


Feb <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "February"),
    color = getColor2(df_07_map, "February"),
    fillOpacity = 0.6)


Mar <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "March"),
    color = getColor2(df_07_map, "March"),
    fillOpacity = 0.6)


Apr <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "April"),
    color = getColor2(df_07_map, "April"),
    fillOpacity = 0.6)


May <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "May"),
    color = getColor2(df_07_map, "May"),
    fillOpacity = 0.6)

Jun <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "June"),
    color = getColor2(df_07_map, "June"),
    fillOpacity = 0.6)


Jul <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "July"),
    color = getColor2(df_07_map, "July"),
    fillOpacity = 0.6)


Aug <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "August"),
    color = getColor2(df_07_map, "August"),
    fillOpacity = 0.6)


Sep <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "September"),
    color = getColor2(df_07_map, "September"),
    fillOpacity = 0.6)


Oct <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "October"),
    color = getColor2(df_07_map, "October"),
    fillOpacity = 0.6)


Nov <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "November"),
    color = getColor2(df_07_map, "November"),
    fillOpacity = 0.6)


Dec <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=df_07_map$elon, lat=df_07_map$nlat,
    label = df_07_map$Station,
    radius = get_radius(df_07_map),
    popup = get_pop_up_info2(df_07_map, "December"),
    color = getColor2(df_07_map, "December"),
    fillOpacity = 0.6)


Jan
Feb
Mar
Apr
May
Jun
Jul
Aug
Sep
Oct
Nov
Dec

#### ML models ####
set.seed(1234)
i<-2

predset <- main_dataset[is.na(main_dataset$ACME)]
Date<-predset[,1]
final_predictions<-Date
head(final_predictions)

for(i in 2:99){
  main_train <- main_dataset[!is.na(main_dataset$ACME),1:109] 
  station<-names(main_train)[i]
  valuemean<-mean(main_train[,get(station)], na.rm = TRUE)
  main_train[,"Month"]<-month(main_train$Date)
  main_train$Month
  
  # cleaning the dataset
  dat<-main_train[,get(station)]
  head(dat)
  var<-main_train[,c(100:105)]
  head(var)
  
  dat<-cbind(dat,var)
  as.data.frame(dat)
  names(dat)[1]<-station
  head(dat)
  
  # row indices for training data (70%)
  train_index <- sample(1:nrow(dat), 0.7*nrow(dat))  
  
  # row indices for validation data (15%)
  val_index <- sample(setdiff(1:nrow(dat), train_index), 0.15*nrow(dat))  
  
  # row indices for test data (15%)
  test_index <- setdiff(1:nrow(dat), c(train_index, val_index))
  
  # split data
  train <- dat[train_index,] 
  val <- dat[val_index,] 
  test  <- dat[test_index,]
  
  head(test)
  
  train
  head(train)
  nrow(train)
  
  
  # Start cluster

  stopImplicitCluster()
  registerDoParallel(cores = detectCores())
  
  ### Define grid
  c_values <- 10^seq(from = -2, to = 1, by = 0.5)
  eps_values <- 10^seq(from = -2, to = 0, by = 0.5)
  gamma_values <- 10^seq(from = -3, to = -1, by = 0.5)
  
  
  ### Compute grid search                                            
  grid_results <-  foreach (c = c_values, .combine = rbind)%:%        
    foreach (eps = eps_values, .combine = rbind)%:%       
    foreach (gamma = gamma_values, .combine = rbind)%dopar%{
      
      print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma))
      
      # train SVM model with a particular set of hyperparamets
      model <- svm(get(station) ~ ., data = train, kernel="radial",           
                   cost = c, epsilon = eps, gamma = gamma)
      
      # Get model predictions
      predictions_train <- predict(model, newdata = train)
      predictions_val <- predict(model, newdata = val)
      
      # Get errors
      errors_train <- predictions_train - train[,get(station)]                
      errors_val <- predictions_val - val[,get(station)]                     
      
      # Compute Metrics
      mae_train <- round(mean(abs(errors_train)), 2)                
      mae_val <- round(mean(abs(errors_val)), 2)                
      
      # Build comparison table
      data.table(c = c, eps = eps, gamma = gamma, 
                 mae_train = mae_train,
                 mae_val = mae_val)
    }
  
  # Order results by increasing mse and mae
  grid_results <- grid_results[order(mae_val, mae_train)]
  
  # Check results
  best <- grid_results[1]
  
  ### Train final model
  # train SVM model with best found set of hyperparamets
  model <- svm(get(station) ~ ., data = train, kernel="radial",                 
               cost = best$c, epsilon = best$eps, gamma = best$gamma)
  
  # Get model predictions
  predictions_train <- predict(model, newdata = train)
  predictions_val <- predict(model, newdata = val)
  predictions_test <- predict(model, newdata = test)
  
  # Get errors
  errors_train <- predictions_train - train[,get(station)]          
  errors_val <- predictions_val - val[,get(station)]                  
  errors_test <- predictions_test - test[,get(station)]               
  
  # Compute Metrics
  mae_train <- round(mean(abs(errors_train)), 2)
  mae_val <- round(mean(abs(errors_val)), 2)
  mae_test <- round(mean(abs(errors_test)), 2)
  
  ## Summary
  sprintf("MAE_train = %s - MAE_val = %s - MAE_test = %s", mae_train, mae_val, mae_test)
  
  
  #predictions
  
  predset <- main_dataset[is.na(main_dataset[,get(station)]),1:109] 
  Date<-predset[,1]
  head(predset)
  predset[,"Month"]<-month(predset$Date)
  predset$Month
  
  
  var2<-predset[,get(station)] 
  head(var2)
  var3<-predset[,c(100:105)]
  
  predset<-cbind(var2,var3)
  head(predset)
  
  as.data.frame(predset)
  names(predset)[1]<-station
  predset<-as.data.frame(predset)
  head(predset)
  predset[,1]<- valuemean
  
  predictions<- predict(model, newdata = predset)
  head(predictions)
  
  predict_df<-as.data.frame(predictions)
  names(predict_df)[1]<-station
  head(predict_df)
  
  final_predictions<-cbind(final_predictions,predict_df)
  
}

final_predictions


write.csv(final_predictions, file.path(folder_path, "predictions.csv"))

#### References ####
# EDA: https://r4ds.had.co.nz/exploratory-data-analysis.html

# Plotting Oklahoma map. https://github.com/dannguyen/ok-earthquakes-RNotebook/blob/master/chapter-2-basic-r-concepts.Rmd
# Need to ?register_google
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
# Counting NAs for each column: https://stackoverflow.com/questions/24027605/determine-the-number-of-na-values-in-a-column
# Corrplot: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
# Interactive corrplot with slider: https://stackoverflow.com/questions/52420397/bring-out-correlation-with-a-double-ended-range-slider
# Using Leaflet (https://rstudio.github.io/leaflet/)
# Numerical indexing using aggregation: https://stackoverflow.com/questions/68145836/aggregate-function-in-r-using-column-index-numbers-rather-than-names