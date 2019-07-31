
rm(list=ls())
library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)
#install.packages("dummies")

train <- read.csv("C://Users/jenil/Desktop/SAS project/train.csv",na.strings = c(NA,"","unknown","Unknown"))
train_label <- read.csv("C://Users/jenil/Desktop/SAS project/trainlabel.csv",na.strings = c(NA,"","unknown","Unknown"))
pump_data_new <- join(train,train_label)


pump_data_new = pump_data_new %>%
  mutate(funder = ifelse(funder == 0, NA, as.character(funder)))  %>%
  mutate(installer = ifelse(installer == 0, NA, as.character(installer))) %>%
  mutate(gps_height = ifelse(gps_height == 0, NA, gps_height)) %>%
  mutate(population = ifelse(population == 0, NA, population)) %>%
  mutate(amount_tsh = ifelse(amount_tsh == 0, NA, amount_tsh)) %>%
  mutate(construction_year = ifelse(construction_year == 0, NA, construction_year))



mean.na = function(x) {	mean(is.na(x)) }
t(pump_data_new %>% summarise_each(funs(mean.na)))


pump_data_new = pump_data_new %>% select( - amount_tsh)
pump_data_new = pump_data_new %>% select( - construction_year, -population, -gps_height)
pump_data_new = pump_data_new %>% select( - id, -recorded_by)
pump_data_new = pump_data_new %>% select( - date_recorded , -scheme_name,-wpt_name )

#extraction_type
pump_data_new %>% group_by(pump_data_new$extraction_type_class,pump_data_new$extraction_type_group, pump_data_new$extraction_type) %>% tally()


pump_data_new = pump_data_new %>% mutate(extraction_type = revalue(extraction_type,c("cemo" = "other motorpump","climax" = "other motorpump","other - mkulima/shinyanga" = "other handpump",
"other - play pump" = "other handpump","walimi" = "other handpump","other - swn 81" = "swn","swn 80" = "swn","india mark ii" = "india mark","india mark iii" = "india mark"))) %>% select( - extraction_type_group ) 

#management_group
pump_data_new %>% group_by(management_group, management) %>% tally()


#payment
pump_data_new %>% group_by(payment, payment_type) %>% tally()
pump_data_new = pump_data_new %>% select( - payment)

#water_quality
pump_data_new %>% group_by(water_quality, quality_group) %>% tally()
pump_data_new = pump_data_new %>% select( - water_quality)

#quantity
pump_data_new %>% group_by(quantity, quantity_group) %>% tally()
pump_data_new = pump_data_new %>% select( - quantity_group)

#source
pump_data_new %>% group_by(source_type, source_class,source) %>% tally()
#pump_data_new = pump_data_new %>% select( - source_type)

#waterpoint_type
pump_data_new %>% group_by(waterpoint_type, waterpoint_type_group) %>% tally()
pump_data_new = pump_data_new %>% select( - waterpoint_type)

#lga
pump_data_new %>% group_by(lga) %>% tally()
pump_data_new = pump_data_new %>% mutate(lga = ifelse( grepl(" Rural", lga), "Rural",ifelse( grepl(" Urban", lga), "Urban","Other")))

#region
pump_data_new %>% group_by(region, region_code,district_code,latitude,longitude,lga) %>% tally()
pump_data_new = pump_data_new %>% select( - region_code , - district_code, - latitude, - longitude, - subvillage, - ward)












cbind(
  pump_data_new %>% group_by(funder) %>% tally() %>% arrange(desc(n)) %>% slice(1:10),
  pump_data_new %>% group_by(installer) %>% tally() %>% arrange(desc(n)) %>% slice(1:10)
  
  
  
)






reduce.num.levels = function(x, nlevels = 12) {
  levels = table(x)
  if ( n_distinct(x) > (nlevels+1) )  {
    small.levels = names(sort(levels, decreasing = TRUE)[ - seq(nlevels)])
    x[x %in% small.levels] = "other"
  }
  return (x)
}
reduce.size.levels = function(x, min.size = 500) {
  levels = table(x)
  if ( min(levels) < min.size) {
    small.levels = names(levels[levels < min.size])
    x[x %in% small.levels] = "other"
  }
  return (x)
}
myreduce.levels = function(x) {
  return (reduce.num.levels(reduce.size.levels(x)))
}


pump_data_new = pump_data_new %>%
  mutate(funder = myreduce.levels(funder)) %>%
  mutate(installer = myreduce.levels(installer)) 




pump_data_new = pump_data_new %>% select( - num_private ) %>%  filter(scheme_management != "None" | is.na(scheme_management))

mean.na = function(x) {	mean(is.na(x)) }
t(pump_data_new %>% summarise_each(funs(mean.na)))

pump_data_new <- na.omit(pump_data_new)


pump_data_new$status_group[pump_data_new$status_group=="functional needs repair"] <- "non functional"
pump_data_new$status_group[pump_data_new$status_group=='functional needs repair']

pump_data_new$lga <- as.factor(pump_data_new$lga)
pump_data_new$installer <- as.factor(pump_data_new$installer)
pump_data_new$funder <- as.factor(pump_data_new$funder)
levels(pump_data_new$status_group) <- droplevels.factor(pump_data_new$status_group)


#write.csv(pump_data_new, file = "ssssssss.csv")




indx <- sapply(pump_data_new, is.factor)
pump_data_new[indx] <- lapply(pump_data_new[indx], function(x) as.numeric(x))
idx=seq(1,as.integer(0.7*nrow(pump_data_new)),by=1)
test_data=pump_data_new[-idx,]
training_data=pump_data_new[idx,]
# Load the randomForest library
library(randomForest)
r_model <- randomForest(x=training_data[,1:19],y=training_data[,20],importance = TRUE)

prediction=predict(r_model,newdata = test_data[,1:19])




# Set seed and create a random forest classifier
set.seed(42)
model_forest <- randomForest(status_group~., data = pump_data_new, importance = TRUE, ntree = 5, nodesize = 2)

# Use random forest to predict the values in train
pred_forest_train <- predict(model_forest, pump_data_new)

# Observe the first few rows of your predictions
head(pred_forest_train)

# Confusion Matrix for Accuracy
prediction[prediction < 1.45] <- 1
prediction[prediction >= 1.45] <- 2
correct=test_data[,20]==prediction

cbind(test_data$status_group,prediction,correct)
e=table(correct)
error_rate=e[names(e)==FALSE]/nrow(test_data)
error_rate
table(prediction,test_data$status_group)

#to assess the predictive utility of the given variables in the model. According to the output, what variable is LEAST important based on the mean decrease in accuracy?
varImpPlot(r_model)
importance(r_model)
