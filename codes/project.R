###################################################
################ Loading Libraries ################
###################################################

library(ggplot2)
library(varhandle)

###################################################
################## Loading Data ###################
###################################################

# sets directory of current file as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# reads dataset
data <- read.csv2("../data/cardio.csv")

# explores types of the attributes
sapply(data, class)

# summerizes dataset
summary(data)

###################################################
################# Managing Data ###################
###################################################

# drops 'id' attribute from the dataset
data <- subset(data, select = -c(id))

# recoding categorical or ordered data into readable format
data$cardio <- as.factor(ifelse(data$cardio == 0, "Negative", "Positive"))
data$active <- as.factor(ifelse(data$active == 0, "Inactive", "Active"))
data$alco <- as.factor(ifelse(data$alco == 0, "Non-alcoholic", "Alcoholic"))
data$smoke <- as.factor(ifelse(data$smoke == 0, "Non-smoker", "Smoker"))
data$gluc <- as.ordered(ifelse(data$gluc == 1, "Normal", ifelse(data$gluc == 2, "Above Normal", "Well Above Normal")))
data$cholesterol <- as.ordered(ifelse(data$cholesterol == 1, "Normal", ifelse(data$cholesterol == 2, "Above Normal", "Well Above Normal")))
data$gender <- as.factor(ifelse(data$gender == 1, "Woman", "Man"))

# converts 'weight' from character to numeric
data$weight <- unfactor(data$weight)

# converts unit of 'age' from days to years
data$age <- round(data$age / 365, digits = 0)

# removes observation for which 'ap_hi' or 'ap_lo' has negative values
data <- subset(data, ap_hi > 0 & ap_lo > 0)

# removes 'ap_hi' and 'ap_lo' outliers
iqr_multiplier = 3
ap_hi_upper_threshold <- quantile(data$ap_hi, .75) + iqr_multiplier * (quantile(data$ap_hi, .75) - quantile(data$ap_hi, .25))
ap_hi_lower_threshold <- quantile(data$ap_hi, .25) - iqr_multiplier * (quantile(data$ap_hi, .75) - quantile(data$ap_hi, .25))
ap_lo_upper_threshold <- quantile(data$ap_lo, .75) + iqr_multiplier * (quantile(data$ap_lo, .75) - quantile(data$ap_lo, .25))
ap_lo_lower_threshold <- quantile(data$ap_lo, .25) - iqr_multiplier * (quantile(data$ap_lo, .75) - quantile(data$ap_lo, .25))
data <- subset(data, ap_hi < ap_hi_upper_threshold & ap_hi > ap_hi_lower_threshold & ap_lo < ap_lo_upper_threshold & ap_lo > ap_hi_lower_threshold)

# removes 'weight' outliers
weight_upper_threshold <- quantile(data$weight, .75) + iqr_multiplier * (quantile(data$weight, .75) - quantile(data$weight, .25))
weight_lower_threshold <- quantile(data$weight, .25) - iqr_multiplier * (quantile(data$weight, .75) - quantile(data$weight, .25))
data <- subset(data, weight > weight_lower_threshold & weight < weight_upper_threshold)

# explores types of the newly encoded attributes
sapply(data, class)

# summerizes newly encoded dataset
summary(data)

###################################################
############## Exploratory Analysis ###############
###################################################

