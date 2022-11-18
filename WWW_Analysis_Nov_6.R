# R Code for
# Coupled human-natural system impacts of a winter weather whiplash event 
# 

# Christopher Macdonald Hewitt (University of Western Ontario)
# Nora Casson (University of Winnipeg)
# Alix Contosta (University of New Hampshire)
# John Campbell (USDA - FS)
# David Lutz (Dartmouth College)
# Anita Morzillo (University of Connecticut)
# Irena Creed (University of Toronto)

# This code was written by Christopher Macdonald Hewitt with contributions by Nora Casson and Alix Contosta.  

# Load libraries.
library(rpart)
library(randomForest)
library(regclass)

# Set working directory.
setwd("C:/Users/Chris Hewitt/Downloads/Input_Data_Nov_2022")

# Load the different data sources.
data2 <- read.csv("County_Area_and_SVI_Data.csv")
Newspaper <- read.csv("Newspaper_Analysis_Output.csv") # Use Results tab from Newspaper_Analysis Excel file.
data2<- data2[which(data2$R_PL_THE_4>-999.0000),]
data3 <- Newspaper[,c(3,7,15,16,17)]

# Snowfall
Snowfall <- read.csv("Snow_28_31_Table.csv")
Snowfall$MEAN_mm <- Snowfall$MEAN * 25.4 # Converts from inches to mm.
Snowfall2 <- Snowfall[,c(1,11)]
names(Snowfall2)[names(Snowfall2) == 'MEAN_mm'] <- 'SWE_Oct_28_31'

# Roads
Sec_Road_dis <- read.csv("Secondary_Roads_Distance.csv")

# Leaf Area Index (LAI)
lai <- read.csv("lai_10_28.csv")
lai2 <- lai[,c(1,8)]
names(lai2)[names(lai2)=='MEAN'] <- 'LAI_Mean'

# Merge the datasets together.  
data2 <- merge(data2,data3,by="GEO_ID", all.x = T)
data2 <- merge(data2,Snowfall2, by="GEO_ID", all.x = T)
data2 <- merge(data2,Sec_Road_dis, by="GEO_ID")
data2 <- merge(data2,lai2, by="GEO_ID")

# Assign 0 to counties with no data.  
data2[is.na(data2)] <- 0

# Calculate road density, population density values
data2$Sec_Road_Den <- data2$Sum_Length / data2$Area_KM
data2$Pop_Den <- data2$TOTPOP / data2$Area_KM

# Remove data points with error values.  
data<- data2[which(data2$R_PL_THE_4>-999.0000),]

# Restrict data to only areas with measured snowfall.  
data <- data[which(data$SWE_Oct_28_31>0),]

# Attach data to R frame.  
attach(data)

# Set Dependent variable.  
Depend <- Rate

# function to calculate rsq from a rpart object 

rsq_func_rpart<-function(x) {
  tmp <- printcp(x)
  rowmin <- which(tmp[,4] == min(tmp[,4]))
  rsq.val <- 1-tmp[rowmin,c(3,4)]
  rsq<- as.numeric(rsq.val[1])
  return(rsq)
}

# function to calculate aic from a rpart object 

aic_func_rpart<-function(x) {
  sumtree<-summarize_tree(x)
  return(sumtree$aic)
}

# rpart models

# Both socio-economic & ecological variables combined.  
SWE_Pop_Road_Inc_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + 	Sec_Road_Den	+ E_PCI + LAI_Mean		, data = data)
SWE_Pop_Road_SVI_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + 	Sec_Road_Den	+ R_PL_THE_4 + LAI_Mean		, data = data)
SWE_Pop_Road_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + 	Sec_Road_Den + LAI_Mean		, data = data)
SWE_Pop_Inc_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + E_PCI + LAI_Mean		, data = data)
SWE_Pop_SVI_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + R_PL_THE_4 + LAI_Mean			, data = data)
SWE_Pop_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + LAI_Mean					, data = data)
SWE_Road_Inc_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Sec_Road_Den	+ E_PCI + LAI_Mean		, data = data)
SWE_Road_SVI_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Sec_Road_Den	+ R_PL_THE_4 + LAI_Mean		, data = data)
SWE_Road_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 + Sec_Road_Den	+ LAI_Mean			, data = data)
SWE_Inc_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	E_PCI + LAI_Mean	, data = data)
SWE_SVI_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	R_PL_THE_4 + LAI_Mean			, data = data)

# Socio-economic variables only.
SWE_Pop_Road_Inc	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + 	Sec_Road_Den	+ E_PCI			, data = data)
SWE_Pop_Road_SVI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + 	Sec_Road_Den	+ R_PL_THE_4			, data = data)
SWE_Pop_Road	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + Sec_Road_Den				, data = data)
SWE_Pop_Inc	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + E_PCI 			, data = data)
SWE_Pop_SVI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den + R_PL_THE_4					, data = data)
SWE_Pop	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Pop_Den 					, data = data)
SWE_Road_Inc	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Sec_Road_Den + E_PCI			, data = data)
SWE_Road_SVI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Sec_Road_Den + R_PL_THE_4			, data = data)
SWE_Road	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	Sec_Road_Den				, data = data)
SWE_Inc	<- rpart(	Depend ~ 	SWE_Oct_28_31 + E_PCI 			, data = data)
SWE_SVI	<- rpart(	Depend ~ 	SWE_Oct_28_31 + R_PL_THE_4					, data = data)

# Ecological variables only.
SWE_LAI	<- rpart(	Depend ~ 	SWE_Oct_28_31 +	LAI_Mean		, data = data)

# SWE variable only.
SWE	<- rpart(	Depend ~ 	SWE_Oct_28_31 						, data = data)

# function to prune rpart models based on complexity parameter associated with minimum error
prune_func<-function(x) {
  cptable<-x$cptable[which.min(x$cptable[,"xerror"]),"CP"]
  prune_tree<-prune(x, cptable)
  return(prune_tree)
}

# Model list.
model_list <- list(SWE_Pop_Road_Inc_LAI,	SWE_Pop_Road_SVI_LAI,	SWE_Pop_Road_LAI,	
                   SWE_Pop_Inc_LAI,	SWE_Pop_SVI_LAI,	SWE_Pop_LAI,	SWE_Road_Inc_LAI,	SWE_Road_SVI_LAI,	SWE_Road_LAI,	
                   SWE_Inc_LAI,	SWE_SVI_LAI,			
                   SWE_Pop_Road_Inc,	SWE_Pop_Road_SVI,	SWE_Pop_Road,	SWE_Pop_Inc,	SWE_Pop_SVI,	SWE_Pop,	SWE_Road_Inc,	
                   SWE_Road_SVI,	SWE_Road,	SWE_Inc,	SWE_SVI, SWE_LAI,	SWE
                   )

# Model names.
model_names <- c("SWE_Pop_Road_Inc_LAI",	"SWE_Pop_Road_SVI_LAI",	"SWE_Pop_Road_LAI",	
                 "SWE_Pop_Inc_LAI",	"SWE_Pop_SVI_LAI",	"SWE_Pop_LAI",	"SWE_Road_Inc_LAI",	"SWE_Road_SVI_LAI",	"SWE_Road_LAI",	
                 "SWE_Inc_LAI",	"SWE_SVI_LAI",			
                 "SWE_Pop_Road_Inc",	"SWE_Pop_Road_SVI",	"SWE_Pop_Road",	"SWE_Pop_Inc",	"SWE_Pop_SVI",	"SWE_Pop",	"SWE_Road_Inc",	
                 "SWE_Road_SVI",	"SWE_Road",	"SWE_Inc",	"SWE_SVI", "SWE_LAI",	"SWE"
                 )

# Model list together with pruning models.
prune_model_list<-lapply(model_list, prune_func)

# Creates table with AIC, AICc and rsq values.  
AIC_rsq_table<-cbind.data.frame(model_names, unlist(lapply(prune_model_list, aic_func_rpart)),unlist(lapply(prune_model_list, rsq_func_rpart)))

# Random forest analysis

RF_SWE_Road_Inc_LAI <- randomForest(Depend~SWE_Oct_28_31 +	Sec_Road_Den	+ E_PCI + LAI_Mean, ntree = 1000, importance = T, type = regression)
RF_SWE_Road_Inc_LAI
importance(	RF_SWE_Road_Inc_LAI	, type = 1)									

### TABLES and FIGURES ###

########################################################################

### Table 1

### Created from examples of themes in newspapers.

### Table 2

### Output AIC_rsq_table.
write.csv(AIC_rsq_table, "AIC_Table_rpart.csv")

### Formatted in Excel.  

########################################################################

### Figure 1

### Conceptual framework, modified after Casson et al., 2019.

### Figure 2

### Output file to generate maps.
write.csv(data, "Input_Data_Sept_1.csv")

### Created in ArcGIS. Maps of predictor variables per county. 
### (A) Average 4-day snowfall accumulation (mm snow water equivalent). 
### (B) Average leaf area index (unitless) for October 28, 2011.
### (C) Road density (km km-2). 
### (D) Population density (people km-2).
### (E) Average per capita income in $USD.
### (F) Social vulnerability index (unitless).

### Figure 3

### extract predicted values
predicted<-RF_SWE_Road_Inc_LAI$predicted
data_predicted<-cbind(data, predicted)

### Output file to generate maps.
write.csv(data_predicted, "Output_Data_Sept_1.csv")

### Created in ArcGIS 
### (A) Number of newspaper articles about the snowstorm per county. 
### (B) Number of publishing newspapers of those articles in A. 
### (C) Observed impact of the storm expressed as the number of articles divided by the number of publishing newspapers. 
### (D) Predicted impact of the Halloween Nor'Easter.

### Figure 4

### Modified R plots.

### (A) Variable importance plot of the random forests analysis showing the relative importance of each predictor variable. 

### Plot importance plot from random forest analysis.
varImp<-varImpPlot(RF_SWE_Road_Inc_LAI, type = 1, yaxt = "n")

### (B-E) Partial dependence plots of each predictor variable used in the random forests analysis.
### (B) Average per capita income in $USD.
### (C) Average leaf area index (unitless) for October 28, 2011.
### (D) Road density (km km-2).
### (E) Average 4-day snowfall accumulation (mm snow water equivalent).

### Partial dependence plots 
### Restrict data to only variables of interest.
namesRF<-c("Rate", "SWE_Oct_28_31", "Sec_Road_Den", "LAI_Mean", "E_PCI")
dataRF<-data[namesRF]

### Partial dependence plots by variable.
partialPlot(RF_SWE_Road_Inc_LAI, dataRF, x.var = SWE_Oct_28_31, main = "", ylab = "Response Variable", xlab = "SWE (mm)")
partialPlot(RF_SWE_Road_Inc_LAI, dataRF, x.var = Sec_Road_Den, main = "", ylab = "Response Variable", xlab = "Road Density")
partialPlot(RF_SWE_Road_Inc_LAI, dataRF, x.var = LAI_Mean, main = "", ylab = "Response Variable", xlab = "LAI")
partialPlot(RF_SWE_Road_Inc_LAI, dataRF, x.var = E_PCI, main = "", ylab = "Response Variable", xlab = "Income")

# Order the plots in descending order of variable importance.
imp <- importance(RF_SWE_Road_Inc_LAI)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(3, 2), mar = c(2,2,2,2), oma= c(1,1,1,1))
for (i in seq_along(impvar)) {
  partialPlot(RF_SWE_Road_Inc_LAI, dataRF, impvar[i], xlab=impvar[i], ylab = "Rate",
              main="")
}
par(op)
