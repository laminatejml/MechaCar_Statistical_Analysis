# retrieve packages
library(dplyr)
library(tidyverse)

#read mpg data
mechacar_table <- read.csv(file= "C:\\Users\\relam\\Class\\Putting the R in AutosRU\\MechaCarChallenge\\MechaCar_mpg.csv", check.names = F, stringsAsFactors = F)

# add linear regression 
mpglr_df <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD ,data= MechaCar_mpg)

#add summary function
summary(mpglr_df)

#read in suspension coil data
suspcl_df <- read_csv(file='Suspension_Coil.csv')

#add complete summary
compsum <- suspcl_df %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),Std=sd(PSI))

#add lot summary
lotsum <- suspcl_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),Std=sd(PSI),.groups = 'keep')

#write an RScript using the t.test() function to determine if the PSI across 
#all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch
t.test(suspcl_df$PSI, mu = 1500)

#write three more RScripts in your MechaCarChallenge.RScript using the t.test() function 
#and its subset() argument to determine if the PSI for each manufacturing lot is statistically 
#different from the population mean of 1,500 pounds per square inch
t.test(subset(suspcl_df,Manufacturing_Lot=='Lot1')$PSI,mu=1500)

t.test(subset(suspcl_df,Manufacturing_Lot=='Lot2')$PSI,mu=1500)

t.test(subset(suspcl_df,Manufacturing_Lot=='Lot3')$PSI,mu=1500)