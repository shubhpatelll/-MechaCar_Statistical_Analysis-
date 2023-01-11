# Part 1: Linear Regression to Predict MPG

# 3. load dplyr and tidyverse packages
library(dplyr)
library(tidyverse)

# 4. Import and read in the MechaCar_mpg.csv file as a dataframe.
mechar_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# 5. Perform linear regression using the lm() function
#generate multiple linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +ground_clearance + AWD,data=mechar_df) 


# 6. Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +ground_clearance + AWD,data=mechar_df) )



# Part 2: Create Visualizations for the Trip Analysis
# 2. import and read in the Suspension_Coil.csv file as a table.
coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# 3. creates a total_summary dataframe using the summarize() function to get the mean, median, variance, and standard deviation of the suspension coil’s PSI column
total_summary <- coil_table %>% summarize(Mean=mean(PSI),
                                          Median=median(PSI),
                                          Variance=var(PSI),
                                          SD=sd(PSI),
                                          .groups = 'keep') 

# 4. creates a lot_summary dataframe using the group_by() and the summarize() functions to group each manufacturing lot by the mean, median, variance, and standard deviation of the suspension coil’s PSI column
lot_summary <- coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),
                                                                        Median=median(PSI),
                                                                        Variance=var(PSI),
                                                                        SD=sd(PSI),
                                                                        .groups = 'keep') 

# Part 3: T-Tests on Suspension Coils
# 1. determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch
t.test(log10(coil_table$PSI),mu=log10(1500))


# 2. determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch.
# First create three tables for each lot
mf_lot1 <- subset(coil_table, Manufacturing_Lot=="Lot1")
mf_lot2 <- subset(coil_table, Manufacturing_Lot=="Lot2")
mf_lot3 <- subset(coil_table, Manufacturing_Lot=="Lot3")

# Next perform three t-tests
t.test(log10(mf_lot1$PSI),mu=log10(1500))
t.test(log10(mf_lot2$PSI),mu=log10(1500))
t.test(log10(mf_lot3$PSI),mu=log10(1500))
