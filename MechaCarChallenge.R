library(dplyr)
library(tidyverse)

# DELIVERABLE 1
car_data <- read.csv("MechaCar.csv",check.names=F,stringsAsFactors = F)


mecha_lm <- lm(mpg ~ vehicle_length	+ vehicle_weight + spoiler_angle +
                 ground_clearance + AWD,data=car_data)
summary(mecha_lm)

# DELIVERABLE 2
suspension_data <- read.csv("Suspension_Coil.csv",check.names=F,stringsAsFactors = F)

total_summary <- suspension_data %>% summarize(mean=mean(PSI), median=median(PSI)
                                               ,var=var(PSI),sd=sd(PSI))

lot_summary <- suspension_data %>% group_by(Manufacturing_Lot) %>%
  summarize(mean=mean(PSI), median=median(PSI),var=var(PSI),sd=sd(PSI))

# DELIVERABLE 3
t.test(suspension_data$PSI, mu=1500)
t.test(subset(suspension_data,Manufacturing_Lot=="Lot1")$PSI,mu=1500)
t.test(subset(suspension_data,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(suspension_data,Manufacturing_Lot=="Lot3")$PSI,mu=1500)