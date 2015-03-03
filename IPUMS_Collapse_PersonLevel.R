##################################
###########IN PROGRESS############
##################################


# Install and Load Packages
install.packages("doBy")
library(doBy)

# Import CSV
data <- read.csv("~/Desktop/MW_P_2008.csv")

# Define Functions
weightsum <- function(x) return(sum(x)*10)
narm_mean <- function(x) return(mean(x, na.rm=TRUE))


# Separate Variable: RESIDENT
data$resident_1 <- ifelse(data$resident==1,1,0)
data$resident_2 <- ifelse(data$resident==2,1,0)
data$resident_3 <- ifelse(data$resident==3,1,0)
data$resident_4 <- ifelse(data$resident==4,1,0)
data$resident_9 <- ifelse(data$resident==9,1,0)
# Create Collapsed Variabes: RESIDENT
resident_1 <- summaryBy(resident_1 ~ geo2a_mw, FUN=mean, data=data)
resident_2 <- summaryBy(resident_2 ~ geo2a_mw, FUN=mean, data=data)
resident_3 <- summaryBy(resident_3 ~ geo2a_mw, FUN=mean, data=data)
resident_4 <- summaryBy(resident_4 ~ geo2a_mw, FUN=mean, data=data)
resident_9 <- summaryBy(resident_9 ~ geo2a_mw, FUN=mean, data=data)
# Merge: RESIDENT
resident <- merge(resident_1, resident_2, by="geo2a_mw")
resident <- merge(resident, resident_3, by="geo2a_mw")
resident <- merge(resident, resident_4, by="geo2a_mw")
resident <- merge(resident, resident_9, by="geo2a_mw")


# Variable: MOMLOC
# Variable: POPLOC
# Variable: SPLOC
# Variable: PARRULE
# Variable: SPRULE
# Variable: STEPMOM
# Variable: STEPPOP
# Variable: POLYMAL
# Variable: POLY2ND
# Variable: FAMUNIT
# Variable: FAMSIZE
# Variable: NCHILD
# Variable: NCHLT5
# Variable: ELDCH
# Variable: YNGCH
# Variable: RELATE
# Variable: RELATED
# Variable: AGE

# Separate Variable: AGE2
data$age2_01 <- ifelse(data$age2==01,1,0)
data$age2_02 <- ifelse(data$age2==02,1,0)
data$age2_03 <- ifelse(data$age2==03,1,0)
data$age2_04 <- ifelse(data$age2==04,1,0)
data$age2_05 <- ifelse(data$age2==05,1,0)
data$age2_06 <- ifelse(data$age2==06,1,0)
data$age2_07 <- ifelse(data$age2==07,1,0)
data$age2_08 <- ifelse(data$age2==08,1,0)
data$age2_09 <- ifelse(data$age2==09,1,0)
data$age2_10 <- ifelse(data$age2==10,1,0)
data$age2_11 <- ifelse(data$age2==11,1,0)
data$age2_12 <- ifelse(data$age2==12,1,0)
data$age2_13 <- ifelse(data$age2==13,1,0)
data$age2_14 <- ifelse(data$age2==14,1,0)
data$age2_15 <- ifelse(data$age2==15,1,0)
data$age2_16 <- ifelse(data$age2==16,1,0)
data$age2_17 <- ifelse(data$age2==17,1,0)
data$age2_18 <- ifelse(data$age2==18,1,0)
data$age2_19 <- ifelse(data$age2==19,1,0)
data$age2_20 <- ifelse(data$age2==20,1,0)
data$age2_98 <- ifelse(data$age2==98,1,0)
# Create Collapsed Variabes: AGE2
age2_01 <- summaryBy(age2_01 ~ geo2a_mw, FUN=mean, data=data)
age2_02 <- summaryBy(age2_02 ~ geo2a_mw, FUN=mean, data=data)
age2_03 <- summaryBy(age2_03 ~ geo2a_mw, FUN=mean, data=data)
age2_04 <- summaryBy(age2_04 ~ geo2a_mw, FUN=mean, data=data)
age2_05 <- summaryBy(age2_05 ~ geo2a_mw, FUN=mean, data=data)
age2_06 <- summaryBy(age2_06 ~ geo2a_mw, FUN=mean, data=data)
age2_07 <- summaryBy(age2_07 ~ geo2a_mw, FUN=mean, data=data)
age2_08 <- summaryBy(age2_08 ~ geo2a_mw, FUN=mean, data=data)
age2_09 <- summaryBy(age2_09 ~ geo2a_mw, FUN=mean, data=data)
age2_10 <- summaryBy(age2_10 ~ geo2a_mw, FUN=mean, data=data)
age2_11 <- summaryBy(age2_11 ~ geo2a_mw, FUN=mean, data=data)
age2_12 <- summaryBy(age2_12 ~ geo2a_mw, FUN=mean, data=data)
age2_13 <- summaryBy(age2_13 ~ geo2a_mw, FUN=mean, data=data)
age2_14 <- summaryBy(age2_14 ~ geo2a_mw, FUN=mean, data=data)
age2_15 <- summaryBy(age2_15 ~ geo2a_mw, FUN=mean, data=data)
age2_16 <- summaryBy(age2_16 ~ geo2a_mw, FUN=mean, data=data)
age2_17 <- summaryBy(age2_17 ~ geo2a_mw, FUN=mean, data=data)
age2_18 <- summaryBy(age2_18 ~ geo2a_mw, FUN=mean, data=data)
age2_19 <- summaryBy(age2_19 ~ geo2a_mw, FUN=mean, data=data)
age2_20 <- summaryBy(age2_20 ~ geo2a_mw, FUN=mean, data=data)
age2_98 <- summaryBy(age2_98 ~ geo2a_mw, FUN=mean, data=data)
# Merge: AGE2
age2 <- merge(age2_01, age2_02, by="geo2a_mw")
age2 <- merge(age2, age2_03, by="geo2a_mw")
age2 <- merge(age2, age2_04, by="geo2a_mw")
age2 <- merge(age2, age2_05, by="geo2a_mw")
age2 <- merge(age2, age2_06, by="geo2a_mw")
age2 <- merge(age2, age2_07, by="geo2a_mw")
age2 <- merge(age2, age2_08, by="geo2a_mw")
age2 <- merge(age2, age2_09, by="geo2a_mw")
age2 <- merge(age2, age2_10, by="geo2a_mw")
age2 <- merge(age2, age2_11, by="geo2a_mw")
age2 <- merge(age2, age2_12, by="geo2a_mw")
age2 <- merge(age2, age2_13, by="geo2a_mw")
age2 <- merge(age2, age2_14, by="geo2a_mw")
age2 <- merge(age2, age2_15, by="geo2a_mw")
age2 <- merge(age2, age2_16, by="geo2a_mw")
age2 <- merge(age2, age2_17, by="geo2a_mw")
age2 <- merge(age2, age2_18, by="geo2a_mw")
age2 <- merge(age2, age2_19, by="geo2a_mw")
age2 <- merge(age2, age2_20, by="geo2a_mw")
age2 <- merge(age2, age2_98, by="geo2a_mw")




#####################################################
# Merge All Person Level Variables
mw_2008_p <- merge(resident, XXXXX, by="geo2a_mw")
mw_2008_p <- merge(mw_2008_p, XXXXX, by="geo2a_mw")