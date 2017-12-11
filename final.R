# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.


# read file and do some basic setup
MyData <- read.csv("states (1).csv", stringsAsFactors = FALSE)

colnames(MyData) <- c("State", 
                   "Uninsured_Rate_2010", 
                   "Uninsured_Rate_2015",
                   "Uninsured_Rate_Ch_2010_2015", 
                   "Health_Ins_Cov_Ch_2010_2015",
                   "Empl_Ins_Cov_Ch_2010_2015", 
                   "Mktpl_Ins_Cov_Ch_2010_2015",
                   "Mktpl_Tx_Credits", 
                   "Avg_Mo_Tx_Credit", 
                   "State_Medicaid_Exp", 
                   "Medicaid_Enroll_2013", 
                   "Medicaid_Enroll_2016",
                   "Medicaid_Enroll_Ch_2013_2016", 
                   "Medicare_Enroll_2016")




# convert character percentage rates to numeric
MyData$Uninsured_Rate_2010 <- as.numeric(sub("%", "", MyData$Uninsured_Rate_2010))/100
MyData$Uninsured_Rate_2015 <- as.numeric(sub("%", "", MyData$Uninsured_Rate_2015))/100
MyData$Uninsured_Rate_Ch_2010_2015 <- as.numeric(sub("%", "", MyData$Uninsured_Rate_Ch_2010_2015))/100

# convert $ to numeric
MyData$Avg_Mo_Tx_Credit <- as.numeric(gsub("[^0-9]", "", MyData$Avg_Mo_Tx_Credit))

# convert state to a factor
MyData$State <- as.factor(MyData$State)

# the value for the US in Uninsured_Rate_Ch_2010_2015 is incorrect.  we'll fix it
MyData$Uninsured_Rate_Ch_2010_2015 <- MyData$Uninsured_Rate_2015 - MyData$Uninsured_Rate_2010

# create a data frame with just the US summary info and a set with just the states
MyData_state <- MyData[!MyData$State == "United States", ]
MyData_US <- MyData[MyData$State == "United States", ]

# drop unused factor levels (not really necessary, but I prefer it)
MyData_state$State <- factor(MyData_state$State)
MyData_US$State <- factor(MyData_US$State)


# Summarize US Stats (just print it)
MyData_US


# create a new State vector thats ordered by Unisured Rate Change for better plotting
library(ggplot2)
MyData_state$State2 <- factor(MyData_state$State, levels =MyData_state[order(MyData_state$Uninsured_Rate_Ch_2010_2015), "State"])
ggplot(MyData_state, aes(State2, Uninsured_Rate_Ch_2010_2015)) + 
  geom_bar(stat="identity", fill = "firebrick") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  ggtitle("Uninsured Rate Change 2010 - 2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Uninsured Rate Change 2010-2015") + 
  geom_hline(yintercept = MyData_US$Uninsured_Rate_Ch_2010_2015, color = "blue", linetype = "dotdash") # add national average



# medicaid expansion

# 2 missing values
sum(is.na(MyData_state$Medicaid_Enroll_2013)) 
aca_state2 <- MyData_state[complete.cases(MyData_state), ] # we'll just eliminate them for this exercise

# plot
aca_state2$State2 <- factor(aca_state2$State, levels =aca_state2[order(aca_state2$Medicaid_Enroll_Ch_2013_2016), "State"])
ggplot(aca_state2, aes(State2, Medicaid_Enroll_Ch_2013_2016, fill=State_Medicaid_Exp)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Medicaid Enrollment Change 2013-2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Medicaid Enrollment Change 2013-2016") +
  scale_fill_discrete(name="Medicaid\nExpansion") +
  geom_hline(yintercept = mean(aca_state2$Medicaid_Enroll_Ch_2013_2016), color = "blue", linetype = "dotdash") # add national average



#"firebrick", color="black"

# Look at Health Insurance Coverage Change 
# this would be more useful on a per capita basis
MyData_state$State2 <- factor(MyData_state$State, levels =MyData_state[order(MyData_state$Health_Ins_Cov_Ch_2010_2015), "State"])
ggplot(MyData_state, aes(State2, Health_Ins_Cov_Ch_2010_2015)) + 
  geom_bar(stat="identity", fill = "firebrick") +
  coord_flip() +
  ggtitle("Health Insurance Change 2013-2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Health Insurance Change 2013-2015") + 
  geom_hline(yintercept = mean(MyData_state$Health_Ins_Cov_Ch_2010_2015), color = "blue", linetype = "dotdash") # add national average


# Marketplace Insurance Coverage Change 
# this would be more useful on a per capita basis
MyData_state$State2 <- factor(MyData_state$State, levels =MyData_state[order(MyData_state$Mktpl_Ins_Cov_Ch_2010_2015), "State"])
ggplot(MyData_state, aes(State2, Mktpl_Ins_Cov_Ch_2010_2015)) + 
  geom_bar(stat="identity", fill = "firebrick") +
  coord_flip() +
  ggtitle("Marketplace Health Insurance Change 2013-2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Marketplace Health Insurance Change 2013-2015") + 
  geom_hline(yintercept = mean(MyData_state$Mktpl_Ins_Cov_Ch_2010_2015), color = "blue", linetype = "dotdash") # add national average




# Avg Monthly Tax Credit
# this would be more useful on a per capita basis
MyData_state$State2 <- factor(MyData_state$State, levels =MyData_state[order(MyData_state$Avg_Mo_Tx_Credit), "State"])
ggplot(MyData_state, aes(State2, Avg_Mo_Tx_Credit)) + 
  geom_bar(stat="identity", fill = "red") +
  coord_flip() +
  ggtitle("Avg Monthly Tax Credit") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Avg Monthly Tax Credit") + 
  geom_hline(yintercept = mean(MyData_state$Avg_Mo_Tx_Credit), color = "blue", linetype = "dotdash") # add national average




  