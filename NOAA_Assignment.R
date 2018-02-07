## JHU - Reproducible Research - Week 4 - Course Project 2

## Read the data and load the file into a data set.

noaaData <- read.csv("repdata-data-StormData.csv.bz2", header = TRUE, sep = ",")

## Take a look at the data set's head, tail and structure.

head(noaaData)
tail(noaaData)

str(noaaData)

## "1. Across the United States, which types of events (as indicated in the EVTYPE variable)
## are most harmful with respect to population health?"

## Take a look at the EVTYPE variable.

head(noaaData$EVTYPE)
unique(noaaData$EVTYPE)

## We see 985 levels of different types of events, spread out within almost a million observations,
## or 902297 to be precise.

## Look at the types of variables available (with the names() function) and the provided
## documentation, to check which variable/s corresponds to "population health."

names(noaaData)

## The variables that seem to correspond to population's health are "FATALITIES" and "INJURIES".

## Subset the three variables - "EVTYPE", "FATALITIES", "INJURIES".

healthData <- subset(noaaData, select = c(EVTYPE, FATALITIES, INJURIES))

## Calculate the mean values of "FATALITIES" and "INJURIES" for every "EVTYPE" factor.

install.packages("dplyr")
library(dplyr)

healthDataMeans <- healthData %>% group_by(EVTYPE) %>% summarise_all(funs(mean))

str(healthDataMeans)

## We need integer numbers, i.e. whole numbers, in the "FATALITIES" and "INJURIES" variables,
## in order to better read and understand the results, i.e. to count the number of fatalities
## and injuries per event type.

healthDataMeans$FATALITIES <- as.integer(healthDataMeans$FATALITIES)
healthDataMeans$INJURIES <- as.integer(healthDataMeans$INJURIES)

str(healthDataMeans)

## Since there are 985 observations and many "EVTYPE", or types of events, with an average
## value of "FATALITIES" and "INJURIES" equal to 0, the data set can be optimized by
## cleaning these values. The 0 values can be replaced with NA, in order to use the argument
## na.rm = TRUE/FALSE later when plotting and using other functions.

healthDataMeansNa <- healthDataMeans

healthDataMeansNa[healthDataMeansNa == 0] <- NA

## Checking the summary of the data. It can give us idea how it is distributed and what values
## are critical to answering the main question.

summary(healthDataMeansNa$FATALITIES)
summary(healthDataMeansNa$INJURIES)

## Going furhter, the following code subsets the data set only with the type of event and the
## corresponding number of fatalities. In addition, it cleans the rows with NA values.

maxFatal <- subset(healthDataMeansNa, select = c(EVTYPE, FATALITIES))
maxFatalNoNa <- maxFatal[complete.cases(maxFatal), ]

## Since the 3rd quantile of the data in the "FATALITIES" variable is 2, that number of fatalities
## can be the standard for "most harmful", as asked in the main question. There can be one more data
## set showing only the "most harmful" event types and corresponding number of fatalities, i.e. more
## than 2.

maxFatalFinal <- maxFatalNoNa[maxFatalNoNa$FATALITIES > 2, ]

## Going furhter, the following code subsets the data set only with the type of event and the
## corresponding number of injuries. In addition, it cleans the rows with NA values.

maxInj <- subset(healthDataMeansNa, select = c(EVTYPE, INJURIES))
maxInjNoNa <- maxInj[complete.cases(maxInj), ]

## Since the 3rd quantile of the data in the "INJURIES" variable is 5.25, that number of injuries
## can be the standard for "most harmful", as asked in the main question. There can be one more data
## set showing only the "most harmful" event types and corresponding number of injuries, i.e. more
## than 5.25, or more than 6 (as a whole number).

maxInjFinal <- maxInjNoNa[maxInjNoNa$INJURIES > 6, ]

## Finally, the two-panel plot shows the graphical representation of the "most harmful" event types.

install.packages("ggplot2")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

plot1 <- ggplot(maxInjFinal, aes(EVTYPE, INJURIES)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Average Injuries per Event Type",
             x = "Type of Event",
             y = "Number of Injuries")
plot2 <- ggplot(maxFatalFinal, aes(EVTYPE, FATALITIES)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Avage Fatalities per Event Type",
             x = "Type of Event",
             y = "Number of Fatalities")
grid.arrange(plot1, plot2, ncol = 2)

## The plot above lists the "most harmful" types of events with respect to population health.

## "2. Across the United States, which types of events have the greatest economic consequences?"

## Look at the types of variables available (with the names() function) and the provided
## documentation, to check which variable/s have direct "economic consequences."

## The variables that seem to correspond to economic consequences are 

names(noaaData)

head(noaaData)
tail(noaaData)

## As the provided information shows, the variables that have direct economic impact, in terms
## of monetary consequences, or recorded losses, are "PROPDMG" and "PROPDMGEXP", as well as
## "CROPDMG" and "CROPDMGEXP". The two corresponding pairs combine in itself the amount of money
## that the damage caused. The fisrst variable of each pair represents the number and the second
## shows K or M, or whether the number in the first variable is in thousands or millions.

## The following two steps subset the variables mentioned above - one for cost of damage for
## properties and one for cost of damage for crop, along with the types of events that caused them.

propDmgExp <- subset(noaaData, select = c(EVTYPE, PROPDMG, PROPDMGEXP))
cropDmgExp <- subset(noaaData, select = c(EVTYPE, CROPDMG, CROPDMGEXP))

## Take a look at the structure and the unique values within the two data sets.

str(propDmgExp)
str(cropDmgExp)

unique(propDmgExp$PROPDMGEXP)
unique(cropDmgExp$CROPDMGEXP)

## In each of the two data sets (the one for property damage and the one for crop damage),
## the variable that shows whether the number is in hundreds, thousands, millions, or billions
## has factors that don't give the required information. In addition, there are many 0 monetary
## values, and a corresponding "" (or blank) value as its type of expense.

## Since the needed values are only the ones in hundreds, thousands, millions, or billions,
## the following code converts the expense type variable from class factor to class character
## and extracts the repsective characters for hundreds (h, H), thousands (k, K), millions (m, M),
## or billions (B).

propDmgExp$PROPDMGEXP <- as.character(propDmgExp$PROPDMGEXP)
str(propDmgExp)
unique(propDmgExp$PROPDMGEXP)

propDmgExp2 <- propDmgExp[propDmgExp$PROPDMGEXP %in% c("B", "h", "H", "K", "m", "M"), ]
str(propDmgExp2)
unique(propDmgExp2$PROPDMGEXP)

cropDmgExp$CROPDMGEXP <- as.character(cropDmgExp$CROPDMGEXP)
str(cropDmgExp)
unique(cropDmgExp$CROPDMGEXP)

cropDmgExp2 <- cropDmgExp[cropDmgExp$CROPDMGEXP %in% c("M", "K", "m", "B", "k"), ]
str(cropDmgExp2)
unique(cropDmgExp2$CROPDMGEXP)

## Once the needed data is isolated, the two variables, the one showing the monetary number
## and the one showing the number of zeros, need to be merged in order to preceed further
## with the exploratory analysis.

## First, the letters in the variables showing hundreds, thousands, millions, and billions are
## substitutted with the corresponding number (e.g. 100, 1000, etc.), so they can be ready for
## multiplication later on.

cropDmgExp3 <- cropDmgExp2
unique(cropDmgExp3$CROPDMGEXP)

cropDmgExp3$CROPDMGEXP <- ifelse(cropDmgExp3$CROPDMGEXP == "M", 1000000,
                                 ifelse(cropDmgExp3$CROPDMGEXP == "m", 1000000,
                                        ifelse(cropDmgExp3$CROPDMGEXP == "K", 1000,
                                               ifelse(cropDmgExp3$CROPDMGEXP == "k", 1000,
                                                      ifelse(cropDmgExp3$CROPDMGEXP == "B", 1000000000, 0)))))

str(cropDmgExp3)

propDmgExp3 <- propDmgExp2
unique(propDmgExp3$PROPDMGEXP)

propDmgExp3$PROPDMGEXP <- ifelse(propDmgExp3$PROPDMGEXP == "K", 1000,
                                 ifelse(propDmgExp3$PROPDMGEXP == "M", 1000000,
                                        ifelse(propDmgExp3$PROPDMGEXP == "B", 1000000000,
                                               ifelse(propDmgExp3$PROPDMGEXP == "m", 1000000,
                                                      ifelse(propDmgExp3$PROPDMGEXP == "h", 100,
                                                             ifelse(propDmgExp3$PROPDMGEXP == "H", 100, 0))))))

str(propDmgExp3)

## After the replacement, the two columns (in both data sets), representing the monetary value
## and the number of zeros, are multiplied and displayed in a new variable called "COST".

cropDmgExp3$COST <- with(cropDmgExp3, CROPDMG * CROPDMGEXP)
propDmgExp3$COST <- with(propDmgExp3, PROPDMG * PROPDMGEXP)

## Once the cost for damage on crop and properties is calculated, it's time to calculate the
## total cost for each event type, in both data sets, and place the information in new data sets
## with only two variables "EVTYPE" and "COST", for crop and properties repspectively.

cropDmgCost <- subset(cropDmgExp3, select = c(EVTYPE, COST))
propDmgCost <- subset(propDmgExp3, select = c(EVTYPE, COST))

install.packages("dplyr")
library(dplyr)

cropDmgCost2 <- cropDmgCost %>% group_by(EVTYPE) %>% summarise_all(funs(sum))
propDmgCost2 <- propDmgCost %>% group_by(EVTYPE) %>% summarise_all(funs(sum))

## Taka e look at the summary of the "COST" variable, before plotting the data from the two data sets,
## for crop and properties, in order to get familiar with the high and low values. That will give
## an idea which types of events are the most damaging to crop and properties, as per the
## main question - "which types of events have the greatest economic consequences?"

summary(cropDmgCost2$COST)
summary(propDmgCost2$COST)

cropDmgCostFinal <- cropDmgCost2[cropDmgCost2$COST >= 2.721e+07, ]
propDmgCostFinal <- propDmgCost2[propDmgCost2$COST >= 5.000e+06, ]

str(cropDmgCostFinal)
str(propDmgCostFinal)

## After subsetting the third quartile, there are still 40 and 102 observations, respectively
## in the crop and properties damage cost data sets. Since the main question is which event
## types have the greatest economic impact, the final subset, thus the final graph will show
## the top ten high-cost types of events, in terms of crop and properties.

## The following code creates the final data sets, for both crop and properties, which include
## top ten high-cost types of events.

cropDmgCostFinalCut <- cropDmgCostFinal %>% arrange(desc(COST)) %>% slice(1:10)
propDmgCostFinalCut <- propDmgCostFinal %>% arrange(desc(COST)) %>% slice(1:10)

## The following code plots the final two data sets, with top ten high-cost types of events.

plot3 <- ggplot(cropDmgCostFinalCut, aes(EVTYPE, COST)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Total Cost of Damage for Crop",
             x = "Type of Event",
             y = "Cost of Damage")
plot4 <- ggplot(propDmgCostFinalCut, aes(EVTYPE, COST)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Total Cost of Damage for Properties",
             x = "Type of Event",
             y = "Cost of Damage")
grid.arrange(plot3, plot4, ncol = 2)

## The plot above lists the top ten high-cost types of events with respect to economic
## consequences, in terms of damaged crop and properties.
