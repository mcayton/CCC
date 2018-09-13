################################################
# Comparing traditional CPR to CCC
# Michael D. Cayton
# The College of Saint Scholastica
# 3/1/18
# Last updated 8/20/18
###############################################

## Load Libraries ###################################################
library(tidyverse)
library(haven)
library(readr)


## Functions ##########################################################
# Create mode function. Retrieved December 13, 2017 from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


###########################################################
## Create control group 'cpr' #############################
######################################################


## Import combined data set if you have it, this will save time and memory.
## If not, uncomment memory.limit, public_events, resuscitation, and full_join lines. 

cpr <- read_csv('cpr.csv')

## Increase R memory for large data set################################
# memory.limit(size=32000)

## Import data set from NEMSIS, table 'public_events'.  ################
# public_events <- read_sas("C:/Users/mdcay/Desktop/public_events.sas7bdat")


## Select only columns needed ##########################################
# public_events <- public_events %>% select(eventID, E11_01, E11_02, E20_10)

## Import data set, table 'resuscitation'; adds E11_03 from another NEMSIS table ##################
# resuscitation <- read_sas("C:/Users/mdcay/Desktop/resuscitation.sas7bdat")

##  Add resuscitation$E11_03 and public_events columns using JOIN ##### 
# cpr <- full_join(public_events, resuscitation, by="eventID")

## Select only columns needed ######################################
cpr <- cpr %>% select(E11_01, E11_03, E20_10)

## Convert to tbl class ################
cpr <- cpr %>% tbl_df()

cpr

## Step 1 ##
## Filter to contain only data with cardiac arrests: 'Yes, Prior to EMS Arrival' (2240)
## and 'Yes, After EMS Arrival' (2245) ################################################
cpr <- cpr %>% filter(E11_01 %in% c(2240, 2245))

## Step 2 ##
## Filter observations to contain only those where cpr was started ######
cpr <- cpr %>% filter(E11_03 == 2290)

cpr

## Save a copy of the combined data
# cpr %>% write.csv(file = 'cpr.csv')
# remove(public_events, resuscitation)

unique(cpr$E20_10)

## E20_10 has 'field values' that do not make sense if CPR was initiated (E11_03). 
## For example, if CPR was initiated a patient cannot refuse care (4835).  Others include:
## 'cancelled' (4815), 'no patient found' (4825), 'no treatement required' (4830), and  'treated and released' (4840).
cpr %>% group_by(E20_10) %>% count()


## Step 3 ##
## Filter observations to contain data were treatment was provided ###########
cpr <- cpr %>% filter(E20_10 %in% c(4820, 4845, 4850, 4855, 4860))


## Step 4 ##
## Create new attribute 'survived' and 
## assign 'yes' to patients that were transported and 'no' to those not transported ####### 
cpr <- cpr %>% mutate(survived=if_else(cpr$E20_10 == 4820, 'no', 'yes'))



######################################################################
######## CCC group ##################################################
#####################################################################
ccc <- read_csv("ccc.csv", 
                col_types = cols(`Incident Date` = col_datetime(format = "%m/%d/%Y %H:%M")))

ccc <- tbl_df(ccc)

ccc

colnames(ccc) <- c('E11_01','date','E20_10', 'E11_03')

##  Clean data to only include date: time is not an important factor in this analysis.
ccc$date <- as.Date(ccc$date)

## Filter observations to those after January 4, 2015 (when CCC was implemented) ###########
ccc <- filter(ccc, 'date' >= '2015-01-04')

ccc <- ccc %>% select('E11_01', 'E11_03', 'E20_10')

## Step 1 ##
## Filter for only observations that have cardiac arrest and cpr was started ###########
ccc <- filter(ccc, !is.na(E11_01))


## Step 2 ##
## Filter observations to contain only those where cpr was started ######
ccc <- filter(ccc, E11_03 != 'No')


## Step 3 ##
## Filter observations to remove NA's from E20_10 (NA means we don't know the outcome)
ccc <- filter(ccc, !is.na(E20_10))

## Convert characters to NEMSIS codes ####################
ccc[ccc$E11_01 == 'Yes, Prior to EMS Arrival', 'E11_01'] <- '2240'
ccc[ccc$E11_01 == 'Yes, After EMS Arrival', 'E11_01'] <- '2245'
ccc$E11_01 <- ccc$E11_01 %>% as.integer()
ccc[ccc$E11_03 == 'Yes', 'E11_03'] <- '2290'
ccc$E11_03 <- ccc$E11_03 %>% as.integer()
ccc[ccc$E20_10 == 'Dead on Scene, No Transport', 'E20_10'] <- '4820'
ccc[ccc$E20_10 == 'Treatment, No Transport', 'E20_10'] <- '4840'
ccc[ccc$E20_10 == 'Transported Lights/Siren', 'E20_10'] <- '4850'
ccc[ccc$E20_10 == 'Transported No Lights/Siren', 'E20_10'] <- '4850'
ccc$E20_10 <- ccc$E20_10 %>% as.integer

## Filter observations to contain data were treatment was provided for cardiac arrest###########
ccc <- ccc %>% filter(E20_10 %in% c(4820, 4845, 4850, 4855, 4860))

## Step 4 ##
## Create new attribute 'survived' and 
## assign 'yes' to patients that were transported and 'no' to those not transported ####### 
ccc <- ccc %>% mutate(survived=if_else(ccc$E20_10 == 4820, 'no', 'yes'))

################################################################
## Descriptive  statistics #######################################
################################################################
stats <- cpr %>%  summarise(name= 'cpr', mean=mean(survived=='yes'), sd = sd(survived=='yes'), mode = getmode(survived))
stats <- rbind(stats, ccc %>%  summarise(name = 'ccc', mean=mean(survived=='yes'), sd = sd(survived=='yes'), mode = getmode(survived)))
stats

## SD and mode have little meaning for categorical data and will not be reported.  

##############################################################
## Z-test #####################################################
###############################################################

#  National CPR survival rate is 0.74 (74%)
#  A local EMS agency uses CCC.  With 27 patients they have a survival rate of 0.44 (44%)

# Research Question ############################################
# Is the survival rate for CCC significantly different than the national standard of 74%?

# Hypotheses ##################################################
# Null Hypothesis: CCC survival rate is equal to the national average.  
# Alternative Hypothesis:  CCC is significantly different than the national average.
# Alpha level of 0.05

# Z-stat
attach(stats)

count(ccc)
ccc %>% group_by(survived) %>% summarise(n=n())
count(cpr)
cpr %>% group_by(survived) %>% summarise(n=n())
p <- mean[name=='ccc']
p0 <- mean[name=='cpr']
n <- nrow(ccc)
x <- sum(ccc$survived=='yes')

(p-p0) / sqrt(p0 * ((1-p0)/n))
# (0.44-0.74)/sqrt(0.74*((1-0.74)/27))

prop.test(x=x, n=n, p=p0, alternative="two.sided", correct=FALSE)


################################################################
## Plots ########################################################
################################################################
ggplot() +
  geom_bar(data = stats, aes(x=name, y=mean, fill=name), stat='identity') +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  geom_text(aes(2, .60, label='National')) +
  geom_text(aes(1, .30, label="LAFD")) +
  labs(title="Cardiac Survival Rates",
       x = "Type of CPR",
       y = "Survival Rate") +
  theme(legend.position='none'
        ,panel.grid.major.x=element_blank()
        ,panel.grid.minor.x=element_blank()
        ,panel.background = element_rect(fill='grey', color='black')
        ,plot.background = element_rect(fill='white')
        ,text = element_text(size=18)) 
