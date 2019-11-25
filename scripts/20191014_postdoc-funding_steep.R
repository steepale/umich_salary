#'---
#' title: "Examination of Postdoc Funding Opportunities at the University of Michigan"
#' author: Alec Steep
#' date: "`r format.Date( Sys.Date(), '%Y%m%d' )`"
#' output: 
#'     html_document:
#'         code_folding: hide
#'         toc: true
#'         highlight: zenburn
#'---

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(cache = FALSE)

#' ## Explanation of Data 
#' Annual salary data (PDF format) was obtained from UM's hr website: https://hr.umich.edu/working-u-m/management-administration/hr-reports-data-services/hr-data-requests-standard-reports. The data was extracted from pdf format and cleaned for analysis within R.
#' 
#' [Github Repo](https://github.com/steepale/umich_salary)
#' 
#' 
#' Potential postdoc funding opportunities was generously provided by Aaron Reifler
#' 
#' ## Setup the Environment

#+ Setup Environment

################################################################################
##### Resources and Dependencies ###############################################
################################################################################

# Set the working directory
setwd('/Users/Alec/Documents/umich_salary')

# Load the dependencies
#source("https://bioconductor.org/biocLite.R")
#BiocManager::install("R.utils")
#install.packages("ggpubr")

# Load dependencies
pacs...man <- c("dplyr","tibble","magrittr","stringr", "data.table","readr","R.utils","ggplot2","ggpubr")
lapply(pacs...man, FUN = function(X) {
        do.call("library", list(X)) 
})

############################################################
##### Functions ############################################
############################################################

# Make the 'not in' operator
'%!in%' <- function(x,y) {
        !('%in%'(x,y))
}

# Capture the Date
date <- format.Date( Sys.Date(), '%Y%m%d' )
auth <- "steep"

################################################################################

#' ## Load & Clean Data
#' ##### Data Files to Load:
#' * UM Salary Information
#' * Postdoc Funding Opportunities
#'
#+ Load the Data

################################################################################
#####     Load & Clean Data      ###############################################
################################################################################

# Load the postdoc funding information
tg <- read.table(file = '/Users/Alec/Documents/umich_salary/data/2019_training_grants.txt', header = TRUE, sep = '\t')
tg <- as_tibble(tg)

# Load in the salary data for all of UM (sorry y'all)
df <- read.table(file = "/Users/Alec/Documents/umich_salary/data/UMsalary.txt", header = TRUE, sep = '\t')
df <- as_tibble(df)

# Remove commas from income
df$APPT_ANNUAL_FTR <- str_replace(df$APPT_ANNUAL_FTR, ',', '') %>% as.numeric()
df <- df[!is.na(df$APPT_ANNUAL_FTR),]

#' ## Add Features
#' ##### Added Features:
#' * Annual Income
#' * Employee Identifiers
#'
#+ Add Features

################################################################################
#####     Add Features      ####################################################
################################################################################

# Calculate annual income
df$APPT_ANNUAL_INCOME <- df$APPT_ANNUAL_FTR / df$APPT_FTR_BASIS * 12

# How to assign unique identifiers? -- not neccessarily 100% accurate but good enough (Pareto distribution & efficiency)
# https://stackoverflow.com/questions/10118856/how-to-assign-a-unique-identifier-to-multiple-data-frame-entries
df$ID <- as.factor(as.numeric(as.factor(with(df, paste(CAMPUS, NAME, TITLE, sep="_")))))
#names(df)
# Sanity check
#rev(sort(table(df$ID)))

# Annotate employee year (only if not working in 2015 -- interested in 1st year postdocs)

# Create seperate data frame
# Check distinct value frequency based on column
#df %>% dplyr::select(NAME) %>% distinct() %>% dim()
#df %>% dplyr::select(ID) %>% distinct() %>% dim()
#df %>% distinct() %>% dim()

df_id <- as.data.frame(table(df$ID))
names(df_id) = c("ID", "YRS_POST_2015")
#str(df_id)
# perform left join
df <- left_join(df, df_id, by = "ID")

# Approximetely how many RESEARCH_FELLOWS? AKA mostly postdocs: 2,749 (maybe 2,700 postdocs)
#df %>% filter(TITLE == 'RESEARCH_FELLOW') %>% distinct(ID) %>% dim()

#' ## Visualize the Data
#' * Dimensions
#' * Structure
#' * First Rows
#' 
#+ Vis Data

################################################################################
#####     Visualize Data      ##################################################
################################################################################

#' #### UM Salary Information:
dim(df)
str(df)
head(df)
#' #### Postdoc Funding Opportunities:
dim(tg)
str(tg)
head(tg)

#' ## Determine the Frequency of First Year Postdocs
#' First year postdocs included in years 2016-2019 (no way to determine 2015)
#' 
#' 
#+ FYPD Stats

################################################################################
############ Determine Income for 1st Year Postdocs ############################
################################################################################

# Create a dataframe of first year postdocs (from 2016 - 2018)
df_pd <- filter(df, TITLE == 'RESEARCH_FELLOW' & YRS_POST_2015 == 1 & YEAR != 2015)

# Refactor some columns
df_pd$DEPT <- as.character(df_pd$DEPT)
df_pd$DEPT <- as.factor(df_pd$DEPT)

# Count the number of first year postdocs over a 3 year period: 915
# df_pd %>% dplyr::select(ID) %>% distinct() %>% dim()
# dim(df_pd)
# -- No redundant names

#' #### The Frequency of First Year Post Docs in the Past 3 Years

# Postdoc Admittance frequency per year
# names(df_pd)
table(df_pd$YEAR)
# 2016 2017 2018 
# 214  210  491 

# Divide by 3 and consider average (305) and predicted value (human prediction 200-600)

#' It's difficult to predict the number of postdocs this year (let alone the number looking for funding). I'll take a conservative approach and assume UM will admit 200-600 postdocs and use 400 in the model. A more realistic model will come after collecting data from funders.
#' 
#' ## Internal Training Grants Stats
#' 
#' 
#' 
#+ ITG Stats

################################################################################
####### Stas on Number of Internal Training Grants #############################
################################################################################

# Stats: 139 opportunities
# Probability of winning award if everyone applied to one award: 0.3475

# Determine the number of open positions
fund_opps <- sum(tg$OPEN_POSITIONS)
# Predicted number of 1st year postdocs in 2019 (human estimate)
fypd <- 400

# Prob win and loss
PW <- fund_opps/fypd
PL <- 1-PW

#' Number of open positions:
fund_opps
#' Number of FYPD:
fypd
#' Probability of winning:
PW
#' Don't say it:
!print(PL)

#' In reality, the probability is dependant on the number of applicants and where motivation to fund applicant lies for funders. Next model will be more sophisticated to capture predicted variance for opportunities applied to.
#' 
#' 
#' ## Model
#' #### How many positions would you need to apply to to ensure at least one fellowship with a 95% probability of success?
#' 
#' 
#+ Model

################################################################################
####### Simulation #############################################################
################################################################################

# Number of fellowships to apply to to have high liklihood of winning >=95%

# Not true: NW <- ceiling(0.95/PW) 

# Set the seed
set.seed(123)
# Trial numbers
M = 10000
# The number of amplications
AN = 10

x <- as.numeric()
y <- as.numeric()
# Run the sim
for (apps in 1:AN) {
        # Empty trials
        trials = c()
        # Perform an simulation in which a postdoc applies to n positions 100,000 times
        for (i in 1:M) {
                # Run the trial
                results <- sample(c('W','L'),apps,replace=TRUE,prob = c(PW,PL))
                if ('W' %in% results) {
                        WL <- TRUE
                }else{
                        WL <- FALSE
                }
                trials = c(trials, WL)
        }
        
        # Use table to visualize experiment
        NT <- as.numeric(table(trials)[2])
        NF <- as.numeric(table(trials)[1])
        N_TF <- NT + NF
        # Percentage of times the event has atleast 1 win
        PC <- NT/N_TF
        # Show results
        print(paste0('Number of applications: ',apps))
        print(paste0('Probability of getting at least one win: ',PC))
        x <- c(x,apps)
        y <- c(y, PC)
}
# Create a data frame of probability of winning by positions applied to (All we do is win win win)
dfplot <- data.frame(x,y)
names(dfplot) <- c('APPS','PROB_WIN')

#str(dfplot)
#pdf('./figures/apps-by-prob.pdf', width = 6, height = 6)
ggplot(dfplot, aes(y= PROB_WIN, x = APPS)) +
        geom_point(alpha = 0.6) +
        ylab("Probability of winning at least one fellowship") +
        ylim(0,1.0) +
        scale_x_discrete(name ="Number of fellowships applied", 
                         limits=1:10)
#dev.off()

#' ### Analysis Inspired by: [The Win](https://www.youtube.com/watch?v=GGXzlRoNtHU)




