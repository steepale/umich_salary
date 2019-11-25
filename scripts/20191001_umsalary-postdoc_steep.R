#'---
#' title: "Examination of Postdoc Salatries at the University of Michigan"
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

#' ## Load Data
#' ##### Data Files to Load:
#' * UM salary information
#'
#+ Load the Data

################################################################################
#####     Load Data      #######################################################
################################################################################

# Load in the sample annotation table
df <- read.table(file = './data/UMsalary.txt', header = TRUE, sep = '\t')
df <- as_tibble(df)

# Remove commas from income
df$APPT_ANNUAL_FTR <- str_replace(df$APPT_ANNUAL_FTR, ',', '') %>% as.numeric()
df <- df[!is.na(df$APPT_ANNUAL_FTR),]

# Calculate annual income
df$APPT_ANNUAL_INCOME <- df$APPT_ANNUAL_FTR / df$APPT_FTR_BASIS * 12

# How to assign unique identifiers? -- not neccessarily 100% accurate but good enough (Pareto distribution & efficiency)
# https://stackoverflow.com/questions/10118856/how-to-assign-a-unique-identifier-to-multiple-data-frame-entries
df$ID <- as.factor(as.numeric(as.factor(with(df, paste(CAMPUS, NAME, TITLE, sep="_")))))
names(df)
# Check
rev(sort(table(df$ID)))

# Annotate employee year (only if not working in 2015 -- interested in 1st year postdocs)

# Create seperate data frame
# Check distinct value frequency based on column
names(df)
df %>% dplyr::select(NAME) %>% distinct() %>% dim()
df %>% dplyr::select(ID) %>% distinct() %>% dim()
df %>% distinct() %>% dim()

df_id <- as.data.frame(table(df$ID))
names(df_id) = c("ID", "YRS_POST_2015")
str(df_id)
# perform left join
df <- left_join(df, df_id, by = "ID")

# Approximetely how many RESEARCH_FELLOWS? AKA mostly postdocs: 2,749 (maybe 2,700 postdocs)
df %>% filter(TITLE == 'RESEARCH_FELLOW') %>% distinct(ID) %>% dim()

rev(sort(table(df$TITLE)))

################################################################################
############ Determine Income for 1st Year Postdocs ############################
################################################################################

# Create a dataframe of first year postdocs (from 2016 - 2018)
df_pd <- filter(df, TITLE == 'RESEARCH_FELLOW' & YRS_POST_2015 == 1 & YEAR != 2015)

# Refactor some columns
df_pd$DEPT <- as.character(df_pd$DEPT)
df_pd$DEPT <- as.factor(df_pd$DEPT)

# Count the number of first year postdocs over a 3 year period: 915
df_pd %>% dplyr::select(ID) %>% distinct() %>% dim()
dim(df_pd)
# -- No redundant names

# Postdoc Admittance frequency per year
names(df_pd)
table(df_pd$YEAR)
# 2016 2017 2018 
# 214  210  491 

# Divide by 3 and consider average (305) and predicted value (human prediction 200-600)

################################################################################
####### Build a DataSet for the Number of Internal Training Grants #############
################################################################################

# File name: 20191014_training-grants_steep.xlsx (file needs to be cleaned)
tg <- read.table(file = './data/2019_training_grants.txt', header = TRUE, sep = '\t')
tg <- as_tibble(tg)

# Stats: 139 opportunities
# Probability of winning award if everyone applied to one award: 0.3475

PW <- 139/400
PL <- 1-PW
# In reality, the probability is dependant on the number of applicants and where motivation to fund applicant lies above threshold.

# How many positions would you need to apply to to ensure at least 1 win 95% of the time?
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
# Run the simulation
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

str(dfplot)
#pdf('./figures/apps-by-prob.pdf', width = 6, height = 6)
ggplot(dfplot, aes(y= PROB_WIN, x = APPS)) +
        geom_point(alpha = 0.6) +
        ylab("Probability of winning at least one fellowship") +
        ylim(0,1.0) +
        scale_x_discrete(name ="Number of fellowships applied", 
                         limits=1:10)
#dev.off()

# Analysis inspired by: https://www.youtube.com/watch?v=GGXzlRoNtHU

# Number of fellowships to apply to to have optimum choice (37% rule suggests you should win at least 3)
NOP <- NW*3

# Conclusion: Each of Tom's Postdocs should consider applying to between 3 and 9 opportunities.

# Choosing optimum opportunities:
tg %>% filter(RESEARCH_SIMILAR >= 2)




################################################################################
############ 1st Year Postdocs in Bioinformatics ###############################
################################################################################

# Collect first year postdocs in bioinformatics (vectors and data.frames)
vt_bi_first <- filter(as.data.frame(df_pd), DEPT == 'Comp_Med_and_Bioinformatics') %>% dplyr::select(APPT_ANNUAL_INCOME) %>% unlist()
df_bi_first <- filter(as.data.frame(df_pd), DEPT == 'Comp_Med_and_Bioinformatics')

# Collect first year postdocs (consider removing outliers) across departments
vt_all_first <- as.data.frame(df_pd) %>% dplyr::select(APPT_ANNUAL_INCOME) %>% unlist() 
df_all_first <- as.data.frame(df_pd)

# Visualize income distribution with histogram
maze = 'yellow'
blue = 'blue'


#Classic Histogram
hist_bi <- ggplot(data=df_bi_first, aes(df_bi_first$APPT_ANNUAL_INCOME)) +
        geom_histogram(aes(),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        ggtitle("Histogram of First-Year Bioinformatics Postdocs at UM") + 
        xlab("Annual Income (USD)")
# Density Histogram
dens_bi <- ggplot(data=df_bi_first, aes(df_bi_first$APPT_ANNUAL_INCOME)) +
        geom_histogram(aes(y=..density..),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        geom_density(col=2) +
        ggtitle("Histogram of First-Year Bioinformatics Postdocs at UM") + 
        xlab("Annual Income (USD)")
# Arrange 2 plots side-by-side
ggarrange(hist_bi, dens_bi, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


# Determine the percentile of job offer
quantile(bi_first, c(0.00, 0.32, 0.57, 0.98)) 

################################################################################
###### Determine Income for 1st Year Assistant Professors (Tenure-Track) #######
################################################################################

# Create a dataframe of first year postdocs
df_ap <- filter(df, TITLE == 'ASST_PROFESSOR' & YRS_POST_2015 == 1 & YEAR != 2015)

# Refactor some columns
df_ap$DEPT <- as.character(df_ap$DEPT)
df_ap$DEPT <- as.factor(df_ap$DEPT)

# Collect first year assistant professors in bioinformatics (vectors and data.frames)
vt_bi_first_ap <- filter(as.data.frame(df_ap), DEPT == 'Comp_Med_and_Bioinformatics') %>% dplyr::select(APPT_ANNUAL_INCOME) %>% unlist()
df_bi_first_ap <- filter(as.data.frame(df_ap), DEPT == 'Comp_Med_and_Bioinformatics')

# Collect first year assistant professors (consider removing outliers) across departments
vt_all_first_ap <- as.data.frame(df_ap) %>% dplyr::select(APPT_ANNUAL_INCOME) %>% unlist() 
df_all_first_ap <- as.data.frame(df_ap)

# Summary stats
summary(df_all_first_ap$APPT_ANNUAL_INCOME)

# Visualize income distribution with histogram
maze = 'yellow'
blue = 'blue'

#Classic Histogram
hist_all_ap <- ggplot(data=df_all_first_ap, aes(df_all_first_ap$APPT_ANNUAL_INCOME)) +
        geom_histogram(aes(),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        ggtitle("Histogram of First-Year Bioinformatics Assistant Profs at UM") + 
        xlab("Annual Income (USD)")
# Density Histogram
dens_all_ap <- ggplot(data=df_all_first_ap, aes(df_all_first_ap$APPT_ANNUAL_INCOME)) +
        geom_histogram(aes(y=..density..),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        geom_density(col=2) +
        ggtitle("Histogram of First-Year Bioinformatics Postdocs at UM") + 
        xlab("Annual Income (USD)")
# Arrange 2 plots side-by-side
ggarrange(hist_all_ap, dens_all_ap, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

################################################################################
########## Determine Distribution of Income in Kretlzer Lab ####################
################################################################################

# Create a vector of Kretzler lab income
kretz_peepz <- c("Kretzler,Matthias","Williams,Amanda","Reamy,Rebecca","Steck,Rebecca","Godfrey,Bradley","Gizinski,Brandi_K","Berthier,Celine_Chantal","Lienczewski,Chrysta_Caryl","Otto,Edgar_Alfred","Martinez,Emily","Eichinger,Felix_Hans_Kamillo","Hamidi,Habib","Ascani,Heather","Saha,Jharna","Hartman,John","Mariani,Laura_Heyns","Hunter,Lois_R","Larkina,Marina","Tomilo,Mark","Rose,Michael","Wys,Noel_Lisa","Dull,Rachel","Eddy,Sean","Roth,Therese","Johnson,Tiffany","Mainieri,Tina","Nair,Viji","Ju,Wenjun","Wang,Yuee","Wright,Zachary_Charles")

# Filter the members from the Kretlzer lab
df_kretz <- filter(df, NAME %in% kretz_peepz)

#Classic Histogram
hist_kretz_all <- ggplot(data=df_kretz, aes(df_kretz$APPT_ANNUAL_INCOME)) +
        geom_histogram(aes(),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        ggtitle("Histogram of Income of Kretlzer Lab Employees") + 
        xlab("Annual Income (USD)")
# Density Histogram
dens_kretz_all <- ggplot(data=df_kretz, aes(df_kretz$APPT_ANNUAL_INCOME)) +
        geom_histogram(aes(y=..density..),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        geom_density(col=2) +
        ggtitle("Histogram of First-Year Bioinformatics Postdocs at UM") + 
        xlab("Annual Income (USD)")

# Plot the Kretler lab income
ggarrange(hist_kretz_all, dens_kretz_all, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

