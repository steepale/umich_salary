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
#' ## UM First Year Postdoc Salary Distribution
#' 
#' 
#' 
#+ Postdoc-salary

################################################################################
####### 1st Year Postdoc Salary Distribution Across UM #########################
################################################################################

################################################################################
############ 1st Year Postdocs #################################################
################################################################################

# Collect first year postdocs (consider removing outliers) across departments

# 2018
#############################
# Collect first year postdoc salaries in 2018
fypd_2018 <- as.data.frame(df_pd) %>% filter(YEAR == "2018")

# NRSA: National Research Science Award
# Standard for 1st year PD 2018
NRSA_2018 <- 48432
# Standard for 1st year PD 2019
NRSA_2019 <- 50004
# Conversion ratio
NRSA_18v19 <- NRSA_2019/NRSA_2018

# Braodcast a new column of postdoc salaries adjusted for 2019
fypd_2018$NRSA_2019_INCOME <- fypd_2018$APPT_ANNUAL_INCOME * NRSA_18v19

# Visualize income distribution with histogram
maze = 'yellow'
blue = 'blue'

#Classic Histogram
hist_pd <- ggplot(data=fypd_2018, aes(fypd_2018$NRSA_2019_INCOME)) +
        geom_histogram(aes(),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        ggtitle("Histogram of First-Year Postdocs at UM (Adjusted for 2019 +3%)") + 
        xlab("Annual Income (USD)") +
        xlim(40000,70000)
# Density Histogram
dens_pd <- ggplot(data=fypd_2018, aes(fypd_2018$NRSA_2019_INCOME)) +
        geom_histogram(aes(y=..density..),
                       binwidth = 200,
                       fill=I(maze),
                       col=I(blue)) +
        geom_density(col=2) +
        ggtitle("Histogram of First-Year Postdocs at UM (Adjusted for 2019 +3%)") + 
        xlab("Annual Income (USD)") +
        xlim(40000,70000)
# Arrange 2 plots side-by-side
ggarrange(hist_pd, dens_pd, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

sort(table(fypd_2018$NRSA_2019_INCOME))

# Determine the percentile of job offer
quantile(bi_first, c(0.00, 0.32, 0.57, 0.98))

# Schmidt Lab offer percentile
quantile(fypd_2018$NRSA_2019_INCOME, c(0.29, 0.30, 0.40, 0.45, 0.46))
# Tom's offer lies between the 30 - 45th percentile

# Kretlzer Lab offer percentile
quantile(fypd_2018$NRSA_2019_INCOME, c(0.80, 0.90, 0.97,0.98))
# Potential Kretlzer Lab offer is > 97th percentile 
# -- trade off, no assistant professorship, career mobility (difference is 75K over 3 years not including acreal of investments ... literally half of industry)

# Who is at or above 70K?
fypd_2018 %>% filter(NRSA_2019_INCOME >= 70000) %>% dplyr::select(NAME, NRSA_2019_INCOME)

# Brad Bitterly (Ross School of Business, expert in negaotiation strategy)
# 77434.34
# https://michiganross.umich.edu/faculty-research/faculty/brad-bitterly

# Keith Levin
# 111515.78
# http://www-personal.umich.edu/~klevin/
# http://www-personal.umich.edu/~klevin/klevin_cv.pdf

# Xiaxin Wang
# 103245.79
# https://lsa.umich.edu/econ/people/faculty/xiaxinw.html
# https://lsa.umich.edu/econ/people/faculty/xiaxinw/_jcr_content/file.res/Xiaxin%20Wang%20CV.pdf

# Larry Kruger (Epidemiology)
# 82613.15

# Esteban Peralta
# 123894.95
# https://lsa.umich.edu/econ/people/faculty/eperalta.html
# https://lsa.umich.edu/econ/people/faculty/eperalta/_jcr_content/file.res/CV%20(2).pdf

# Common denomenators (not a strong publication record)
# Lecturers
# Data Scientists, economic theory on the high end
# Winner of training grants


quantile(fypd_2018$NRSA_2019_INCOME, c(0.00, 0.32, 0.57, 0.98))


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




