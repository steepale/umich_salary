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
#####     Load Data for Random Forest      #####################################
################################################################################

# Load in the sample annotation table
df <- read.table(file = './data/UMsalary.txt', header = TRUE, sep = '\t')
df <- as_tibble(df)

# Remove commas from income
df$APPT_ANNUAL_FTR <- str_replace(df$APPT_ANNUAL_FTR, ',', '') %>% as.numeric()
df <- df[!is.na(df$APPT_ANNUAL_FTR),]

# Calculate annual income
df$APPT_ANNUAL_INCOME <- df$APPT_ANNUAL_FTR / df$APPT_FTR_BASIS * 12

# Annotate employee year (only if not working in 2015 -- interested in 1st year postdocs)

# Create seperate data frame
df_name <- as.data.frame(table(df$NAME))
names(df_name) = c("NAME", "YRS_POST_2015")
# perform left join
df <- left_join(df, df_name, by = "NAME")

sort(table(df$TITLE))

################################################################################
############ Determine Income for 1st Year Postdocs ############################
################################################################################

# Create a dataframe of first year postdocs
df_pd <- filter(df, TITLE == 'RESEARCH_FELLOW' & YRS_POST_2015 == 1 & YEAR != 2015)

# Refactor some columns
df_pd$DEPT <- as.character(df_pd$DEPT)
df_pd$DEPT <- as.factor(df_pd$DEPT)

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

