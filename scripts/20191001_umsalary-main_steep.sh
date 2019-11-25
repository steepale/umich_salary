#===============================================================================
#
#         FILE: /Users/Alec/Documents/umich_salary/scripts/20191001_umsalary-main_steep.sh
#        USAGE: Purely documentation script
#
#  DESCRIPTION:  Documentation script for workflow to analyze UM salary data
# REQUIREMENTS:  ---
#        NOTES:  
#       AUTHOR:  Alec Steep, alec.steep@gmail.com
#  AFFILIATION:  Michigan State University (MSU), East Lansing, MI, United States
#				         USDA ARS Avian Disease and Oncology Lab (ADOL), East Lansing, MI, United States
#				         Technical University of Munich (TUM), Weihenstephan, Germany
#      VERSION:  1.0
#      CREATED:  2019.10.01
#     REVISION:  ---
#===============================================================================

# Interactively enter docker image and mount data
docker run -it -v /Users/Alec/Documents/umich_salary:/Users/Alec/Documents/umich_salary --rm --name pdftotext steepale/pdftotext:1.0

# Working dir
cd /Users/Alec/Documents/umich_salary

# Documentation:
# Data available in PDF format
# https://hr.umich.edu/working-u-m/management-administration/hr-reports-data-services/hr-data-requests-standard-reports

# Steps in analysis:
# 1. Extract & clean data from pdf format to txt format
# 2. Feed clean data inot R for stat analysis


# 1. Extract data from PDF format to txt format
for yr in "2015" "2016" "2017" "2018"
do
echo ${yr}
# Convert pdf to text
# Notes: 
# Keep general layout
# Ensure ASCII7 encoding
pdftotext -layout -enc ASCII7 ./docs/${yr}_UMsalary.pdf ./docs/${yr}_UMsalary.tmp
done

for yr in "2015" "2016"
do
# Remove lines above string CAMPUS
# Print year
# Remove empty lines
# Remove headers scattered across multiple lines
# Convert long stretches (2 or more) of spaces to single tab
# Conver spaces to underscore
# Remove -Month string
# Forcast year as new column
echo "${yr}"
sed -n '/^CAMPUS/,$p' ./docs/${yr}_UMsalary.tmp | \
sed '/^$/d' | \
sed s/"'"/""/g | \
grep -v -e "^CAMPUS" -e "ANNUAL FTR" -e "ROM GENL$" -e "FUND$" -e"Report ID:" -e "SALARY RATE OF FACULTY AND STAFF" | \
sed 's/ \+ /\t/g' | \
sed 's/ /_/g' | \
sed 's/-Month//g' | \
sed "s/$/\t${yr}/g" | \
sed 's/_-_/_/g' \
> ./data/${yr}_UMsalary.txt
done

for yr in "2017"
do
# Remove lines above string CAMPUS
# Print year
# Remove empty lines
# Remove headers scattered across multiple lines
# Convert long stretches (2 or more) of spaces to single tab
# Conver spaces to underscore
# Remove -Month string
# Forcast year as new column
echo "${yr}"
sed -n '/^CAMPUS/,$p' ./docs/${yr}_UMsalary.tmp | \
sed 's/  / /g' | \
sed 's/UM_ANNARBOR/UM_ANN-ARBOR/g' | \
sed 's/8Month/8/g; s/9Month/9/g; s/12Month/12/g' | \
sed '/^$/d' | \
sed s/"'"/""/g | \
grep -v -e "^CAMPUS" -e "ANNUAL FTR" -e "ROM GENL$" -e "FUND$" -e"Report ID:" -e "SALARY RATE OF FACULTY AND STAFF" | \
sed 's/ \+ /\t/g' | \
sed 's/ /_/g' | \
sed "s/$/\t${yr}/g" | \
sed 's/_-_/_/g' \
> ./data/${yr}_UMsalary.txt
done

for yr in "2018"
do
# Remove lines above string CAMPUS
# Print year
# Remove empty lines
# Remove headers scattered across multiple lines
# Convert long stretches (2 or more) of spaces to single tab
# Conver spaces to underscore
# Remove -Month string
# Forcast year as new column
echo "${yr}"
sed -n '/^CAMPUS/,$p' ./docs/${yr}_UMsalary.tmp | \
sed 's/  / /g' | \
sed 's/UM_ANNARBOR/UM_ANN-ARBOR/g' | \
sed '/^$/d' | \
sed s/"'"/""/g | \
grep -v -e "^CAMPUS" -e "ANNUAL FTR" -e "ROM GENL$" -e "FUND$" -e"Report ID:" -e "SALARY RATE OF FACULTY AND STAFF" -e "FRACTION$"| \
sed 's/\.[0-9][0-9] /\.00  /g' | \
sed 's/Month /Month  /g' | \
sed 's/Month//g' | \
sed 's/ \+ /\t/g' | \
sed 's/ /_/g' | \
sed "s/$/\t${yr}/g" | \
sed 's/_-_/_/g' \
> ./data/${yr}_UMsalary.txt
done

sed 's/8Month/8/g; s/9Month/9/g; s/12Month/12/g' | \

# Concatentate all the files
echo "CAMPUS,NAME,TITLE,DEPT,APPT_ANNUAL_FTR,APPT_FTR_BASIS,APPT_FRACTION,PAID,YEAR" | tr ',' '\t' > ./data/UMsalary.txt
# Append with concatentated data
# Some data is difficult to format because fields are single space deliimited; we will simply discard this data
cat ./data/*_UMsalary.txt | awk '($9 == "2015" ) || ($9 == "2016" ) ||  ($9 == "2017" ) ||  ($9 == "2018" )' >> ./data/UMsalary.txt






# Exit docker image

# Working dir
cd /Users/Alec/Documents/umich_salary



sed -n '117p' ./data/UMsalary.txt
