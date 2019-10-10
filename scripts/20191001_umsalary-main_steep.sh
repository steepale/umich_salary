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
# 2. 


# 1. Extract data from PDF format to txt format
for yr in "2017"
do
echo ${yr}
# Convert pdf to text
# Notes: 
# Keep general layout
# Ensure ASCII7 encoding
pdftotext -layout -enc ASCII7 ./docs/${yr}_UMsalary.pdf ./docs/${yr}_UMsalary.tmp
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
if [ ${yr} = "2017" ]
then
echo "Unique adjustment to files of year: ${yr}"
sed -i -n '/^CAMPUS/,$p' ./docs/${yr}_UMsalary.tmp
sed -i 's/  / /g' ./docs/${yr}_UMsalary.tmp
fi
echo ${yr}
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

# Concatentate all the files
echo "CAMPUS,NAME,TITLE,DEPT,APPT_ANNUAL_FTR,APPT_FTR_BASIS,APPT_FRACTION,PAID,YEAR" | tr ',' '\t' > ./data/UMsalary.txt
# Append with concatentated data
cat ./data/*_UMsalary.txt >> ./data/UMsalary.txt

grep "ANNARBOR" ./data/UMsalary.txt | less

grep "Month" ./data/UMsalary.txt | less

# dev
cat ./data/UMsalary.txt | column -t | less -N
grep "'" ./data/UMsalary.txt




# Exit docker image

# Working dir
cd /Users/Alec/Documents/umich_salary



sed -n '117p' ./data/UMsalary.txt
