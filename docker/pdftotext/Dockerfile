#===============================================================================
#
#         FILE: Dockerfile
#    DEV USAGE: docker run -it -v /Users/Alec/Documents/umich_salary:/Users/Alec/Documents/umich_salary --name pdftotext steepale/pdftotext:1.0
#        USAGE: docker image build -t steepale/steepale/pdftotext:1.0 . # local image build
#
#  DESCRIPTION:  This Dockerfile will create a pdftotext installation
# REQUIREMENTS:  ---
#        NOTES:  This Image was interactively built, you can use this Dockerfile as a guild
#       AUTHOR:  Alec Steep, alec.steep@gmail.com
#  AFFILIATION:  Michigan State University (MSU), East Lansing, MI, United States
#				         USDA ARS Avian Disease and Oncology Lab (ADOL), East Lansing, MI, United States
#				         Technical University of Munich (TUM), Weihenstephan, Germany
#      VERSION:  1.0
#      CREATED:  2019.10.01
#     REVISION:  ---
#===============================================================================

# Base Ubuntu image
FROM steepale/ubuntu_base:18.04

# Set working dorectory
WORKDIR /

# Update the OS and install dependencies
RUN sudo apt-get update \
    && sudo apt-get install -y poppler-utils \
    bsdmainutils

# Working directory
WORKDIR /

# Final command
CMD ["/bin/bash"]
