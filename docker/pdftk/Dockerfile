#===============================================================================
#
#         FILE: Dockerfile
#    DEV USAGE: docker run -it -v /Users/Alec/Desktop/screen_shots/20191202_vet-med-human-health_schwabe:/Users/Alec/Desktop/screen_shots/20191202_vet-med-human-health_schwabe -rm --name pdftk steepale/centos_base:6
#        USAGE: docker image build -t steepale/pdftk:1.0 . # local image build
#
#  DESCRIPTION:  This Dockerfile will create a pdftk installation in CentOS Linux 6 RPMs (64-bit)
# REQUIREMENTS:  ---
#        NOTES:  This Image was interactively built, you can use this Dockerfile as a guild
#       AUTHOR:  Alec Steep, alec.steep@gmail.com
#  AFFILIATION:  Michigan State University (MSU), East Lansing, MI, United States
#                                        USDA ARS Avian Disease and Oncology Lab (ADOL), East Lansing, MI, United States
#                                        Technical University of Munich (TUM), Weihenstephan, Germany
#      VERSION:  1.0
#      CREATED:  2019.12.02
#     REVISION:  ---
#===============================================================================

# Base Ubuntu image
FROM steepale/centos_base:6

# Install pdftk CentOS Linux 6 RPMs: (https://www.pdflabs.com/docs/install-pdftk-on-redhat-or-centos/)
# Install dependency libgcj
WORKDIR /src

RUN sudo yum update -y \
    && wget https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/pdftk-2.02-1.el6.x86_64.rpm \
    && sudo yum install -y libgcj \
    && sudo rpm -i pdftk-2.02-1.*.rpm

# Working directory
WORKDIR /

# Final command
CMD ["/bin/bash"]