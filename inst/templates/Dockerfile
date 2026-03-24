# Use an appropriate base image that includes R and RStudio
FROM 537212169289.dkr.ecr.eu-central-1.amazonaws.com/rstudio-standard:R4.4.1-14oct2024

# Update the package list and install necessary system dependencies
RUN apt-get update -qq && apt-get -y --no-install-recommends install libsasl2-dev

# Install the necessary R packages
RUN Rscript -e 'install.packages("here")'