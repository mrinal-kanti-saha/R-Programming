#### Multi Stage Build
# Stage 0 - Base Build containing r-base + r-dev + shiny server + tidyverse
# Stage 1 - ShinyApp Developed

# Stage 0
# get shiny server and R from the rocker project
FROM rocker/shiny:4.1.2 AS rshinyverse
# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
	libxml2-dev
# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "caret", \
              "tidyverse" \ 
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-11-01"\
          )'

# Stage 1	  
FROM rshinyverse	  
# copy the app directory into the image
COPY ./TipsApp/* /srv/shiny-server/
# run app
CMD ["/usr/bin/shiny-server"]
