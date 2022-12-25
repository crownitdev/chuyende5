FROM rocker/shiny:latest

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev
  

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "bslib", \
              "ggplot2" \
            ))'

# run app
CMD ["/usr/bin/shiny-server"]