# Dockerfile for papercheck plumber API

# Base image
FROM rocker/r-ver:latest

# system dependencies
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libicu-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

# install R dependencies
RUN R -e "install.packages(c('remotes', 'plumber'))"
RUN R -e "remotes::install_cran(c('dplyr', 'tidyr', 'logger', 'progress', 'tidytext', 'SnowballC', 'xml2', 'statcheck', 'httr', 'jsonlite', 'quarto', 'knitr'))"

# copy app
COPY . /srv/papercheck
WORKDIR /srv/papercheck

# install papercheck
RUN R -e "remotes::install_local(dependencies = TRUE)"

# expose port
EXPOSE 2005

# run API
CMD ["R", "-e", "api <- plumber::plumb('api.R'); api$run(host='0.0.0.0', port=2005)"]
