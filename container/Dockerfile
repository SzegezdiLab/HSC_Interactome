FROM rocker/rstudio:4.2.2

RUN apt-get update --fix-missing && apt-get install -y apt-utils \
    procps \
    wget \
    bzip2 \
    ca-certificates \
    libglib2.0-0 \
    libxext6 \
    libsm6 \
    libxrender1 \
    subversion \
    build-essential \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libhdf5-dev \
    graphviz

FROM bioconductor/bioconductor_docker:3.17

RUN Rscript -e 'install.packages("remotes", dependencies = TRUE, repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'remotes::install_version("reticulate", "1.28")'
RUN Rscript -e 'install.packages(c("shiny", "DT", "tidyverse" ,"Matrix", "patchwork", "shinycssloaders", "rvest", "devtools"), dependencies = TRUE, repos = "http://cran.us.r-project.org")'
RUN Rscript -e "devtools::install_github('Sarah145/CCPlotR')"

WORKDIR /home/rstudio