# Use an R base image with a specific version
FROM rocker/r-ver:4.4.1
RUN echo 'options(repos = c(P3M = "https://packagemanager.posit.co/cran/__linux__/jammy/latest", CRAN = "https://cloud.r-project.org"))' >>"${R_HOME}/etc/Rprofile.site"

# Install system dependencies for R package development
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    pandoc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install key R development packages
RUN R -e "install.packages(c('devtools', 'remotes', 'roxygen2', 'testthat', 'usethis'), repos='https://cloud.r-project.org/')"

# Set up a working directory
WORKDIR /usr/local/src

# Set container to start with R shell
CMD ["R"]
