# Base R Shiny image
FROM rocker/shiny:4.3.3

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Create a directory in the container
RUN mkdir /home/shiny-app

# Set the CRAN repository
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org/'))"

# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Copy the Shiny app files and renv settings
WORKDIR /home/shiny-app/
COPY app.R .
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Renviron .Renviron
COPY users.RDS users.RDS
COPY code_list.csv code_list.csv
COPY indtable.csv indtable.csv
COPY www/ www/
COPY d2_ou.csv d2_ou.csv
COPY dval_ind.RData dval_ind.RData

# Restore the R environment with renv
RUN R -e "renv::restore()"

# Expose the Shiny application port
EXPOSE 8080

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/home/shiny-app', host='0.0.0.0', port=8080)"]
