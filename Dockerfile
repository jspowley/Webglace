FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    git

# RUN R -e "install.packages(c('devtools', 'shiny', 'bslib', 'tidyverse', 'DT', 'rlang', 'rhandsontable', 'plotly', 'shinyalert', 'lubridate', 'tidyquant', 'facmodCS', 'moments', 'Rcpp', 'shinydashboard', 'profvis', 'shinyalert'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"
RUN R -e "install.packages(c('devtools'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"

# RUN git clone https://github.com/jspowley/scrapedemo.git /opt/my_dependencies/scrapedemo
# RUN chown -R shiny:shiny /opt/my_dependencies
# RUN git clone https://github.com/jspowley/webglace.git /opt/Webglace
# RUN R -e "setwd('/opt/Webglace'); devtools::install(dependencies = TRUE)"

#RECENT
# RUN R -e "devtools::install_github('jspowley/scrapedemo', dependencies = TRUE)"
RUN R -e "devtools::install_github('jspowley/Webglace', dependencies = TRUE)"


#OLD
# RUN R -e "devtools::install_github('https://github.com/jspowley/eiatools')"
# RUN git clone https://github.com/jspowley/bond_app.git /srv/shiny-server/webglace_pack
# RUN R -e "setwd('/srv/shiny-server/webglace_pack'); setwd('/srv/shiny-server/webglace_pack')"


# RUN chown -R shiny:shiny /srv/shiny-server/webglace

EXPOSE 3838

#CMD ["/init"]
CMD ["R", "-e", "Webglace::run_app(options = list(appDir = '/srv/shiny-server/webglace/', host = '0.0.0.0', port = 3838))"]
#RUN R -e "library(Webglace); print(ls('package:Webglace'))"
#CMD ["R", "-e", "Webglace::run_app(host = '0.0.0.0', port = 3838)"]