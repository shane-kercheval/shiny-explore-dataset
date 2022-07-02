FROM r-base

RUN apt-get update -y && apt-get install zsh -y
RUN PATH="$PATH:/usr/bin/zsh"

RUN mkdir /code
WORKDIR /code
ENV PATH "$PATH:/code"

RUN apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev libgit2-dev pandoc

RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('scales')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('tidytext')"
RUN R -e "install.packages('widyr')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('ggrepel')"
RUN R -e "install.packages('forecast')"
RUN R -e "install.packages('janitor')"
RUN R -e "install.packages('fuzzyjoin')"
RUN R -e "install.packages('ggridges')"
RUN R -e "install.packages('ggraph')"
RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('Lahman')"
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('dgrtwo/ebbr')"
RUN R -e "devtools::install_github('davidsjoberg/ggsankey')"
RUN R -e "install.packages('languageserver')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('shinyBS')"
RUN R -e "install.packages('ggExtra')"
RUN R -e "install.packages('fpp2')"
RUN R -e "install.packages('lattice')"
RUN R -e "install.packages('maps')"
RUN R -e "install.packages('mapproj')"

RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('shinyBS')"

RUN R -e "devtools::install_github('shane-kercheval/rtools')"

RUN mkdir shiny-explore-dataset 
COPY ./shiny-explore-dataset ./shiny-explore-dataset

