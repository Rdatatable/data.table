FROM registry.gitlab.com/jangorecki/dockerfiles/SRC_IMAGE_NAME

MAINTAINER Jan Gorecki j.gorecki@wit.edu.pl

COPY bus/build/cran/ /cran/

RUN Rscript -e 'install.packages("data.table", repos=file.path("file:","cran"))'

CMD ["R"]
