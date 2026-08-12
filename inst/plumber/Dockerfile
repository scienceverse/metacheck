# Dockerfile for metacheck plumber API

# Base image
FROM rocker/r-ver:4.5

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
    cmake \
    git \
    libwebp-dev \
    && rm -rf /var/lib/apt/lists/*

# install R dependencies (pre-cache heavy packages for better Docker layer caching)
RUN R -e "install.packages(c('remotes', 'plumber'))"
RUN R -e "remotes::install_cran(c('dplyr', 'tidyr', 'logger', 'progress', 'tidytext', 'SnowballC', 'xml2', 'statcheck', 'httr2', 'jsonlite', 'quarto', 'knitr', 'arrow', 'ellmer', 'rvest', 'officer', 'DT', 'bibtex', 'bib2df', 'roxygen2', 'gitcreds', 'anytime', 'htmltools', 'base64enc', 'urltools', 'fs', 'rappdirs', 'curl'))"

# Quarto CLI — report() renders the HTML report via quarto::quarto_render().
# The `quarto` R package (installed above) is only a wrapper around this binary,
# which rocker/r-ver does not ship. Download it with R (libcurl) so we don't add
# a curl CLI to the image. The version is pinned for reproducible builds (bump
# QUARTO_VERSION deliberately); the `quarto --version` at the end fails the build
# loudly if the install is broken. HTML-only output → no TinyTeX/LaTeX needed.
ARG QUARTO_VERSION=1.9.38
RUN R -e "download.file('https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb', '/tmp/quarto.deb', quiet = TRUE)" \
    && apt-get update && apt-get install -y /tmp/quarto.deb \
    && rm -rf /var/lib/apt/lists/* /tmp/quarto.deb \
    && quarto --version

# In the production deployment the container runs with a read-only root
# filesystem and only /tmp writable (a tmpfs — see docker-compose.yml), so point
# every transient Quarto/Deno write at it. Without this, quarto_render can't
# create its caches at runtime and the HTML render fails (gracefully — the JSON
# check still succeeds — but no report is produced).
ENV XDG_CACHE_HOME=/tmp/.cache \
    DENO_DIR=/tmp/.deno \
    TMPDIR=/tmp

# copy app
COPY . /srv/metacheck
WORKDIR /srv/metacheck

# install metacheck
RUN R -e "remotes::install_local(dependencies = TRUE)"

# expose port
EXPOSE 2005

# run API
CMD ["R", "-e", "api <- plumber::plumb('./inst/plumber/api.R'); api$run(host='0.0.0.0', port=2005)"]
