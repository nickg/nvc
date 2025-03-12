FROM debian:bookworm AS build
WORKDIR /src
COPY . .
RUN ln -s contrib/debian .
ARG VERSION
RUN sed -i "s/nvc (.*)/nvc ($VERSION-1)/" debian/changelog
RUN apt-get -y update
RUN apt-get -y build-dep .
RUN DEB_BUILD_OPTIONS=nocheck dpkg-buildpackage -uc -us -b

FROM debian:bookworm
RUN --mount=from=build,target=/src \
    apt-get -y update && \
    apt-get -y install /src/nvc_*.deb && \
    rm -rf /var/lib/apt/lists/*
RUN nvc --version
CMD ["/usr/bin/nvc"]
LABEL org.opencontainers.image.source=https://github.com/nickg/nvc
