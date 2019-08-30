FROM haskell:8.6.5 AS build-stage

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Create /opt/sleepchart-server/bin and /opt/sleepchart-server/src.  Set
# /opt/sleepchart-server/src as the working directory.
RUN mkdir -p /opt/sleepchart-server/src
RUN mkdir -p /opt/sleepchart-server/bin
WORKDIR /opt/sleepchart-server/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/sleepchart-server/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/sleepchart-server/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's package.yaml file.
COPY ./package.yaml /opt/sleepchart-server/src/package.yaml
RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/sleepchart-server/src
RUN stack --no-terminal build

# Install application binaries to /opt/sleepchart-server/bin.
RUN stack --no-terminal --local-bin-path /opt/sleepchart-server/bin install

# Remove source code.
RUN rm -rf /opt/sleepchart-server/src


FROM debian:stretch

RUN apt-get update && \
    apt-get install -y --no-install-recommends gnupg ca-certificates dirmngr curl git && \
    echo 'deb http://downloads.haskell.org/debian stretch main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA3CBA3FFE22B574 

RUN apt-get upgrade -y --assume-yes
RUN apt-get install -y --assume-yes libpq-dev build-essential netbase

COPY --from=build-stage /opt/sleepchart-server /opt/sleepchart-server

COPY ./jwk.json /opt/sleepchart-server
COPY ./audience.json /opt/sleepchart-server
COPY ./conf/database-setting.yml /opt/sleepchart-server/conf/database-setting.yml

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/sleepchart-server
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/sleepchart-server/bin"

# Set the working directory as /opt/sleepchart-server/.
WORKDIR /opt/sleepchart-server

CMD /opt/sleepchart-server/bin/sleepchart-server-exe
