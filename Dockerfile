FROM haskell:8.0.2

COPY . /tmp/code

WORKDIR /tmp/code

RUN cabal update
RUN cabal install --only-dependencies
RUN cabal install --bindir=.

FROM debian:stretch
COPY --from=0 /tmp/code/tttool /usr/local/bin

RUN apt-get update -y
RUN apt-get install -y libgmp10 libgmp-dev

ENTRYPOINT [ "/usr/local/bin/tttool" ]