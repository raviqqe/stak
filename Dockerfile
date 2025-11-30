FROM rust:alpine AS build

ARG DIRECTORY=.

ADD . /src
WORKDIR /src
RUN apk add build-base
WORKDIR $DIRECTORY
RUN cargo build --release --locked --target $(uname -m)-unknown-linux-musl

FROM scratch

ARG BINARY=stak

COPY --from=build /src/target/*-unknown-linux-musl/release/$BINARY /$BINARY
RUN ["/$BINARY", "--version"]
ENTRYPOINT ["/$BINARY"]
