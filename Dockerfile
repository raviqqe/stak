FROM rust:alpine AS build

RUN apk add build-base
RUN cargo install --locked stak

ARG BINARY=stak
ARG DIRECTORY=.

ADD . /src
WORKDIR /src/$DIRECTORY
RUN cargo build --locked --release --bin $BINARY --target $(uname -m)-unknown-linux-musl

FROM scratch

ARG BINARY=stak
ARG DIRECTORY=.

COPY --from=build /src/$DIRECTORY/target/*-unknown-linux-musl/release/$BINARY /app
ENTRYPOINT ["/app"]
