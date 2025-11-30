FROM rust:alpine AS build

ARG DIRECTORY=.

ADD . /src
WORKDIR /src
RUN apk add build-base
WORKDIR $DIRECTORY
RUN cargo install --locked stak
RUN cargo build --locked --release --target $(uname -m)-unknown-linux-musl

FROM scratch

ARG BINARY=stak
ARG DIRECTORY=.

COPY --from=build /src/$DIRECTORY/target/*-unknown-linux-musl/release/$BINARY /
ENTRYPOINT ["/$BINARY"]
