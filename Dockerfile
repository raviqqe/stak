ARG RUST_VERSION=1.92

FROM rust:$RUST_VERSION-alpine AS build

RUN apk update && apk add build-base
RUN cargo install --locked stak

ARG BINARY=stak
ARG DIRECTORY=.

ADD . /src
WORKDIR /src/$DIRECTORY
RUN cargo build --locked --release --bin $BINARY --target $(uname -m)-unknown-linux-musl
RUN cp target/*-unknown-linux-musl/release/$BINARY /app

FROM scratch

ENV PATH=
COPY --from=build /app /app
ENTRYPOINT ["/app"]
