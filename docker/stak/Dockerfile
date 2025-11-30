FROM rust:alpine AS build
ADD ../.. /src
WORKDIR /src
RUN apk add build-base
RUN cargo build --release --locked --target $(uname -m)-unknown-linux-musl

FROM scratch
COPY --from=build /src/target/*-unknown-linux-musl/release/stak /stak
RUN ["/stak", "--version"]
ENTRYPOINT ["/stak"]
