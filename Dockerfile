FROM rust:1.96.0-alpine@sha256:66f48b19d6e88519e2e58bebe0d945779a6a4ca41c2db17db78c9569655b50ac AS build

RUN apk update && apk add build-base
RUN cargo install --locked stak

ARG BINARY=stak
ARG DIRECTORY=.

ADD . /src
WORKDIR /src/$DIRECTORY
# TODO Enable the `--lock` option always.
# The current implementation of Dependabot cannot update internal dependencies
# in different workspaces automatically.
RUN cargo build $([ $DIRECTORY = . ] && echo --locked) --release --bin $BINARY --target $(uname -m)-unknown-linux-musl
RUN cp target/*-unknown-linux-musl/release/$BINARY /app
RUN strip /app

FROM scratch

ENV PATH=
COPY --from=build /app /app
ENTRYPOINT ["/app"]
