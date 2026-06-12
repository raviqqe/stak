FROM rust:1.96.0-alpine@sha256:2ea3db105d38fdfa4e31f366674287fcaa828087e2fe3973befdc537f2d443b1 AS build

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
