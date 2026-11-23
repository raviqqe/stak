FROM rust:1.100.0-alpine@sha256:8d0e82abfdf9a38071e07cabc5afba4ddbe077631a80e4e742a760c38beadc33 AS build

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

FROM scratch

ENV PATH=
COPY --from=build /app /app
USER 65532:65532
ENTRYPOINT ["/app"]
