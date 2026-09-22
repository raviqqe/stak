FROM rust:1.98.1-alpine@sha256:7cc1c22d77d9432f7fe012a70e6d3e555af54c2a6832700ed7d553f1769ae89f AS build

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
