FROM alpine:3.20 AS builder
RUN apk update
RUN apk add --no-cache curl gcc g++ git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz zlib-dev zlib-static ncurses-dev ncurses-static ncurses-terminfo ncurses-terminfo-base

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_GHC_VERSION=9.10.1 BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1.0 BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

WORKDIR /build
COPY bollama.cabal /build/
COPY README.md /build/
COPY app /build/app
COPY src /build/src
COPY cabal.project /build/

RUN --mount=type=cache,target=/root/.cabal,id=compiler_cabal_cache \
  cabal update && cabal build --enable-executable-static && \
  mkdir -p /tmp/bin/ && \
  cp -f $(cabal list-bin bollama-exe) /tmp/bin/bollama

FROM alpine:3.20
RUN apk add --no-cache ncurses-terminfo-base bash
COPY --from=builder /tmp/bin/bollama /bollama/bollama
ENV TERM=xterm-256color

WORKDIR /bollama/

