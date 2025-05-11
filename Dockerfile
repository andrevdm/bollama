FROM andrevdm/ghc-musl:ghc_9_12_2_musl AS builder
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

