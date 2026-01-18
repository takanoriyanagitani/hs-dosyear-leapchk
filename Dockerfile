FROM haskell:9.12.2-slim-bookworm AS builder
RUN echo cabal update date: 2026-01-05
RUN cabal update --verbose=2

WORKDIR /hs-dosyear-leapchk
COPY --link ./hs-dosyear-leapchk.cabal ./
RUN cabal update --verbose=2
RUN cabal build --only-dependencies
COPY --link ./app/ ./app/
COPY --link ./src/ ./src/
COPY --link ./LICENSE ./
RUN cabal build
RUN cp $( cabal list-bin hs-dosyear-leapchk | fgrep --max-count=1 hs-dosyear-leapchk ) /usr/local/bin/
RUN which hs-dosyear-leapchk
RUN echo "--- Running demo for 1980 (leap) ---" && \
    echo 1980 | hs-dosyear-leapchk
RUN echo "--- Running demo for 1981 (not leap) ---" && \
    (echo 1981 | hs-dosyear-leapchk || true)
RUN echo "--- Running demo for 'foo' (invalid) ---" && \
    (echo 'foo' | hs-dosyear-leapchk || true)

FROM debian:bookworm-slim
COPY --link --from=builder /usr/local/bin/hs-dosyear-leapchk /usr/local/bin/

CMD ["/usr/local/bin/hs-dosyear-leapchk"]
