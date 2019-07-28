FROM alpine:latest
LABEL author="Caravela Hacker Club"

RUN apk add --no-cache \
    nodejs \
    npm \
    && npm install -g --unsafe-perm \
    elm \
    elm-analyse
