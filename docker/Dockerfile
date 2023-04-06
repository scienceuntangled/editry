#
# Dockerfile for editly, modified from https://github.com/vimagick/dockerfiles/blob/master/editly/Dockerfile
#

FROM node:lts-bullseye
MAINTAINER Science Untangled

ARG FFMPEG_VERSION=5.1.1

RUN set -xe \
 && apt update \
 && apt install -y curl dumb-init fonts-noto-cjk xvfb xz-utils \
 && curl -sSL https://www.johnvansickle.com/ffmpeg/old-releases/ffmpeg-5.1.1-amd64-static.tar.xz \
    | tar xJC /usr/bin/ ffmpeg-${FFMPEG_VERSION}-amd64-static/ffprobe ffmpeg-${FFMPEG_VERSION}-amd64-static/ffmpeg --strip 1 \
 && ffmpeg -version \
 && ffprobe -version \
 && rm -rf /var/lib/apt/lists/*

ADD https://api.github.com/repos/scienceuntangled/editly/git/refs/heads/logo editly_version.json

RUN set -xe \
 && apt update \
 && apt install -y libxext-dev libxi-dev xserver-xorg-dev \
 && ln -sf /usr/bin/python3 /usr/bin/python \
 && npm install --global --unsafe-perm scienceuntangled/editly.git#logo \
 && apt remove -y libxext-dev libxi-dev xserver-xorg-dev \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /data

ENTRYPOINT ["/usr/bin/dumb-init", "--", "xvfb-run", "--server-args", "-screen 0 1280x1024x24 -ac", "editly"]
CMD ["--help"]
