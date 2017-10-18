FROM bitnami/minideb:jessie

RUN apt-get update \
 && apt-get -y install wget build-essential libffi-dev libgmp-dev xz-utils zlib1g-dev libpq-dev \
                       git gnupg libgsl0-dev liblapack-dev libatlas-base-dev pkg-config \
 && wget -qO- https://get.haskellstack.org/ | sh && rm -rf /var/lib/apt/lists/*

RUN cd /tmp && echo "resolver: lts-8.5" > stack.yaml && echo "packages: []" >> stack.yaml \
 && cat stack.yaml \
 && stack setup \
 && stack install lens scotty hmatrix uuid random-fu haskell-src-exts
