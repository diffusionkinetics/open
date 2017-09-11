FROM filopodia/minidebhs:lts-8.5

RUN cd /tmp && stack install -j2 lucid postgresql-simple \
 typed-process scotty-cookie microlens-platform wreq blaze-html

COPY stack.docker.yaml /src-boot/stack.yaml
COPY filocore.cabal /src-boot/

RUN cd /src-boot && stack build -j 4 --dependencies-only

COPY . /opt/todo

WORKDIR /opt/todo

RUN stack install -j2 --stack-yaml stack.docker.yaml todo:exe:todo \
      && rm -rf /opt/filocore/

EXPOSE 3001

WORKDIR /root

ENV PATH "$PATH:/root/.local/bin"

CMD ["/root/.local/bin/todo", "serve"]