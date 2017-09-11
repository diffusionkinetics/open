FROM filopodia/minidebhs:lts-8.5

RUN cd /tmp && stack install -j2 lucid postgresql-simple \
 typed-process scotty-cookie microlens-platform wreq blaze-html

COPY stack.yaml /src-boot/stack.yaml
COPY todo.cabal /src-boot/

RUN cd /src-boot && stack build -j 4 --dependencies-only

COPY . /opt/todo

WORKDIR /opt/todo

RUN stack install -j2  todo:exe:todo \
      && rm -rf /opt/todo/

EXPOSE 3001

WORKDIR /root

ENV PATH "$PATH:/root/.local/bin"

CMD ["/root/.local/bin/todo", "serve"]