## -*- mode: dockerfile -*-

FROM erlang:23
WORKDIR /buildroot
COPY . /buildroot
RUN rebar3 as prod release -o /{{name}}
WORKDIR /{{name}}/{{name}}
CMD /{{name}}/{{name}}/bin/{{name}} foreground
