FROM swipl:7.5.12
MAINTAINER Chris Mungall <cjmungall@lbl.gov>

RUN apt-get update && apt-get -y install make curl
RUN mkdir /data && curl -L http://purl.obolibrary.org/obo/mondo.owl -o /data/mondo.owl && curl -L http://purl.obolibrary.org/obo/doid.owl -o /data/doid.owl && curl -L http://purl.obolibrary.org/obo/ncit/neoplasm-core.owl -o /data/neoplasm-core.owl

ADD ./prolog/ /tools/prolog
ADD ./bin/ /tools/bin
ADD ./utf8.pl /tools/
ADD ./install.pl /tools/
ADD ./pack.pl /tools/
ADD ./tests/data/inputs.txt /tools/
WORKDIR /tools
RUN swipl -l install -g install_requirements,halt
ENV PATH "/tools/bin:$PATH"

RUN swipl -G0  -p library=/tools/prolog -l /tools/utf8.pl /tools/bin/neoplasmer -X .cache -i /data/mondo.owl -i /data/doid.owl -i /data/neoplasm-core.owl inputs.txt

EXPOSE ${PORT}
CMD swipl -p library=prolog ./bin/neoplasmer -h
