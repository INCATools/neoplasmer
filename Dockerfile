FROM swipl:7.5.12
MAINTAINER Chris Mungall <cjmungall@lbl.gov>

RUN apt-get update && apt-get -y install make curl
RUN mkdir /data && curl -L http://purl.obolibrary.org/obo/mondo.owl -o /data/mondo.owl && curl -L http://purl.obolibrary.org/obo/ncit/neoplasm-core.owl -o /data/neoplasm-core.owl

ADD ./prolog/ /tools/prolog
ADD ./bin/ /tools/bin
ADD ./utf8.pl /tools/
WORKDIR /tools
RUN swipl -g "Opts=[interactive(false)],pack_install(sparqlprog,[url('https://github.com/cmungall/index_util/archive/v0.0.2.zip')|Opts]),pack_install(sparqlprog,Opts),pack_install(rdf_matcher,Opts),halt"
ENV PATH "/tools/bin:$PATH"

EXPOSE ${PORT}
CMD swipl -p library=prolog ./bin/neoplasmer -h
