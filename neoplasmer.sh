#!/bin/sh
docker run -v $PWD:/work -w /work --rm -ti cmungall/neoplasmer swipl -G0 -l utf8.pl -p library=/tools/prolog /tools/bin/neoplasmer -X .cache -i /data/mondo.owl "$@"

