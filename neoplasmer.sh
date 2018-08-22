#!/bin/sh
docker run -p 9055:9055 -e PORT=9055 -v $PWD:/work -w /work --rm -ti cmungall/neoplasmer swipl -G0  -p library=/tools/prolog -l /tools/utf8.pl /tools/bin/neoplasmer -X .cache -i /data/mondo.owl -i /data/neoplasm-core.owl "$@"

