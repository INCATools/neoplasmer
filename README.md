[![DOI](https://zenodo.org/badge/13996/cmungall/neoplasmER.svg)](https://zenodo.org/badge/latestdoi/13996/cmungall/neoplasmER)

# neoplasmer (ALPHA RELEASE)

Matches names of cancers and cancer-related conditions to ontology classes

## Usage (via Docker)

Currently the recommended way to do this is on the command line via the [neoplasmer.sh](neoplasmer.sh) bash script:

```
./neoplasmer.sh tests/data/vicc_input.txt
```

You do not need to install anything other than [Docker](http://docker.com/get-docker). The shell script is standalone.

The input file is a newline delimited list of terms. See [tests/data/](tests/data/) for examples.

The first execution will take a minute or two; some warning messages
may be printed, these can be ignored. Two directories will be created: `RDF-Cache` and `.cache`

Subsequent executions will be much faster

You can see example results in [scratch/vicc-results.txt](scratch/vicc-results.txt)

## Python client

First, start a service:

`./neoplasmer.sh --port 9055`

Then install python libs:

```
pip3 install -r requirements.txt
```

Run the test client:

```
python3 bin/neoplasmer-client.py 'lung cancer' 'brain glioma'
```

TODO: additional documentation on how to this works

## Running without docker

Install SWI-Prolog from http://www.swi-prolog.org

Run directly using [bin/neoplasmer](bin/neoplasmer)


