# crawbar_tests
https://github.com/stedolan/crowbar


## commands
> dune build
> afl-fuzz -i input -o output -- ./_build/default/bin/main.exe @@

### afl man
> afl-fuzz --help

### see crashes/hang files
> xxd output/crashes/id\:00000X*

### run a specific test
> ./_build/default/bin/main.exe output/crashes/id\:00000X*

### minimize result
> afl-tmin

> afl-tmin -i output/crashes/id\:000000* -o FILENAME-FOR-OUTPUT -- ./_build/default/bin/main.exe @@
doing test only with the error
> ./_build/default/bin/main.exe FILENAME-FOR-OUTPUT

### other
How many place does the result take
> du -sh output/crashes/