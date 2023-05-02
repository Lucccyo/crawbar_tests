afl-tmin -i output/crashes/id\:000000* -o FILENAME-FOR-OUTPUT -- ./_build/default/bin/main.exe @
sleep 2
./_build/default/bin/main.exe FILENAME-FOR-OUTPUT

