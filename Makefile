# Generates the executable. Requires 'raco' in your path.

BIN_NAME=whitespace

bin:
	raco exe -o $(BIN_NAME) main.rkt

clean:
	rm -f $(BIN_NAME)
	rm -rf compiled
	sed -i "s/[ \t]*$$//" *.rkt
