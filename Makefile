EXE = sbcl
FLAGS = --no-userinit --no-sysinit --non-interactive

all: quicklisp binary

binary: build/412fe

# Remove binaries and fasl compiled files
clean:
	rm build/412fe
	find . -name "*.fasl" -type f -delete

# Remove everything, including libraries
cleanall: clean
	rm -r build

quicklisp: build/quicklisp/setup.lisp

build/412fe:
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/setup.lisp \
		--load ./src/412fe-superspeed.asd \
		--eval '(ql:quickload :alexandria)' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(asdf:load-system :412fe-superspeed)' \
		--eval '(asdf:make :412fe-superspeed)'

build/quicklisp/setup.lisp:
	mkdir build
	mkdir build/quicklisp
	curl -o build/quicklisp/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "./build/quicklisp")' \
		--eval '(quit)'
