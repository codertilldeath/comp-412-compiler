EXE = sbcl
FLAGS = --no-userinit --no-sysinit --non-interactive

all: quicklisp
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/setup.lisp \
		--load ./src/412fe-superspeed.asd \
		--eval '(ql:quickload :alexandria)' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(asdf:load-system :412fe-superspeed)' \
		--eval '(asdf:make :412fe-superspeed)'

clean:
	rm -r build
	rm -r **/*.fasl

quicklisp: build/quicklisp/setup.lisp

build/quicklisp/setup.lisp:
	mkdir build
	mkdir build/quicklisp
	curl -o build/quicklisp/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "./build/quicklisp")' \
		--eval '(quit)'
