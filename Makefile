EXE = sbcl
FLAGS = --no-userinit --no-sysinit --non-interactive

BUILDDIR = ./build
QL = $(BUILDDIR)/quicklisp
QLSOFT = $(QL)/dists/quicklisp/software

all: binary

binary: $(BUILDDIR)/debug $(BUILDDIR)/412fe

# Remove binaries and fasl compiled files
clean:
	rm build/debug
	rm build/412fe
	find . -name "*.fasl" -type f -delete

# Remove everything, including libraries
cleanall: clean
	rm -r build

alexandria: $(QLSOFT)/alexandria-20170830-git

quicklisp: $(QL)/setup.lisp

# Make executable with full debug support and slower execution time
$(BUILDDIR)/debug: alexandria
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/setup.lisp \
		--load ./src/412fe.asd \
		--eval '(ql:quickload :alexandria)' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(asdf:load-system :412fe)' \
		--eval '(asdf:make :412fe)' \
		--eval '(quit)'

$(BUILDDIR)/412fe: alexandria
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/setup.lisp \
		--load ./src/412fe-superspeed.asd \
		--eval '(ql:quickload :alexandria)' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(asdf:load-system :412fe-superspeed)' \
		--eval '(asdf:make :412fe-superspeed)' \
		--eval '(quit)'

$(QLSOFT)/alexandria-20170830-git: quicklisp
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/setup.lisp \
		--eval '(ql:quickload :alexandria)' \
		--eval '(quit)'

$(QL)/quicklisp.lisp:
	mkdir build
	mkdir build/quicklisp
	wget -P build/quicklisp/ http://beta.quicklisp.org/quicklisp.lisp

$(QL)/setup.lisp: $(QL)/quicklisp.lisp
	$(EXE) $(FLAGS) \
		--load ./build/quicklisp/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "./build/quicklisp")' \
		--eval '(quit)'
