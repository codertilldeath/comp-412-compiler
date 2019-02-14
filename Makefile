EXE = sbcl
FLAGS = --no-userinit --no-sysinit --non-interactive

BUILDDIR = ./build
QL = $(BUILDDIR)/quicklisp
LIBS= $(BUILDDIR)/libs.stamp

.PHONY: all clean cleanall
all: $(BUILDDIR)/412fealloc

$(BUILDDIR)/412fealloc: $(LIBS)
	$(EXE) $(FLAGS) \
		--load $(QL)/setup.lisp \
		--load ./src/412fe-superspeed.asd \
		--eval '(ql:quickload :alexandria)' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(asdf:load-system :412fe-superspeed)' \
		--eval '(asdf:make :412fe-superspeed)' \
		--eval '(quit)'

# Make executable with full debug support and slower execution time
$(BUILDDIR)/debug: $(LIBS)
	$(EXE) $(FLAGS) \
		--load $(QL)/setup.lisp \
		--load ./src/412fe.asd \
		--eval '(ql:quickload :alexandria)' \
		--eval '(asdf:disable-output-translations)' \
		--eval '(asdf:load-system :412fe)' \
		--eval '(asdf:make :412fe)' \
		--eval '(quit)'


# Remove binaries and fasl compiled files
clean:
	rm -f scheduler
	find . -name "*.fasl" -type f -delete

# Remove everything, including libraries
cleanall: clean
	rm -rf build

$(LIBS): $(QL)/setup.lisp
	$(EXE) $(FLAGS) \
		--load $(QL)/setup.lisp \
		--eval '(ql:quickload :alexandria)' \
		--eval '(quit)'
	touch $@

$(QL)/quicklisp.lisp:
	mkdir build
	mkdir build/quicklisp
	wget -P build/quicklisp/ http://beta.quicklisp.org/quicklisp.lisp

$(QL)/setup.lisp: $(QL)/quicklisp.lisp
	$(EXE) $(FLAGS) \
		--load $(QL)/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")' \
		--eval '(quit)'
