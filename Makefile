all: quicklisp
	sbcl --load ./superspeed.asd \
	     --load ./quicklisp/quicklisp.lisp \
	     --eval '(quicklisp-quickstart:install :path "./quicklisp")' \
	     --eval '(ql:quickload :alexandria)' \
	     --eval '(asdf:disable-output-translations)' \
	     --eval '(asdf:load-system :412fe)' \
	     --eval '(asdf:make :412fe)'
