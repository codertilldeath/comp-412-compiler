all: 
	sbcl --load 412fe.asd --eval '(asdf:load-system :412fe)' --eval '(asdf:make :412fe)'
