all: 
	sbcl --load superspeed.asd --eval '(asdf:load-system :412fe)' --eval '(asdf:make :412fe)'
