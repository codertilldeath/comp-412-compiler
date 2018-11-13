# Makefile for the COMP 412, Lab 3, code checking tools
CFLAGS=-Wall -O2

tools:	CheckResults SchedOne 

CheckResults:	CheckResults.c
	gcc $(CFLAGS) CheckResults.c
	mv a.out CheckResults

SchedOne:	SchedOne.c
	gcc $(CFLAGS) SchedOne.c
	mv a.out SchedOne

export:	Makefile README SchedOne.c CheckResults.c SchedAndRunAll SchedHelper
	tar cvf export.tar Makefile README *.c SchedAndRunAll SchedHelper\
	        RunAll RunAllHelper 

clean:	
	rm -i *~ CheckResults SchedOne 
