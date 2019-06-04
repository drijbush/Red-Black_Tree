PROG =	rb

SRCS =	numbers.f90 RBtree.f90 test.f90

OBJS =	numbers.o RBtree.o test.o

LIBS =	

CC = gcc
CFLAGS = -O
FC = gfortran
FFLAGS = -std=f95 -Wall -Wextra -pedantic -fcheck=all -g -fprofile-arcs -ftest-coverage 
F90 = gfortran
F90FLAGS = -std=f95 -Wall -Wextra -pedantic -fcheck=all -g -fprofile-arcs -ftest-coverage 
LDFLAGS = -g -fprofile-arcs -ftest-coverage 

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

RBtree.o: numbers.o
test.o: RBtree.o numbers.o
