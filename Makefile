.PHONY: all test run clean

all: bytes-internal-test bytes-test challenges

test: bytes-internal-test bytes-test
	./bytes-internal-test && ./bytes-test

run: challenges
	./challenges

clean:
	rm -f *.o *.import.scm *-test challenges

%.o: %.scm
	csc -c $*.scm

bytes-internal-test: bytes-internal-test.o bytes-internal.o
	csc $^ -o $@

bytes-test: bytes-test.o bytes.o bytes-internal.o
	csc $^ -o $@

challenges: challenges.o bytes.o bytes-internal.o crypto.o
	csc $^ -o $@


# Ugnh, why is there no tool to auto-generate these :(

bytes.o: bytes-internal.o
bytes-test.o: bytes.o
bytes-internal-test.o: bytes-internal.o
crypto.o: bytes.o
challenges.o: bytes.o crypto.o
