CFLAGS= -O3
LIBS  = -lm

include config.mk

SRC= c.c getline.c mt.c p.c r.c v.c 0.c
HDR= h.h incs.h ts.h
OBJ= $(SRC:.c=.o)

all: k

k: k.c $(OBJ)
	$(CC) $(LIBS) $(CFLAGS) $< -o $@ ${OBJ}

k_test: k.c $(OBJ) $(HDR) tests.o
	$(CC) $(LIBS) $(CFLAGS) $< -o $@ ${OBJ} tests.o

test: CFLAGS= -O0 -g3 -DNDEBUG
test: k_test

*.o: ${HDR}

k.c: ${SRC}
0.o: 0.c
c.o: c.c
getline.o: getline.c
mt.o: mt.c
p.o: p.c
r.o: r.c mt.c 0.c
v.o: v.c

tests.o: tests.c $(SRC) $(HDR)

clean:
	rm -f k k_test *.o

TAGS: *.c *.h
	etags *.[ch]

.PHONY: all test clean
