CFLAGS= -O3
LIBS  = -lm
LIBS += -ldl    #If on OpenBSD, comment this out.

SRC= k.c
HDR= h.h
OBJ= $(SRC:.c=.o)

all: k

k k_test: $(OBJ) $(HDR)
	$(CC) $(LIBS) $(CFLAGS) $< -o $@

test: CFLAGS= -O1 -g3 -DNDEBUG
test: k_test

clean:
	rm -f k k_test *.o

.PHONY: all test clean