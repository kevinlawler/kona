CFLAGS= -O3
LIBS  = -lm
LIBS += -ldl    #If on OpenBSD, comment this out.

SRC= k.c c.c getline.c mt.c p.c r.c tests.c v.c
HDR= h.h
OBJ= k.o        # $(SRC:.c=.o)

all: k

k: $(OBJ)
	$(CC) $(LIBS) $(CFLAGS) $< -o $@

k_test: $(SRC) $(HDR)
	$(CC) $(LIBS) $(CFLAGS) $< -o $@

test: CFLAGS= -DNDEBUG
test: k_test

$(OBJ): $(SRC) $(HDR)

clean:
	rm -f k k_test *.o

.PHONY: all test clean
