PREFIX = /usr/local
LDFLAGS = -lm
PRODFLAGS = -O3
DEVFLAGS = -O0 -g3 -DDEBUG

OS := $(shell uname -s | tr "[:upper:]" "[:lower:]")

ifeq (linux,$(OS))
	LDFLAGS += -ldl
endif
ifeq (freebsd,$(OS))
endif
ifeq (openbsd,$(OS))
endif
ifeq (darwin,$(OS))
  PRODFLAGS += -fast
endif
ifeq (sunos,$(OS))
	LDFLAGS += -lsocket
  PRODFLAGS += -fast
endif

all: k k_test

k: CFLAGS += $(PRODFLAGS)
k: k.o c.o getline.o mt.o p.o r.o v.o 0.o

k_test: CFLAGS += $(DEVFLAGS)
k_test: k.t.o c.t.o getline.t.o mt.t.o p.t.o r.t.o v.t.o 0.t.o tests.t.o
	$(CC) $(LOADLIBES) $(LDFLAGS) $^ -o $@
test: k_test

# Dependencies.
k.o c.o getline.o mt.o p.o r.o v.o 0.o k.t.o c.t.o getline.t.o mt.t.o p.t.o r.t.o v.t.o 0.t.o tests.t.o: incs.h h.h ts.h Makefile

install:
	install k $(PREFIX)/bin/k

clean:
	$(RM) -r k k_test k.dSYM k_test.dSYM *.o

TAGS: *.c *.h
	etags *.[ch]

%.t.o: %.c
	$(CC) $(CFLAGS) -c $(CPPFLAGS) -o $@ $<

.PHONY: all clean install
