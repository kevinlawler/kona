PREFIX = /usr/local
LDFLAGS = -lm
PRODFLAGS = -O3
DEVFLAGS = -O3 -g3 -DDEBUG

OS := $(shell uname -s | tr "[:upper:]" "[:lower:]")

OBJS= 0.o k.o c.o getline.o mt.o p.o r.o \
      v.o va.o vc.o vd.o vf.o vg.o vq.o

# k_test versions of OBJS
OBJS_T= $(shell echo ${OBJS} | sed -e "s/.o/.t.o/g")

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
k: $(OBJS)

k_test: CFLAGS += $(DEVFLAGS)
k_test: $(OBJS_T) tests.t.o
	$(CC) $(LOADLIBES) $(LDFLAGS) $^ -o $@

test: k_test

# Dependencies.
*.o: incs.h ts.h Makefile

install:
	install k $(PREFIX)/bin/k

clean:
	$(RM) -r k k_test k.dSYM k_test.dSYM *.o

TAGS: *.c *.h
	etags *.[ch]

%.t.o: %.c
	$(CC) $(CFLAGS) -c $(CPPFLAGS) -o $@ $<

.PHONY: all clean install
# DO NOT DELETE
