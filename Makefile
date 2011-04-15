PREFIX = /usr/local
LDFLAGS = -lm
PRODFLAGS = -O3 #-pg -g3
DEVFLAGS = -O3 -g3 -DDEBUG -Wunused -Wreturn-type -Wimplicit-int -Wall

OS := $(shell uname -s | tr "[:upper:]" "[:lower:]")

OBJS= 0.o c.o getline.o mt.o p.o r.o \
      k.o kc.o kex.o kg.o km.o kn.o ko.o ks.o \
      v.o va.o vc.o vd.o vf.o vg.o vq.o

# k_test versions of OBJS
OBJS_T= $(shell echo ${OBJS} | sed -e "s/\.o/.t.o/g")

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
	$(CC) ${CFLAGS} $(LDFLAGS) $^ -o $@

k_test: CFLAGS += $(DEVFLAGS)
k_test: $(OBJS_T) tests.t.o
	$(CC) ${CFLAGS} $(LDFLAGS) $^ -o $@

test: k_test

install:
	install k $(PREFIX)/bin/k

clean:
	$(RM) -r k k_test k.dSYM k_test.dSYM *.o

TAGS: *.c *.h
	etags *.[ch]

%.t.o: %.c
	$(CC) $(CFLAGS) -c $(CPPFLAGS) -o $@ $<

.PHONY: all clean install

# Dependencies.
*.o: incs.h ts.h Makefile k.h
0.c: 0.h km.h v.h vf.h
c.c: c.h
getline.c: 0.h getline.h
k.c: r.h kc.h kex.h kg.h km.h kn.h ko.h ks.h tests.h v.h va.h vc.h vd.h vf.h vg.h vq.h
kc.c: kc.h
kex.c: kex.h km.h
kg.c: kg.h km.h
km.c: km.h
kn.c: kn.h
ko.c: km.h ko.h
ks.c: ks.h
p.c: km.h p.h v.h vf.h
r.c: r.h va.h vf.h vg.h
tests.c: tests.h
v.c: scalar.h km.h 0.h v.h
va.c: scalar.h r.h vc.h
vc.c: scalar.h km.h ko.h vc.h
vd.c: km.h p.h r.h v.h vd.h
vf.c: km.h vf.h
vg.c: kg.h km.h v.h vc.h
vq.c: r.h v.h vq.h

# DO NOT DELETE
