CC=gcc
PREFIX = /usr/local
CFLAGS=-g
PRODFLAGS = -O3 #-pg -g3
LIB=libkona.a
DEVFLAGS = -O0 -g3 -DDEBUG -Wunused -Wreturn-type -Wimplicit-int #-Wall

OS := $(shell uname -s | tr "[:upper:]" "[:lower:]")

ifeq (mingw32_nt-6.2,$(OS))
LDFLAGS = -lws2_32 -static -lpthread
OBJS= src/win/mman.o src/win/dlfcn.o src/0.o src/c.o src/getline.o src/mt.o src/p.o \
      src/r.o src/k.o src/kc.o src/kx.o src/kg.o src/km.o src/kn.o src/ko.o \
      src/ks.o src/v.o src/va.o src/vc.o src/vd.o src/vf.o src/vg.o src/vq.o
else
LDFLAGS = -lm
OBJS= src/0.o src/c.o src/getline.o src/mt.o src/p.o src/r.o \
      src/k.o src/kc.o src/kx.o src/kg.o src/km.o src/kn.o src/ko.o src/ks.o \
      src/v.o src/va.o src/vc.o src/vd.o src/vf.o src/vg.o src/vq.o
endif

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

lib: $(LIB) 

$(LIB): $(OBJS) src/kapi.o
	$(AR) crv $@ $(OBJS) src/kapi.o

kapi-test: src/kapi-test.o $(LIB)
	$(CC) ${CFLAGS} $^ -o $@ -L. -lkona $(LDFLAGS)

k: CFLAGS += $(PRODFLAGS)
k: $(OBJS) src/main.o
	$(CC) ${CFLAGS} $^ -o $@ $(LDFLAGS)

k_test: CFLAGS += $(DEVFLAGS)
k_test: $(OBJS_T) src/main.t.o src/tests.t.o
	$(CC) ${CFLAGS} $^ -o $@ $(LDFLAGS)

k_dyn: CFLAGS += $(PRODFLAGS)
k_dyn: $(OBJS)
	$(CC) ${CFLAGS} $^ -rdynamic -o $@ $(LDFLAGS)

test: k_test

install:
	install k $(PREFIX)/bin/k

clean:
	$(RM) -r k k_test *.exe k.dSYM k_test.dSYM src/*.o src/win/*.o

TAGS: *.c *.h
	etags *.[ch]

%.t.o: %.c
	$(CC) $(CFLAGS) -c $(CPPFLAGS) -o $@ $<

.PHONY: all clean install

# Dependencies.
ifeq (mingw32_nt-6.2,$(OS))
src/win/dlfcn.c: src/win/dlfcn.h
src/win/mman.c: src/win/mman.h
src/*.o: src/incs.h src/ts.h Makefile src/k.h src/win/mman.h src/win/dlfcn.h
else
src/*.o: src/incs.h src/ts.h Makefile src/k.h
endif
src/0.c: src/0.h src/km.h src/v.h src/vf.h
src/c.c: src/c.h
src/getline.c: src/0.h src/getline.h
src/k.c: src/r.h src/kc.h src/kx.h src/kg.h src/km.h src/kn.h src/ko.h src/ks.h \
         src/tests.h src/v.h src/va.h src/vc.h src/vd.h src/vf.h src/vg.h src/vq.h
src/kc.c: src/kc.h
src/kx.c: src/kx.h src/km.h
src/kg.c: src/kg.h src/km.h
src/km.c: src/km.h
src/kn.c: src/kn.h
src/ko.c: src/km.h src/ko.h
src/ks.c: src/ks.h

src/p.c: src/km.h src/p.h src/v.h src/vf.h
src/r.c: src/r.h src/va.h src/vf.h src/vg.h
src/tests.c: src/tests.h
src/v.c: src/scalar.h src/km.h src/0.h src/v.h
src/va.c: src/scalar.h src/r.h src/vc.h
src/vc.c: src/scalar.h src/km.h src/ko.h src/vc.h
src/vd.c: src/km.h src/p.h src/r.h src/v.h src/vd.h
src/vf.c: src/km.h src/vf.h
src/vg.c: src/kg.h src/km.h src/v.h src/vc.h
src/vq.c: src/r.h src/v.h src/vq.h
src/kapi.c: src/kona.h

# DO NOT DELETE
