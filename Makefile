LIBS=	-lm

#If on OpenBSD, comment this out.
LIBS+=	-ldl

k: k.c
	${CC} ${LIBS} -O3 k.c -o $@

k_test: k.c
	${CC} ${LIBS} -DNDEBUG k.c -o $@

clean:
	rm -f k k_test
