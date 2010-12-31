LIBS=	-lm

#If on OpenBSD, comment this out.
LIBS+=	-ldl

kona: k.c
	${CC} ${LIBS} -O3 k.c -o $@

kona_test: k.c
	${CC} ${LIBS} -O3 -DNDEBUG k.c -o $@

clean:
	rm -f kona kona_test
