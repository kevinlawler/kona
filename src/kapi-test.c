#include <stdio.h>
#include <string.h>
#include <math.h>
#include "kona.h"


Z int pass, fail;

extern K KTREE;
extern K X(S);
extern K show(K);
extern I attend();
extern void boilerplate();

#define tst(e)	if(e){pass++;}else{fprintf(stderr, "Failed:%s\n", #e); fail++;}

#define TEST(i,x,f) do { {i;} tst(x); {f;}} while(0)

int
main(int argc, char** argv)
{
	F pi = atan(1.0)*4;
	K a = gi(2);
	K b = gi(3);
	K c = gi(4);
	K* v;

	cd(ksk("",0));

	tst(Ki(a)==2);
	tst(Ki(b) + 1 == Ki(c));
	cd(a); cd(b); cd(c);

	b = gf(1.0); c = gf(2);
	tst(Kf(b) + 1 == Kf(c));
	cd(b); cd(c);

	a = gs(sp("foo"));
	b = ksk("`foo", 0);
	tst(Ks(a) == Ks(b));
	cd(a); cd(b);

	a = ksk("2 + 3", 0);
	tst(Ki(a) == 5);
	cd(a);

	a = ksk("_ci 65", 0);
	tst(Kc(a) == 'A');

	// XXX this should return type 1 uniform vector
	a=gnk(3,gi(11),gi(22),gi(33));
	tst(a->t == 0);

	v = (K*)a->k;
	tst(Ki(v[0])+Ki(v[1])==Ki(v[2]));
	cd(a);


	{
	b = gsk("pi",gf(pi));
	kap(&KTREE, &b);
	a = X(".pi");
	tst(Kf(a) == pi);
	cd(a);
	}

	{
	K dir = gtn(5,0);
	K t;
	t = gsk("x",gi(1)); kap(&dir, &t);
	t = gsk("y",gi(2)); kap(&dir, &t);
	t = gsk("z",dir); kap(&KTREE, &t);
	a = X(".z.x");
	tst(Ki(a) == 1);
	cd(a);
	a = X(".z.y");
	tst(Ki(a) == 2);
	cd(a);
	}

	{
	I i;
	K d = gtn(5,0);
	K c0 = gtn(0,0);
	K c1 = gtn(-1,0);
	K t0, t1, e;
	t0 = gsk("a", c0); kap(&d,&t0);
	t1 = gsk("b", c1); kap(&d,&t1);
	e = gp("hello1"); kap(&c0,&e);
	e = gp("hello2"); kap(&c0,&e);
	KK(KK(d)[0])[1] = c0;
	i = 1; kap(&KK(KK(d)[1])[1], &i);
	i = 2; kap(&KK(KK(d)[1])[1], &i);
	//i = 1; kap(&c1, &i);
	//i = 2; kap(&c1, &i);
	//KK(KK(d)[1])[1] = c1;
	show(d);
	}


	//b = ksk("+/", a);
	//tst(Ki(b) == 66);

	//argc--;argv++;
	//DO(i, argc, {a=ksk(argv[i], 0);

	//ksk("`0:,/$!10;`0:,\"\n\"", 0);

	fprintf(stderr, "Pass:%4d, fail:%4d\n", pass, fail);
	if (argc > 1 && strcmp(argv[1], "-i") == 0) {
		boilerplate();
		attend();
	}
}
