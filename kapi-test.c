#include <stdio.h>
#include "kona.h"


Z int pass, fail;

#define tst(e)	if(e){pass++;}else{fprintf(stderr, "Failed:%s\n", #e); fail++;}

int
main(int argc, char** argv)
{
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

	// XXX this should return type 1 uniform vector
	a=gnk(3,gi(11),gi(22),gi(33));
	tst(a->t == 0);
	v = (K*)a->k;

	fprintf(stderr, "%p\n", KK(a));
	tst(Ki(v[0])+Ki(v[1])==Ki(v[2]));

	ksk("2 + 3", 0);
	//ksk("`0:,/$!10;`0:,\"\n\"", 0);
	//if (argc > 1 && strcmp(argv[1], "-i") == 0) {
		//boilerplate();
		//attend();
	//}
	fprintf(stderr, "Pass:%4d, fail:%4d\n", pass, fail);
}
