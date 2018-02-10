#include "incs.h"

//abridged. License BSD-style Takuji Nishimura & Makoto Matsumoto http://www.math.hiroshima-u.ac.jp/~m-mat/MT/emt.html
//Before using, initialize the state by using init_genrand64(seed)
//mt19937-64.c

#define NN 312
#define MM 156
#define MATRIX_A 0xB5026F5AA96619E9ULL
#define UM 0xFFFFFFFF80000000ULL // Most significant 33 bits
#define LM 0x7FFFFFFFULL // Least significant 31 bits


static unsigned long long mt[NN]; // The array for the state vector
static int mti=NN+1; // mti==NN+1 means mt[NN] is not initialized

void init_genrand64(unsigned long long seed) // initializes mt[NN] with a seed
{
    mt[0] = seed;
    for (mti=1; mti<NN; mti++)
        mt[mti] =  (6364136223846793005ULL * (mt[mti-1] ^ (mt[mti-1] >> 62)) + mti);
}

// generates a random number on [0, 2^64-1]-interval
unsigned long long genrand64_int64(void)
{
    int i;
    unsigned long long x;
    static unsigned long long mag01[2]={0ULL, MATRIX_A};

    if (mti >= NN) { // generate NN words at one time

        // if init_genrand64() has not been called,
        // a default initial seed is used
        if (mti == NN+1)
            init_genrand64(5489ULL);

        for (i=0;i<NN-MM;i++) {
            x = (mt[i]&UM)|(mt[i+1]&LM);
            mt[i] = mt[i+MM] ^ (x>>1) ^ mag01[(int)(x&1ULL)];
        }
        for (;i<NN-1;i++) {
            x = (mt[i]&UM)|(mt[i+1]&LM);
            mt[i] = mt[i+(MM-NN)] ^ (x>>1) ^ mag01[(int)(x&1ULL)];
        }
        x = (mt[NN-1]&UM)|(mt[0]&LM);
        mt[NN-1] = mt[MM-1] ^ (x>>1) ^ mag01[(int)(x&1ULL)];

        mti = 0;
    }

    x = mt[mti++];

    x ^= (x >> 29) & 0x5555555555555555ULL;
    x ^= (x << 17) & 0x71D67FFFEDA60000ULL;
    x ^= (x << 37) & 0xFFF7EEE000000000ULL;
    x ^= (x >> 43);

    return x;
}

// generates a random number on [0,1)-real-interval, 53-bit precision
double genrand64_real2(void) { return (genrand64_int64() >> 11) * (1.0/9007199254740992.0); }



