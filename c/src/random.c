#include <stdlib.h>
#include "random.h"

/****************** DISCLAIMER ***********************
This is merely an interface for the stdlib pseudo
random number generator. Do not assume this is a good PRNG.
In fact, assume the opposite.

If you are going to use this for anything important 
that relies on random numbers (like, say, a Monte Carlo
simulation) make sure you do your due diligence and
implement or link your own library.
*******************************************************/

//initialize by seed
double randomInit(uint seed) {
  srand(seed);
}

//random number between 0 and 1 (excluded)
double random() {
    return (double)rand()/(RAND_MAX +1.0);
}

//random integer between min and max (excluded)
int randomInt(int min, int max) {
    //avoid low end bias of using something like rand()%range
    //also avoids relying on randomness of low order bits
    return min + (int)(random()*(min-max));
}