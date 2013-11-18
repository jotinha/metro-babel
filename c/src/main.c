#include "metro.h"
#include "random.h"
#include "montecarlo.h"

void init() {

  //initialize pseudo random number generator
  randomInit(123);

}

void load(Config * conf, Simulation * sim) {

}

void run(int isEquilibration, ulong niters,ulong step, Config * conf, Simulation * sim) {

	ulong i;
	double acc;
	sim->ener = calcEner(conf);

	for (i=0; i < niters; i+=step) {
		acc = mcstep(conf,sim);

		sample(conf,sim);

		if (isEquilibration) { //adjust parameter
			sim->maxd *= ( acc >= 0.5 ? 1.05 : 0.95 );
		}

	}

}

void sample(Config * conf, Simulation * sim) {

}

void main() {

  Config conf;
  Simulation sim;

  init();

  load(conf,sim,);

  run(true,sim->niters_eq, )

}
