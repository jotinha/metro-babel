#include "metro.h"
#include "random.h"
#include "montecarlo.h"

struct Files{
  FILE * eners;
} FILES;

void init() {

  //initialize pseudo random number generator
  randomInit(123);

  //open files
  FILES.eners = fopen('eners.dat','wt');

}

void end() {

  fclose(FILES.eners);

}

void readInput(Config * conf, Simulation * sim,
           ulong * niters_eq,ulong * step_eq,
           ulong * niters, ulong * step) {

  fprintf('Temperature: ');
  fscanf('%f',&sim->temp);

  fprintf('Number of iterations for equilibration: ');
  fscanf('%i',niters_eq);

  fprintf('Equilibration step: ');
  fscanf('%i',step_eq);

  fprintf('Number of iterations for run: ');
  fscanf('%i',niters);

  fprint('Sampling step: ');
  fscanf('%i',step);

}

void run(int isEquilibration, ulong niters,ulong step, Config * conf, Simulation * sim) {

	ulong i;
	double acc;
	sim->ener = calcEner(conf);

	for (i=0; i < niters; i+=step) {
		acc = mcstep(conf,sim);

		if (isEquilibration) { //adjust parameter
			sim->maxd *= ( acc >= 0.5 ? 1.05 : 0.95 );
		}

    if (!isEquilibration) sample(conf,sim);

    if (isEquilibration) {
      printf('%8i | %14.6f | %14.6f\n',i,sim->maxd,sim->ener);
    } else {
      printf('%8i | %14.6f\n',i,sim->ener);
    }

	}

}

void sample(Config * conf, Simulation * sim) {

  fprintf(FILES.eners,'%f',sim->ener);

}

void main() {

  Config theconf;
  Simulation thesim;
  Config * conf = &(theconf);
  Simulation * sim = &(thesim);
  ulong niters_eq, niters, step_eq, step;

  init();

  readInput(conf,sim,&niters_eq,&step_eq,&niters,&step);

  //equilibration
  run(true,niters_eq, step_eq,conf,sim);
  //run
  run(false,niters, step,conf,sim);

  end();

}
