#include "metro.h"
#include "random.h"
#include "montecarlo.h"

struct Files{
  FILE * eners;
} FILES;

enum ESTRUCTURE {
  ESLIQ = 0,
  ESFCC = 1,
  ESBCC = 2,
  ESHCP = 3,
}


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
           ulong * niters, ulong * step, ESTRUCTURE * iniConfig) {

  int inic

  double * b = conf->box;

  printf('Temperature: ');
  scanf('%f',&sim->temp);

  printf('Number of iterations for equilibration: ');
  scanf('%i',niters_eq);

  printf('Equilibration step: ');
  scanf('%i',step_eq);

  printf('Number of iterations for run: ');
  scanf('%i',niters);

  print('Sampling step: ');
  scanf('%i',step);

  printf('Number of particles: ');
  scanf('%i',&conf->N)

  printf('Box size (in units of sigma):\n');
  printf('x: '); scanf('%f', b);
  printf('y: '); scanf('%f', b++);
  printf('z: '); scanf('%f', b++);

  printf('Initial configuration ([0] liq, [1] fcc, [2] bcc, [3] hcp) : ');
  do {
    scanf('%i',&inic);
  } while(inic >= 0 && inic <= 3);
  *iniConfig = inic;  
  
  // switch (inic) {
  //   case 0:
  //     *iniConfig = ESLIQ;
  //     break;
  //   case 1:
  //     *iniConfig = ESFCC,
  //     break;
  //   case 2:
  //     *iniConfig = ESBCC,
  //     break;
  //   case 3:
  //     *iniConfig = ESHCP.
  //     break;
  //   default:


  // }



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
  ESTRUCTURE iniConfig;

  init();

  readInput(conf,sim,&niters_eq,&step_eq,&niters,&step,&iniConfig);

  initStructure(conf,iniConfig);

  //equilibration
  run(true,niters_eq, step_eq,conf,sim);
  //run
  run(false,niters, step,conf,sim);

  end();

}
