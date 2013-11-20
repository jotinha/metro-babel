#include <math.h>
#include "metro.h"
#include "core.h"
#include "random.h"
#include "forcefield.h"

double mcstep(ulong niters, Config * conf, Simulation * sim) {

  ulong i,acc = 0;

  for (i = 0; i < niters*conf->N; i++) {
    
    acc += op_disp(conf,sim);

  }

  return double(acc)/double(niters)/double(conf->N);  //what about very large numbers?

}

int op_disp(Config * conf, Simulation * sim) {
  
  double eo, en, de, dx, dy, dz;
  int i, acc;

  i = randomInt(0,conf->N);

  eo = calcEner_i(conf,i);

  dx = (2.0*random() - 1.0)*conf->maxd;
  dy = (2.0*random() - 1.0)*conf->maxd;
  dz = (2.0*random() - 1.0)*conf->maxd;
  
  displaceParticle(conf, i, dx,dy,dz);
      
  en = calcEner_i(conf,i);
  de = en - eo;

  if (accepts(-de/sim->temp)) {

    acc = 1;
    sim->ener += de;

  } else {

    acc = 0;
    displaceAtom(conf, i, dx,dy,dz);

  }
}


int accepts(double arg) {

  return (arg >= 0.0) || exp(arg) >= random();

}