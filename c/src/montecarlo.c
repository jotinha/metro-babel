#include <math.h>
#include "metro.h"
#include "core.h"
#include "random.h"

void mcstep(uint niters) {

  uint i;

  for (i = 0; i < niters; i++) {

  }

}

void op_disp(Config * conf) {
  
  double eo, en, de, dx, dy, dz;
  int i;

  i = randomInt(0,conf->N);

  eo = calcEner_i(conf,i);

  displaceAtom(conf, (2.0*random() - 1.0)*conf->maxd,
                     (2.0*random() - 1.0)*conf->maxd,
                     (2.0*random() - 1.0)*conf->maxd)
    
  en = calcEner_i(conf,i);

  if (accept(-(en-eo)/conf->temp)) {
    
  }
}


int accept(double arg) {

  return (arg >= 0.0) || exp(arg) >= random();

}