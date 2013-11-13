#include <math.h>
#include "metro.h"
#include "core.h"
#include "random.h"

void mcstep(uint niters) {

  uint i;

  for (i = 0; i < niters; i++) {

  }

}

void op_disp() {


}


int accept(double arg) {

  return (arg >= 0.0) || exp(arg) >= random();

}