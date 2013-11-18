#include "metro.h"

typedef struct Config {
  uint N;
  double * pos;
  double * box;
  double eps, sigma;
} Config;

void displaceParticle(Config * conf, uint i, double dx, double dy, double dz);

