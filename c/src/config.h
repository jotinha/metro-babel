#include "metro.h"

typedef struct Config {
  uint N;
  double * pos;
  double * box;
  double eps, sigma;
  double temp;
  double maxd;
} Config;

