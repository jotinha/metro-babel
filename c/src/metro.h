typedef unsigned int uint;
typedef unsigned long int ulong;

typedef struct Config {
  uint N;
  double * pos;
  double * box;
} Config;

typedef struct Simulation {
  
  double temp;
  double maxd;
  double ener;
  double virial;

} Simulation;