#include "core.h"
#include "config.h"
#include "lennardjones.h"

#define calcEner_r2 ljener_r2

double calcEner(Config * conf) {

  int i,j;
  double *p1,*p2;
  double eps = conf->eps, sigma = conf->sigma;
  int N = conf->N;
  double r2;
  double ener = 0, virial = 0, virialij;

  for (i=0, p1 = &(conf->pos[i]); i < N-1; i++, p1 += 3) {
    
    for (j=i, p2 = &(conf->pos[j]); j < N; j++, p2 += 3) {
      
      r2 = minimg_r2(p1, p2, conf->box);
      
      ener += calcEner_r2(r2,eps,sigma,1,&virialij);
      virial += virialij;

    }

  }

}

double calcEner_i(Config * conf, uint i) {
  
  int j;
  double *p1,*p2;
  double eps = conf->eps, sigma = conf->sigma;
  int N = conf->N;
  double ener = 0, virial = 0, virialij;
  double r2;

  p1 = &(conf->pos[i]);

  for (j=i, p2 = &(conf->pos[j]); j < N; j++, p2 += 3) {
    
    r2 = minimg_r2(p1, p2, conf->box);
    
    ener += calcEner_r2(r2,eps,sigma,1,&virialij);
    virial += virialij;

  }

}