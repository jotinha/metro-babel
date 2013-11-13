#include <math.h>
#include "core.h"
#include "config.h"
double calcEner_r2(double r2, double eps, double sigma, int doVirial, double * virial) {


double sr2,sr4,sr6,sr12;

  sr2 = sigma*sigma/r2;
  sr4 = sr2*sr2;
  sr6 = sr4*sr2;
  sr12 = sr6*sr6;

  if (doVirial) {
    *virial = 4.0*eps*(12.0*sr12 - 6.0*sr6);
  }

  return 4.0*eps*(sr12-sr6);

}

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