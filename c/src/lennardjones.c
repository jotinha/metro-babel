#include <math.h>
#include "lennardjones.h"

double ljener_r2(double r2, double eps, double sigma, int doVirial, double * virial) {

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