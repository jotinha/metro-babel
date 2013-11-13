#include <math.h>
#include <stdlib.h>
#include "core.h"

double minimg_r2(double * p1, double * p2, double *box) { 

    double dx,dy,dz,lx,ly,lz;

    dx = p2[0] - p1[0];
    dy = p2[1] - p1[1];
    dz = p2[2] - p1[2];
    lx = box[0];
    ly = box[1];
    lz = box[2];

    if (fabs(dx) > lx*0.5) dx -= lx*round(dx/lx); 
    if (fabs(dy) > ly*0.5) dy -= ly*round(dy/ly);
    if (fabs(dz) > lz*0.5) dz -= lz*round(dz/lz);

    return dx*dx + dy*dy + dz*dz;

}

double minimg_r(double * p1, double * p2, double *box) {

  return sqrt(minimg_r2(p1,p2,box));

}

double rand_d() {
    return (double)rand()/(double)RAND_MAX;
}