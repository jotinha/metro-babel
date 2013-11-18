#include "metro.h"
#include "config.h"

void displaceParticle(Config * conf, uint i, double dx, double dy, double dz) {

	double * p = &(conf->pos[i*3]);
	*p += dx;
	*(p++) += dy;
	*(p++) += dz;

}