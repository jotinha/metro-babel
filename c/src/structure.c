#include "structure.h"

void initStructureRandom(Config * conf) {

  double * p = conf->pos; 
  double * b = conf->box;
  for (uint i=0; i < conf->N*3; i++) {
    p[i] = random()*b[i%3];
  }
}

double getCellDimension(uint nAtomsPerCell, uint nAtomsTotal) {
  //find m which obeys: nAtomsTotal = nAtomsPerCell*m**3

  uint c;

  if (nAtomsTotal % nAtomsPerCell != 0) {
    ERROR
  }


}

void initStructureFCC(Config * conf) {


  

}

void initStructure(Config * conf,ESTRUCTURE type) {
  
  switch (type) {
  
    case ESLIQ:
      initStructureRandom(Config * conf);
      break;
    case ESFCC:
      initStructureFCC(Config * conf);
      break;
    case ESBCC:
      initStructureBCC(Config * conf);
      break;
    case ESHCP:
      initStructureHCP(Config * conf);
      break;
  }

}