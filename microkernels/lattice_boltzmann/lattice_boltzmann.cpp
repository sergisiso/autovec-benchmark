#define NOMINMAX

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <algorithm>
using std::max;

#include "../timing.h"

extern void initialize(double *&array, double *&array_new, double *&array_pot, double *&lbphi,
    double *&lbg, double *&lbincp, double *&lbtf, double *&lbbdforcex, double *&lbbdforcey, double *&lbbdforcez);

extern void lattice_boltzmann_serial(double * array, double * array_new, double * array_pot, double * lbphi,
    double * lbg, double * lbincp, double * lbtf, double * lbbdforcex, double * lbbdforcey, double * lbbdforcez);
 
static void usage() {
    printf("usage: lattice_boltzmann \n");
}


int main(int argc, char *argv[]) {
    int nOptions = 128*1024;

    for (int i = 1; i < argc; ++i) {
        if (strncmp(argv[i], "--count=", 8) == 0) {
            nOptions = atoi(argv[i] + 8);
            if (nOptions <= 0) {
                usage();
                exit(1);
            }
        }
    }

     // Define arrays   
    double * array;
    double * array_new;
    double * array_pot;
    double * lbphi;
    double * lbg;
    double * lbincp; // Fluids
    double * lbtf; // Fluids
    double * lbbdforcex; // Fluids
    double * lbbdforcey; // Fluids
    double * lbbdforcez; // Fluids

    initialize(array, array_new, array_pot, lbphi, lbg, lbincp, lbtf, lbbdforcex, lbbdforcey, lbbdforcez);

    //
    // Lattice Boltzmann, serial implementation
    //
    double lattice_boltzmann_t = 1e30;
    for (int i = 0; i < 5; ++i) {
        reset_and_start_timer();
        lattice_boltzmann_serial(array, array_new, array_pot, lbphi, lbg, lbincp, lbtf, lbbdforcex, lbbdforcey, lbbdforcez);
        double dt = get_elapsed_msec();
        lattice_boltzmann_t = std::min(lattice_boltzmann_t, dt);
    }
    printf("[lattice_boltzmann serial]:\t\t[%.3f] mseconds (avg %f)\n", 
           lattice_boltzmann_t, "result");

    return 0;
}
