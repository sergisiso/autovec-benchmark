#define NOMINMAX

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <algorithm>
using std::max;

#include "../timing.h"

extern void lattice_boltzmann_serial(double * array, double * array_new, double * array_pot,
    double * lbphi, double * lbg, double * lbincp, double * lbtf, double * lbbdforcex,
    double * lbbdforcey, double * lbbdforcez, int p_fluids, int p_lats, int p_size);
 
void initialize(
    double *&array,
    double *&array_new,
    double *&array_pot,
    double *&lbphi,
    double *&lbg,
    double *&lbincp,
    double *&lbtf,
    double *&lbbdforcex,
    double *&lbbdforcey,
    double *&lbbdforcez,
    int FLUIDS,
    int t_total,
    int t_pot,
    int t_phi) {
 
    // Allocate array
    array = new double [t_total];
    array_new = new double [t_total];
    array_pot = new double [t_pot];
    lbphi = new double [t_phi];
    lbg = new double [FLUIDS*FLUIDS];
    lbincp = new double [FLUIDS];
    lbtf = new double [FLUIDS];
    lbbdforcex = new double [FLUIDS];
    lbbdforcey = new double [FLUIDS];
    lbbdforcez = new double [FLUIDS];

    // Initialize arrays
    for(int i = 0; i < FLUIDS; i++){
        lbincp[i] = 1; lbtf[i] = 1; lbbdforcex[i] = 1; lbbdforcey[i] = 1; lbbdforcez[i] = 1;
    }
    for(int i = 0; i < FLUIDS*FLUIDS; i++) lbg[i] = 1;
    for(int i = 0; i < t_total; i++) array[i] = 1;
    for(int i = 0; i < t_total; i++) array_new[i] = 1;
    for(int i = 0; i < t_pot; i++) array_pot[i] = 1;
    for(int i = 0; i < t_phi; i++) lbphi[i] = 1;
}

int main(int argc, char *argv[]) {

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

    // Define parameters
    int FLUIDS = 4;
    int LATS = 19;
    int TSIZE = 100;
    int t_total = TSIZE * TSIZE * TSIZE * FLUIDS * LATS;
    int t_pot = TSIZE * TSIZE * TSIZE * FLUIDS;
    int t_phi = TSIZE * TSIZE * TSIZE;

    initialize(array, array_new, array_pot, lbphi, lbg, lbincp, lbtf, lbbdforcex,
               lbbdforcey, lbbdforcez, FLUIDS, t_total, t_pot, t_phi);

    //
    // Lattice Boltzmann, serial implementation
    //
    double lattice_boltzmann_t = 1e30;
    for (int i = 0; i < 1; ++i) {
        reset_and_start_timer();
        lattice_boltzmann_serial(array, array_new, array_pot, lbphi, lbg, lbincp, lbtf,
                                 lbbdforcex, lbbdforcey, lbbdforcez, FLUIDS, LATS, TSIZE);
        double dt = get_elapsed_msec();
        lattice_boltzmann_t = std::min(lattice_boltzmann_t, dt);
    }
    printf("[lattice_boltzmann serial]:\t\t%.3f mseconds \n", lattice_boltzmann_t);

    return 0;
}
