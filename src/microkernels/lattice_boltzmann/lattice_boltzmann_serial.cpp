#include <cmath>
#include <utility>
#include <iostream>

int TSIZE;
int HEndIn;
#if SPECIALIZE
constexpr int HIniIn = 1;
constexpr int FLUIDS = 4;
int LATS = 19; // Should be constexpr but it causes compiler error?
constexpr double postequil = 1; // ? which value
#else
int HIniIn;
int FLUIDS;
int LATS;
double postequil;
#endif

// Define Model - D3Q19
// {centre(1), +near(3), +long(6), -near(3), -long(6)}
const int lbvx[19] = {0,-1,0,0,-1,-1,-1,-1,0,0,1,0,0,1,1,1,1,0,0};
const int lbvy[19] = {0,0,-1,0,-1,1,0,0,-1,-1,0,1,0,1,-1,0,0,1,1};
const int lbvz[19] = {0,0,0,-1,0,0,-1,1,-1,1,0,0,1,0,0,1,-1,1,-1};
const double lbw[19] = {1.0/3.0,1.0/18.0,1.0/18.0,1.0/18.0,1.0/36.0,
    1.0/36.0,1.0/36.0,1.0/36.0,1.0/36.0,1.0/36.0,
    1.0/18.0,1.0/18.0,1.0/18.0,1.0/36.0,1.0/36.0,
    1.0/36.0,1.0/36.0,1.0/36.0,1.0/36.0};

const double lbvwx[19] = {0.0,-1.0/18.0,0,0,-1.0/36.0,-1.0/36.0,-1.0/36.0,
    -1.0/36.0,0,0,1.0/18.0,0,0,1.0/36.0,1.0/36.0,
    1.0/36.0,1.0/36.0,0,0};
const double lbvwy[19] = {0.0,0,-1.0/18.0,0,-1.0/36,1.0/36.0,0,0,-1.0/36.0,
    -1.0/36.0,0,1.0/18.0,0,1.0/36.0,-1.0/36.0,0,0,
    1.0/36.0,1.0/36.0};
const double lbvwz[19] = {0.0,0,0,-1.0/18.0,0,0,-1.0/36.0,1.0/36.0,-1.0/36.0,
    1.0/36.0,0,0,1.0/18.0,0,0,1.0/36.0,-1.0/36.0,
    1.0/36.0,-1.0/36.0};


//DistributionFunctions (with halo)
inline int index(int x, int y, int z, int f, int l){
    return ( (((x*TSIZE+y)*FLUIDS+f)*LATS+l)*TSIZE+z );
}
inline int index_neighbour(int x, int y, int z, int f, int l){
    return index(x+lbvx[l],y+lbvy[l],z+lbvz[l],f,l);
}
inline int t_total(){return TSIZE * TSIZE * TSIZE * FLUIDS * LATS;}

//Potentials (with halo)
inline int indexpot(int x, int y, int z, int f){
    return ( ((x*TSIZE+y)*FLUIDS+f)*TSIZE+z );
}
inline int indexpot_neighbour(int x, int y, int z, int f, int l){
    return indexpot(x+lbvx[l],y+lbvy[l],z+lbvz[l],f);
}
inline int t_pot(){return TSIZE * TSIZE * TSIZE * FLUIDS;}

// lbphi (with halo)
inline int indexphi(int x, int y, int z){
    return (x*TSIZE+y)*TSIZE+z;
}
inline int t_phi(){return TSIZE * TSIZE * TSIZE;}

inline int total_mem(){
    return (sizeof(double) * (t_total()*2+t_pot()) + t_phi() * sizeof(int))/1024/1024;
}

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
    double *&lbbdforcez) {
 
    TSIZE = 84;
    HEndIn = 82;
#ifndef SPECIALIZE
    HIniIn = 1;
    FLUIDS = 4;
    LATS= 19;
#endif

    // Allocate array
    array = new double [t_total()];
    array_new = new double [t_total()];
    array_pot = new double [t_pot()];
    lbphi = new double [t_phi()];
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
    for(int i = 0; i < t_total(); i++) array[i] = 1;
    for(int i = 0; i < t_total(); i++) array_new[i] = 1;
    for(int i = 0; i < t_pot(); i++) array_pot[i] = 1;
    for(int i = 0; i < t_phi(); i++) lbphi[i] = 1;
}

#if SPECIALIZE
void lattice_boltzmann_serial(
    double * __restrict__ array,
    double * __restrict__ array_new,
    double * __restrict__ array_pot,
    double * __restrict__ lbphi,
    double * __restrict__ lbg,
    double * __restrict__ lbincp,
    double * __restrict__ lbtf,
    double * __restrict__ lbbdforcex,
    double * __restrict__ lbbdforcey,
    double * __restrict__ lbbdforcez) {
    double * __restrict__ array_ptr;
#else
void lattice_boltzmann_serial(
    double * array,
    double * array_new,
    double * array_pot,
    double * lbphi,
    double * lbg,
    double * lbincp,
    double * lbtf,
    double * lbbdforcex,
    double * lbbdforcey,
    double * lbbdforcez) {
    double * array_ptr;
#endif

#ifndef SPECIALIZE
    postequil = 1; // ? which value
#endif

    for(int iteration = 0; iteration <= 5; iteration++){

        // Calculate Potential of each particle
//#pragma omp parallel for schedule(static) collapse(2)
        for(int x=0; x<TSIZE; x++){
            for(int y=0; y<TSIZE; y++){
#pragma omp simd
                for(int z=0; z<TSIZE; z++){
                    for(int f=0; f<FLUIDS; f++) {
                        double mass=0.0;
                        for(int l=0; l<LATS; l++){
                            mass += array[index(x,y,z,f,l)];
                        }
                        array_pot[indexpot(x,y,z,f)] = lbincp[f] * (1.0 - exp(-mass)/lbincp[f]);
                    }
                }
            }
        }

        // Compute interaction forces, collition and propagation
//#pragma omp parallel for schedule(static) collapse(2)
        for(int x=HIniIn; x<=HEndIn; x++){
            for(int y=HIniIn; y<=HEndIn; y++){
#pragma omp simd
                for(int z=HIniIn; z<=HEndIn; z++){
                    double factorx[FLUIDS];
                    double factory[FLUIDS];
                    double factorz[FLUIDS];
                    for(int i=0; i<FLUIDS; i++){
                        factorx[i] = 0.0;
                        factory[i] = 0.0;
                        factorz[i] = 0.0;
                    }

                    double particle_speedx = 0.0;
                    double particle_speedy = 0.0;
                    double particle_speedz = 0.0;
                    double particle_mass = 0.0;

                    //Compute particle mass and speed
                    double fluid_mass=0.0;
                    for(int fluid=0; fluid<FLUIDS; fluid++){
                        for(int l=0; l<LATS; l++) {
                            particle_mass += array[index(x,y,z,fluid,l)];
                            particle_speedx += array[index(x,y,z,fluid,l)] * lbvx[l];
                            particle_speedy += array[index(x,y,z,fluid,l)] * lbvy[l];
                            particle_speedz += array[index(x,y,z,fluid,l)] * lbvz[l];
                        }
                    }
                    const double particle_inverse_mass = 1.0/particle_mass;
                    particle_speedx *= particle_inverse_mass;
                    particle_speedy *= particle_inverse_mass;
                    particle_speedz *= particle_inverse_mass;

                    // Compute particle factors
                    if(lbphi[indexphi(x,y,z)] == 0){
                        for(int lattice=1; lattice<LATS; lattice++){
                            for(int fluid=0; fluid<FLUIDS; fluid++){
                                const double psi = array_pot[indexpot_neighbour(x,y,z,fluid,lattice)];
                                factorx[fluid] += lbvwx[lattice] * psi;
                                factory[fluid] += lbvwy[lattice] * psi;
                                factorz[fluid] += lbvwz[lattice] * psi;
                            }
                        }
                    }

                    // Collide and propagate
                    for(int fluid=0; fluid<FLUIDS; fluid++){
                        double fluid_mass = 0.0;
                        double interforce_local_x = 0.0;
                        double interforce_local_y = 0.0;
                        double interforce_local_z = 0.0;

                        for(int lattice=0; lattice<LATS; lattice++) {
                            fluid_mass += array[index(x,y,z,fluid,lattice)];
                        }


                        // Shan Chen interaction
                        if(lbphi[indexphi(x,y,z)] == 0.0){
                            const double psik = std::abs(array_pot[indexpot(x,y,z,fluid)]);
                            for(int fluid2=0; fluid2<FLUIDS; fluid2++){
                                double gfluid = lbg[fluid*FLUIDS+fluid2] * psik;
                                interforce_local_x-=gfluid*factorx[fluid2];
                                interforce_local_y-=gfluid*factory[fluid2];
                                interforce_local_z-=gfluid*factorz[fluid2];
                            }
                        }


                        // Collision
                        if(lbphi[indexphi(x,y,z)] != 11){
                            interforce_local_x += postequil * lbbdforcex[fluid];
                            interforce_local_y += postequil * lbbdforcey[fluid];
                            interforce_local_z += postequil * lbbdforcez[fluid];
                            if(lbphi[indexphi(x,y,z)] != 12 && lbphi[indexphi(x,y,z)] !=13){
                                const double invmass = 1.0/fluid_mass;
                                const double invmassrelax = invmass/lbtf[fluid];
                                const double relax = 1.0 - lbtf[fluid];
                                const double sx = particle_speedx + interforce_local_x * invmassrelax;
                                const double sy = particle_speedy + interforce_local_y * invmassrelax;
                                const double sz = particle_speedz + interforce_local_z * invmassrelax;
                                const double speed_mod = sx*sx + sy*sy + sz*sz;

                                for(int l=0; l<LATS; l++){
                                    const double uv=lbvx[l]*sx+lbvy[l]*sy+lbvz[l]*sz;
                                    const double feq=fluid_mass*lbw[l]*(1+3.0*uv+4.5*uv*uv-1.5*speed_mod);

                                    // Streaming step
                                    array_new[index_neighbour(x,y,z,fluid,l)] = relax * array[index(x,y,z,fluid,l)] + feq*lbtf[fluid];
                                }
                            }
                        }
                    }
                }
            }
        }

        // Array swap
        std::swap(array_new, array);
    }

    // Give the class pointers the proper final state
    //array_ptr = array;
    //array_new_ptr = array_new;

}


