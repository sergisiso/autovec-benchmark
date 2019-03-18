#include <stdio.h>
#include <algorithm>
#include <string.h>
#include <cstdlib>

#include <math.h>
#include <stdlib.h>
#include <iostream>

/* Min, max functions */
#define MIN(X, Y)           (((X) < (Y)) ?  (X):(Y))
#define MAX(X, Y)           (((X) > (Y)) ?  (X):(Y))

#define COLUMNS 	0
#define ROWS 		1

#define TRUE 		1
#define FALSE 		0


double log2(double n)
{
	return log(n)/log((double) 2);
}

void _fast_wh(double *work, int N, double *output, unsigned char scale_output)
{
	/* Note that Hadamard transform will only work for power of 2 sized vectors */
	int i, j, k, stage, L, J, M;
	double *tmp;
	double scale;

	/* Warning! work variable will be modified! */

	/* Compute add and difference vectors */
	for (i = 0; i < N-1; i += 2){
		work[i] = work[i] + work[i+1];
		work[i+1] = work[i] - 2*work[i+1];
	}

	/* Main loop. Iteratively compute the hadamard transform */
	L = 1;
	for (stage = 2; stage <= ceil(log2(N)); ++stage){
		M = (int) pow((float)2, L);
		J = 0;
		k = 0;

		while (k < N-1){
			for (j = J; j < J+M-1; j = j+2){
				output[k] = work[j] + work[j+M];
				output[k+1] = work[j] - work[j+M];
				output[k+2] = work[j+1] - work[j+M+1];
				output[k+3] = work[j+1] + work[j+M+1];

				/* Increment by 4 since we computed 4 entries */
				k += 4;
			}

			J += 2*M;
		}

		/* Swap so that we can compute for the next iteration */
		tmp = work;
		work = output;
		output = tmp;

		/* Increment the level */
		L += 1;
	}

	if (scale_output == TRUE){
		scale = sqrt((float)N);
	}else{
		scale = 1;
	}

	/* Scale the output */
	for (i = 0; i < N; i++){
		output[i] = work[i]/scale;
	}
}

void fast_wh_vec(double *input, int N, double *output,
	   			 unsigned char scale_output)
{
	/* Create a copy and send it to core function */
	int i;

	double * work = (double *)malloc(N*sizeof(double));

	for (i = 0; i < N; i++){
		work[i] = input[i];
	}

	/* Call the function */
	_fast_wh(work, N, output, scale_output);

	/* Free the work array */
	//free(work);
}

void fast_wh_mat(double *input, int M, int N, double *output, unsigned char dir,
				 unsigned char scale_output)
{
	int i, j, idx1, idx2;
	double *work, *vec_output;

	/* Allocate temporary work memory */
	if (dir == COLUMNS){
		work = (double *) malloc(M*sizeof(double));
		vec_output = (double *) malloc(M*sizeof(double));
		idx1 = N;
		idx2 = M;
	}else{
		work = (double *) malloc(N*sizeof(double));
		vec_output = (double *) malloc(N*sizeof(double));
		idx1 = M;
		idx2 = N;
	}

	for (i = 0; i < idx1; i++){
		/* Copy data */
		for (j = 0; j < idx1; j++){
			work[j] = input[i*idx1 + j];
		}

		/* Now call core function */
		_fast_wh(work, idx2, vec_output, scale_output);

		/* Copy to output array */
		for (j = 0; j < idx1; j++){
			output[i*idx1 + j] = vec_output[j];
		}
	}

	/* Free the data variables */
	free(work);
	free(vec_output);
}
