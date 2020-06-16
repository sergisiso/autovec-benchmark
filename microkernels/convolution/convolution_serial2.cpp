
#if SPECIALIZE
constexpr int matrixsize = 3;
constexpr int offset = matrixsize / 2;
#else
int matrixsize;
int offset;
#endif


float matrix[3*3] = {  -1, -1, -1 , -1,  9, -1 , -1, -1, -1 }; 

#if SPECIALIZE
void draw(float * __restrict__ img, int width, int height,
        int matrixsizep, float * __restrict__ output) {
#else
void draw(float * img, int width, int height, int matrixsizep, float * output) {
    matrixsize = matrixsizep;
    offset = matrixsize / 2;
#endif

    float redout[width*height];
    float greenout[width*height];
    float blueout[width*height];


    // Begin our loop for every pixel in the smaller image
    for (int x = 1; x < width-1; x++) {
        for (int y = 1; y < height-1; y++ ) {
            //color c = convolution(x, y, matrix, matrixsize, img);
            float rtotal = 0.0;
            float gtotal = 0.0;
            float btotal = 0.0;
            for (int i = 0; i < matrixsize; i++){
                for (int j= 0; j < matrixsize; j++){
                    // What pixel are we testing
                    int xloc = x+i-offset;
                    int yloc = y+j-offset;
                    int loc = xloc + width * yloc;
                    // Calculate the convolution
                    rtotal += img[loc] * matrix[i + matrixsize*j];
                    gtotal += img[loc + (width*height)] * matrix[i + matrixsize*j];
                    btotal += img[loc + 2*(width*height)] * matrix[i + matrixsize*j];
                }
            }

            int loc = x + y * width;
            redout[loc] = rtotal;
            greenout[loc] = gtotal;
            blueout[loc] = btotal;
        }
    }

    for(int i=0; i< width*height; i++){
        output[i] = redout[i];
        output[i + (width*height)] = greenout[i];
        output[i + 2*(width*height)] = blueout[i];
    }
}
