/**
 * @file   test.c
 * @author Nix <nix@naunine.com>
 * @date   Fri Oct  2 09:33:42 2009
 * 
 * @brief  
 */

#include <stdio.h>

#define M 5
#define N 5

int A (int m, int n)
{
    // printf("A[%d %d] = ",m,n);
    if(0==m){
        //    printf("%d\n",n+1);
        return n+1;
    }else if(m>0 && 0==n){
        //printf("A[%d,%d]\n",m-1,1);
        return A(m-1,1);
    }else if(m>0 && n>0){
        //printf("A[%d, A[%d %d]]\n",m-1,m,n-1);
        return A(m-1, A(m,n-1));
    }
}

//-----------------------------------------------------------------------------
/** 
 * MAIN This is where is all starts
 * 
 * @param argc Total number of input parameters
 * @param argv Array of input strings
 * 
 * @return 0 on success and negative numbers on error
 */
//-----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
    int i,j;
    int a,b;
    //int a[M][N];
    for (i=0; i < M; ++i) {
        for (j=0; j<N; ++j) {
            a = i<j? j-i :i-j;
            b =  (a + 51) / 52;
            printf("%5.2f",(double)(1-b)*A(a,a)); 
        }
        printf("\n");
    }

    return 0;
}
