#include <stdio.h>

int divisibleRec(int i, int j){
  while (j>0) {
  if(j==1){ return 0; }
  else if(i%j==0){ return 1; }
  else{ //return divisibleRec(i,j-1); 
    j = j - 1;
  }
  }
}

int divisible(int i){ return divisibleRec(i, i-1); }

int main(void){
  int i, count =0;
  for(i=2; i<20000; ++i){
    if(!divisible(i)){
      count = count+1;
    }
  }
  printf("number of primes = %d\n",count);
  return 0;
}
