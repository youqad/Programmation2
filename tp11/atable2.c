#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <stdbool.h>

#define N 1000000
#define M 10

int c[M];


/*****************************************/
// DECLARATION DE VERROUS A COMPLETER

int use[2][2];

pthread_mutex_t use_mutex;

bool overlap(i,j,k,l) {
  return (((i<=k) && (j <= l)) || ((k<=i) && (l <= j)))? true : false;
}

// FONCTIONS A COMPLETER

void enter_critical(int i, int j, int tid){

  bool acquired = false;

  while (!acquired){
    pthread_mutex_lock(&use_mutex);
    acquired = use[1-tid][0]>j || use[1-tid][1]<i;
    if (acquired){
      use[tid][0] = i;
      use[tid][1] = j;
    }
    pthread_mutex_unlock(&use_mutex);
  }

  // while (!acquired){
  //   use[tid][0] = i;
  //   use[tid][1] = j;
  //   acquired = use[1-tid][0]>j || use[1-tid][1]<i;
  //   if (acquired){
  //     use[tid][0] = M;
  //     use[tid][1] = -1;
  //   }
  // }

}

void exit_critical(int i, int j, int tid){
  // pthread_mutex_lock(&mut);
  use[tid][0] = M;
  use[tid][1] = -1;
  // pthread_mutex_unlock(&mut);

}

void init_mutexes(){
  int i;
  for (i = 0; i < 1; i++) {
    use[i][0] = M;
    use[i][1] = -1;
  }
}

void destroy_mutexes(){}

/*****************************************/


int randpos() {
  return (rand() % M);
}

void atomic_perm(int i, int j, int tid){

  if (i>j) {
    int t = i;
    i = j;
    j = t;
  }

  enter_critical(i,j,tid);

  int n;
  int fst;
  fst = c[i];
  for(n=i; n<j; n++) c[n] = c[n+1];
  c[j] = fst;

  exit_critical(i,j,tid);

}


void *threadfun(void *arg) {

  int n;
  int tid = *(int *)arg;

  for(n=0;n<N;n++){
    int i = randpos();
    int j = randpos();
    if (i!=j) atomic_perm(i,j,tid);
  }

  printf("Fin %d\n",tid);
  return NULL;

}


int main(int argc, char **argv){

  pthread_t t1,t2;

  srand(time(NULL));

  int tid[2];
  tid[0] = 0;
  tid[1] = 1;

  int i;
  for(i=0; i<M; i++) c[i] = i;

  init_mutexes();
  pthread_create(&t1,NULL,threadfun,&tid[0]);
  pthread_create(&t2,NULL,threadfun,&tid[1]);
  pthread_join(t1,NULL);
  pthread_join(t2,NULL);
  destroy_mutexes();

  for(i=0; i<M; i++) printf("c[%d] = %d\n",i,c[i]);
  printf("Fin main.\n");

}
