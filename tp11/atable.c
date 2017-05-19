#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

#define N 1000000
#define M 10

int c[M];


/*****************************************/
// DECLARATION DE VERROUS A COMPLETER


pthread_mutex_t mut[M];


// FONCTIONS A COMPLETER

void enter_critical(int i, int j){
  pthread_mutex_lock (&mut[(i<j)?i:j]);
  pthread_mutex_lock (&mut[(i<j)?j:i]);
}

void exit_critical(int i, int j){
  pthread_mutex_unlock (&mut[i]);
  pthread_mutex_unlock (&mut[j]);
}

void init_mutexes(){
  int j;
  for(j=0;j<M;j++) pthread_mutex_init (&mut[j], NULL);
}

void destroy_mutexes(){
  int j;
  for(j=0;j<M;j++) pthread_mutex_destroy (&mut[j]);
}

/*****************************************/


int randpos() {
  return (rand() % M);
}

void atomic_swap(int i, int j){

  enter_critical(i, j);

  int t = c[i];
  c[i] = c[j];
  c[j] = t;

  exit_critical(i, j);

}


void *threadfun(void *arg) {

  int n;

  for(n=0;n<N;n++){

    int i = randpos();
    int j = randpos();

    if (i!=j) atomic_swap(i,j);

  }

  printf("Fin %s\n",(char *)arg);
  return NULL;

}



int main(int argc, char **argv){

  pthread_t t1,t2;

  srand(time(NULL));

  int i;
  for(i=0;i<M;i++) c[i] = i;

  init_mutexes();

  pthread_create(&t1,NULL,threadfun,"thread 1");
  pthread_create(&t2,NULL,threadfun,"thread 2");
  pthread_join(t1,NULL);
  pthread_join(t2,NULL);

  destroy_mutexes();

  for(i=0;i<M;i++) printf("c[%d] = %d\n",i,c[i]);
  printf("Fin main.\n");

}
