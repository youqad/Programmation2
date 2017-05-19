#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

#define N1 10000
#define N2 1000
#define NumLocalThreads 5
#define NumGlobalThreads 2
#define M 10


int c[M];


/*****************************************/
// DECLARATION DE VERROUS A COMPLETER

pthread_mutex_t global_mutex;
pthread_mutex_t local_mutex[M];

pthread_cond_t nains;

unsigned int num_nains = 0;


// FONCTIONS A COMPLETER

void enter_critical_local(int i, int j){
  pthread_mutex_lock (&local_mutex[(i<j)?i:j]);
  pthread_mutex_lock (&local_mutex[(i<j)?j:i]);
  pthread_mutex_lock (&global_mutex);
  num_nains++;
  pthread_mutex_unlock (&global_mutex);

}

void exit_critical_local(int i, int j){
  pthread_mutex_lock (&global_mutex);
  num_nains--;
  pthread_mutex_unlock (&global_mutex);

  if (num_nains <= 0) {
    pthread_cond_broadcast(&nains) ;
  }
  pthread_mutex_unlock (&local_mutex[i]);
  pthread_mutex_unlock (&local_mutex[j]);
}

void enter_critical_global(){
  pthread_mutex_lock (&global_mutex);
  while ( num_nains > 0 ) {
    pthread_cond_wait (&nains , &global_mutex);
  }
}

void exit_critical_global(){
  pthread_mutex_unlock (&global_mutex);
}

void init_mutexes() {
    pthread_mutex_init (&global_mutex, NULL);
    int j;
    for(j=0;j<M;j++) pthread_mutex_init (&local_mutex[j], NULL);
    pthread_cond_init(&nains, NULL);
}

void destroy_mutexes() {
    pthread_mutex_destroy (&global_mutex);
    int j;
    for(j=0;j<M;j++) pthread_mutex_destroy (&local_mutex[j]);
    pthread_cond_destroy(&nains);
}

/*****************************************/




int randpos() {
  return (rand() % M);
}

void local_change(int i, int j){

  c[i]++;
  c[j]--;

}


void global_change() {

  int i;
  int fst;
  fst = c[0];
  for(i=0;i<M;i++) c[i] = c[i+1];
  c[M-1] = fst;

}



void *threadfun_local(void *arg) {

  int tid = * (int *) arg;

  printf("Debut thread local %d\n",tid);


  int n;

  for(n=0;n<N1;n++){

    int i = randpos();
    int j = randpos();

    if (i!=j) {
      enter_critical_local(i,j);
      local_change(i,j);
      exit_critical_local(i,j);
    }
  }

  printf("Fin thread local %d\n",tid);
  return NULL;

}




void *threadfun_global(void *arg) {

  int n;

  int tid = * (int *) arg;

  printf("Debut thread global %d\n",tid);

  for(n=0;n<N2;n++){

    enter_critical_global();
    global_change();
    exit_critical_global();

  }

  printf("Fin thread global %d\n",tid);
  return NULL;

}

int tid[NumLocalThreads+NumGlobalThreads];

int main(int argc, char **argv){

  pthread_t local[NumLocalThreads];
  pthread_t global[NumGlobalThreads];

  srand(time(NULL));

  int i;
  for(i=0; i<M; i++) c[i] = 0;

  for(i=0; i<NumLocalThreads+NumGlobalThreads; i++) tid[i] = i;

  init_mutexes();

  for(i=0; i<NumLocalThreads; i++)
    pthread_create(&local[i],NULL,threadfun_local,&tid[i]);

  for(i=0; i<NumGlobalThreads; i++)
    pthread_create(&global[i],NULL,threadfun_global,&tid[i]);

  for(i=0; i<NumLocalThreads; i++) pthread_join(local[i],NULL);

  for(i=0; i<NumGlobalThreads; i++) pthread_join(global[i],NULL);

  destroy_mutexes();

  int sum = 0;
  for(i=0; i<M; i++) {
    sum += c[i];
    printf("c[%d] = %d\n",i,c[i]);
  }
  if (sum == 0) printf("correct\n"); else printf("incorrect\n");

  printf("Fin main.\n");

}
