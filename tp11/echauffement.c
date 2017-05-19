#include <stdio.h>
#include <pthread.h>

#define N 100000

int c = 0;

/*****************************************/
// DECLARATION DE VERROUS A COMPLETER

static pthread_mutex_t mut = PTHREAD_MUTEX_INITIALIZER;

// FONCTIONS A COMPLETER

void enter_critical(){
  pthread_mutex_lock (&mut);
}

void exit_critical(){
  pthread_mutex_unlock (&mut);
}

void init_mutexes(){
  pthread_mutex_init(&mut, NULL);
}

void destroy_mutexes(){
  pthread_mutex_destroy (&mut);
}

/*****************************************/

void *threadfun(void *arg) {

  int i;

  for(i=0;i<N;i++){
    enter_critical();
    c++;
    exit_critical();
  }

  printf("Fin %s\n",(char *)arg);
  return NULL;

}


int main(int argc, char **argv){

  pthread_t t1,t2;

  init_mutexes();
  pthread_create(&t1,NULL,threadfun,"thread 1");
  pthread_create(&t2,NULL,threadfun,"thread 2");
  pthread_join(t1,NULL);
  pthread_join(t2,NULL);
  printf("c=%d\nFin main.\n",c);
  destroy_mutexes();

}
