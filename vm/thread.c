#ifndef THREAD
#define THREAD

#include "mpsc_queue.c"

struct Thread;
typedef struct Thread Thread;

void initializeThread(Thread* thread);
void destructThread(Thread* thread);

struct Thread
{
  MPSCQueue inbox;
};

void initializeThread(Thread* thread)
{
  initializeMPSCQueue(&(thread->inbox));
}

void destructThread(Thread* thread)
{
  destructMPSCQueue(&(thread->inbox));
}

#endif
