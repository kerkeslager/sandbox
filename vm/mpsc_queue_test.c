#include <assert.h>
#include <stdio.h>

#include "mpsc_queue.c"

void noopFreer(void* _) { }

bool test_single_threaded()
{
  Queue queue;
  initializeQueue(&queue);

  int inputs[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  for(int i = 0; i < 10; i++)
  {
    enqueue(&queue, &(inputs[i]));
  }

  int* outputs[10];
  int index = 0;

  while(!isEmpty(&queue))
  {
    outputs[index++] = dequeue(&queue);
  }

  for(int i = 0; i < 10; i++)
  {
    assert(i == *(outputs[i]));
  }

  freeQueue(&queue, noopFreer);

  return true;
}

int main(int argc, char** argv)
{
  #define TEST_COUNT 1
  bool (*tests[TEST_COUNT])() = {
    test_single_threaded
  };

  for(int i = 0; i < TEST_COUNT; i++)
  {
    if(tests[i]())
    {
      printf(".");
    }

    else
    {
      printf("F");
    }
  }

  return EXIT_SUCCESS;
}
