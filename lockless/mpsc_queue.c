#include <assert.h>
#include <stdlib.h>

struct Queue;
typedef struct Queue Queue;

struct QueueNode;
typedef struct QueueNode QueueNode;

struct Queue
{
  QueueNode* head;
  QueueNode* tail;
};

struct QueueNode
{
  void* item;
  QueueNode* next;
};

void initQueue(Queue* queue)
{
  QueueNode* fake = malloc(sizeof(QueueNode));

  fake->next = NULL;

  queue->head = fake;
  queue->tail = fake;
}

void enqueue(Queue* queue, void* item)
{
  // Create the node
  QueueNode* node = malloc(sizeof(QueueNode));
  node->item = item;
  node->next = NULL;

  // Append the node
  while(!__sync_bool_compare_and_swap(&(queue->tail->next), NULL, node));

  // Reuse the node variable to update the tail
  while((node = queue->tail)->next != NULL)
  {
    __sync_bool_compare_and_swap(&(queue->tail), node, node->next);
  }
}

void* dequeue(Queue* queue, void* defaultResult)
{
  if(queue->head == queue->tail) return defaultResult;

  assert(queue->head->next != NULL);

  QueueNode* previous = queue->head;
  queue->head = previous->next;
  free(previous);
  return queue->head->item;
}

void freeQueue(Queue* queue)
{
  QueueNode* node = queue->head;
  free(queue);

  while(node != NULL)
  {
    QueueNode* temp = node;
    node = temp->next;
    free(temp);
  }
}

int main() { }
