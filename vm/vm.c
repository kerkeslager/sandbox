#include <stdbool.h>
#include <stdlib.h>

struct Queue;
typedef struct Queue Queue;
struct QueueNode;
typedef struct QueueNode QueueNode;

void initializeQueue(Queue* queue);
void enqueue(Queue* queue, void* item);
void* dequeue(Queue* queue);
void freeQueue(Queue* queue, void (*itemFreer)(void*));

struct Queue
{
  // This implements a multiple-producer, single-consumer lockless concurrent
  // queue.
  //
  // After initialization, `head` and `tail` always points to a valid memory
  // location--they are never `NULL`. To achieve this we initialize `head` and
  // `tail` with a dummy node. This has two negative implications:
  //
  // 1. Unintuitively, `head->item` actually represents the item most recently
  //    dequeued. In fact, `head->item` may have been freed after it was
  //    dequeued, so we should never dereference it.
  // 2. At initialization, no item has been dequeued yet, so we must allocate
  //    an extra `QueueNode` at initialization which will never be used to
  //    store an item.
  //
  // However, this extra memory allocation at initialization means that we
  // never have to check whether `head` or `tail` are `NULL`, or initialize
  // these variables concurrently, which are expensive operations.
  QueueNode* head;
  QueueNode* tail;
};

struct QueueNode
{
  // This implements a singly-linked list.
  void* item;
  QueueNode* next;
};

void initializeQueue(Queue* queue)
{
  // Initialize the queue with a node that fakes the previously dequeued item.
  // We don't have to initialize `dummyNode->item` because it will never be
  // used.
  QueueNode* dummyNode = malloc(sizeof(QueueNode));
  dummyNode->next = NULL;
  queue->head = dummyNode;
  queue->tail = dummyNode;
}

bool isEmpty(Queue* queue)
{
  // In a normal singly-linked list, the list is empty when the `head` is
  // `NULL`, but here the queue is empty when the `head` is the same as the
  // `tail`.
  return queue->head == queue->tail;
}

void enqueue(Queue* queue, void* item)
{
  // Create the node that will be the new tail.
  QueueNode* node = malloc(sizeof(QueueNode));
  node->item = item;
  node->next = NULL;

  // Append the node to the list. Note that this is blocked as long as
  // `queue->tail` is not actually pointing to the last node in the list
  // because then `queue->tail->next` will not be `NULL`. This situation
  // occurs when another thread has called `enqueue` and appended a node to
  // the list, but hasn't yet updated `queue->tail`.
  while(!__sync_bool_compare_and_swap(&(queue->tail->next), NULL, node));

  // Move `queue->tail` forward until it points to the end of the list.
  // Other threads attempting to eqneue will be unable to do so until this
  // is complete. Additionally, other threads attempting to dequeue will be
  // blocked until `queue->tail` has advanced by at least one node.
  //
  // We don't have to worry that another thread might have appended more
  // nodes; advancing `queue->tail` to the end of the linked list is always
  // the correct thing to do.
  QueueNode* tail;
  while((tail = queue->tail)->next != NULL)
  {
    __sync_val_compare_and_swap(&(queue->tail), tail, tail->next);
  }
}

void* dequeue(Queue* queue)
{
  if(isEmpty(queue))
  {
    return NULL; // TODO Provide ability to return a defined default.
  }

  QueueNode* oldHead = queue->head;
  queue->head = oldHead->next;
  free(oldHead);

  return queue->head->item;
}

void freeQueue(Queue* queue, void (*itemFreer)(void*))
{
  QueueNode* current = queue->head;
  free(queue);

  while(current != NULL)
  {
    itemFreer(current->item);
    QueueNode* temp = current->next;
    free(current);
    current = temp;
  }
}

int main(int argc, char** argv)
{
  return EXIT_SUCCESS;
}
