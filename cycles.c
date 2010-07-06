#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct List List;

struct List { List* next; };

List* generateCycle()
{
	srand(time(NULL));

	// It may be necessary to tinker with this value to keep rand() within
	// reasonable bounds.
	const int modifier = 16;
	int a = rand() >> modifier,b = rand() >> modifier;

	// a must be less than or equal to b
	if(a > b)
	{
		int t = a;
		a = b;
		b = t;
	}

	List* cycle;
	List* current = malloc(sizeof(List));

	List* r = current;

	for(int i = 0; i < b; i++)
	{
		current->next = malloc(sizeof(List));
		current = current->next;

		if(i == a) cycle = current;
	}

	current->next = cycle;

	return r;
}

// TODO Implement a function that frees the cycled list.

size_t tortoiseAndHare(List* list)
{
	size_t count = 0;

	List* tortoise = list;
	List* hare = list;

	while(1)
	{
		count++;

		hare = hare->next;
		if(hare == NULL) return 0;
		else if(tortoise == hare) return count;

		hare = hare->next;
		if(hare == NULL) return 0;
		else if(tortoise == hare) return count;

		tortoise = tortoise->next;
	}
}

size_t teleportingTurtle(List* list)
{
	size_t count = 0;

	List* turtle = list;
	List* rabbit = list;
	int patience = 1;

	while(1)
	{
		for(int i = 0; i < patience; i++)
		{
			count++;

			rabbit = rabbit->next;
			if(rabbit == NULL) return 0;
			else if(turtle == rabbit) return count;

			rabbit = rabbit->next;
			if(rabbit == NULL) return 0;
			else if(turtle == rabbit) return count;

			turtle = turtle->next;
		}

		patience = patience << 1;
		turtle = rabbit;			// TELEPORT!
	}
}

// TODO Instead of counting steps, take the time.
// TODO Research how to detect where the cycle starts (and if it is useful).

int main()
{
	int limit = 256;
	size_t th, thtotal = 0, tt, tttotal = 0;

	for(int i = 0; i < limit; i++)
	{
		printf("Generating racetrack.\n");
		List* cycle = generateCycle();

		printf("Racing tortoise vs. hare.\n");
		th = tortoiseAndHare(cycle);

		printf("Racing teleporting turtle vs. rabbit.\n");
		tt = teleportingTurtle(cycle);

		thtotal += th;
		tttotal += tt;
	}

	printf("Tortoise and hare: %i\n",thtotal / limit);
	printf("Teleporting turtle: %i\n",tttotal / limit);
}
