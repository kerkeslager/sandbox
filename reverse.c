// This code uses nested functions, which were introduced to C in the 1999
// standard, so it won't compile on some older compilers. To compile with GCC,
// use "gcc -std=c99 reverse.c".

#include <stdlib.h>

typedef struct List List;

struct List
{
	void* head;
	List* tail;
};

List* makeList(void* head, List* tail)
{
	List* result = malloc(sizeof(result));
	result->head = head;
	result->tail = tail;
	return result;
}

List* reverse(List* list)
{
	List* internal(List* in, List* out)
	{
		if(in == NULL)
			return out;

		return internal(in->tail, makeList(in->head, out));
	}

	return internal(list,NULL);
}

// Tests.
#include <assert.h>
#include <stdio.h>

void test_reverseOnEmptyList()
{
	List* fixture = NULL;

	List* result = reverse(fixture);

	assert(result == NULL);
}

void test_reverseOnSingleElementList()
{
	int one = 1;
	List* fixture = makeList(&one,NULL);

	List* result = reverse(fixture);

	assert(result->head == &one);
	assert(result->tail == NULL);
}

void test_reverseOnMultiElementList()
{
	int one = 1, two = 2, three = 3;
	List* fixture = makeList(&one, makeList(&two, makeList(&three, NULL)));

	List* result = reverse(fixture);

	assert(result->head == &three);
	assert(result->tail->head == &two);
	assert(result->tail->tail->head == &one);
	assert(result->tail->tail->tail == NULL);
}

void test_doublyReversedListIsSameAsOriginal()
{
	int listsAreEqual(List* left, List* right)
	{
		if(left == NULL && right == NULL)
			return 1;

		if(left == NULL || right == NULL || left->head != right->head)
			return 0;

		return listsAreEqual(left->tail, right->tail);
	}

	int one = 1, two = 2, three = 3;
	List* fixture = makeList(&one, makeList(&two, makeList(&three, NULL)));

	List* result = reverse(reverse(fixture));

	assert(listsAreEqual(fixture, result));
}

int main(int argc, char** argv)
{
	test_reverseOnEmptyList();
	test_reverseOnSingleElementList();
	test_reverseOnMultiElementList();
	test_doublyReversedListIsSameAsOriginal();

	printf("All tests passed.\n");
	return 0;
}
