def swap(xs, a, b):
    swap.counter += 1
    temp = xs[a]
    xs[a] = xs[b]
    xs[b] = temp

swap.counter = 1

def merge(xs, start, divide, end):
    if divide < end:
        while start < divide and xs[start] < xs[divide]:
            start += 1

        if start == divide:
            return

    else:
        return

    # if start < divide is implied by the most recent check
    while divide < end and xs[divide - 1] < xs[end - 1]:
        end -= 1

    if divide == end:
        return

    swap(xs, start, divide)
    start += 1

    i = start
    j = divide + 1

    while i < divide and j < end:
        if xs[i] < xs[j]:
            i += 1
        else:
            swap(xs, i, j)
            i += 1
            j += 1

    # 100K
    #merge(xs, start, i, divide)
    #merge(xs, divide, j, end)
    #merge(xs, start, divide, end)

    # 100K
    #merge(xs, start, i, divide)
    #merge(xs, start, divide, j)
    #merge(xs, start, j, end)

    # 80K
    #merge(xs, i, divide, j)
    #merge(xs, start, i, j)
    #merge(xs, start, j, end)

    # 80K
    #merge(xs, i, divide, j)
    #merge(xs, i, j, end)
    #merge(xs, start, i, end)

    # 80K
    merge(xs, divide, j, end)
    merge(xs, i, divide, end)
    merge(xs, start, i, end)

def sort(xs):
    divisions = [0]

    for i in range(len(xs) - 1):
        if xs[i] > xs[i + 1]:
            divisions.append(i + 1)

    divisions.append(len(xs))

    while len(divisions) > 2:
        new_divisions = divisions[:]

        for i in range(1,len(divisions) - 1,2):
            merge(xs, divisions[i - 1], divisions[i], divisions[i + 1])
            new_divisions.remove(divisions[i])

        divisions = new_divisions

xs = [i for i in range(1000)]
import random
random.shuffle(xs)

print(xs)

sort(xs)

print(xs)

print('Swaps:', swap.counter)

def group_swap(xs, start, divide, end):
    if start == divide or divide == end:
        return

    width = divide - start
    for i in range(start, end):
        swap(xs, i, i + width if i + width < end else start + i + width - end)

print(xs)
