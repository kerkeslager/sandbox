import collections
import functools
import sys

def memoize(f):
    memo = {}
    sentinel = object()

    @functools.wraps(f)
    def wrapped(*args):
        result = memo.get(args, sentinel)

        if result is sentinel:
            result = f(*args)
            memo[args] = result

        return result

    return wrapped

@memoize
def lcs(a, b, i=None, j=None):
    if i is None and j is None:
        i = len(a)
        j = len(b)

    if i == 0 or j == 0:
        return ()

    if a[i - 1] == b[j - 1]:
        return lcs(a, b, i - 1, j - 1) + (a[i - 1],)

    x = lcs(a, b, i, j - 1)
    y = lcs(a, b, i - 1, j)

    if len(x) > len(y):
        return x
    else:
        return y

def changes(a, b, c):
    c = lcs(a, b)

    i, j, k = 0, 0, 0

    while i < len(a) or j < len(b):
        while i < len(a) and (k == len(c) or a[i] != c[k]):
            yield ('d', i, a[i])
            i += 1

        while j < len(b) and (k == len(c) or b[j] != c[k]):
            yield ('a', j, b[j])
            j += 1

        while i < len(a) and j < len(b) and a[i] == b[j]:
            assert a[i] == c[k]
            assert b[j] == c[k]

            i += 1
            j += 1
            k += 1

Block = collections.namedtuple(
    'Block',
    (
        'kind',
        'start',
        'finish',
        'lines',
    ),
)

def compress_block(b):
    return Block(
        kind=b[0][0],
        start=b[0][1],
        finish=b[-1][1],
        lines=tuple(ch[2] for ch in b),
    )

def format_block(b):
    direction = '>' if b.kind == 'a' else '<'

    def format_line(l):
        return '{} {}'.format(direction, l)

    return '{}{},{}\n{}'.format(
        b.kind,
        b.start,
        b.finish,
        '\n'.join(format_line(l) for l in b.lines),
    )

def diff(a, b):
    c = lcs(a, b)

    block = []
    i = 0

    for change in changes(a, b, c):
        if len(block) == 0:
            block.append(change)

        elif block[-1][0] == change[0] and block[-1][1] + 1 == change[1]:
            block.append(change)

        else:
            yield compress_block(block)
            block = [change]

    yield compress_block(block)

before_file_path = sys.argv[1]
after_file_path = sys.argv[2]

with open(before_file_path, 'r') as before_file:
    before = tuple(l.rstrip() for l in before_file.readlines())

with open(after_file_path, 'r') as after_file:
    after = tuple(l.rstrip() for l in after_file.readlines())

for block in diff(before, after):
    print(format_block(block))
