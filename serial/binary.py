import collections

TAG_NULL = 0x00
TAG_TRUE = 0x01
TAG_FALSE = 0x02

TaggedObject = collections.namedtuple(
    'TaggedObject',
    [
        'tag',
        'instance',
    ],
)

def _make_tag_only_serializer(tag, expected_value):
    tag = bytes([tag])

    def serializer(to):
        assert to.instance == expected_value
        return tag

    return serializer

_TAGS_TO_SERIALIZERS = {
    TAG_NULL: _make_tag_only_serializer(TAG_NULL, None),
    TAG_TRUE: _make_tag_only_serializer(TAG_TRUE, True),
    TAG_FALSE: _make_tag_only_serializer(TAG_FALSE, False),
}

def serialize(to):
    return _TAGS_TO_SERIALIZERS[to.tag](to)

def _make_tag_only_parser(tag, value):
    def parser(b):
        return TaggedObject(tag = tag, instance = value), b

    return parser

_TAGS_TO_PARSERS = {
    TAG_NULL: _make_tag_only_parser(TAG_NULL, None),
    TAG_TRUE: _make_tag_only_parser(TAG_TRUE, True),
    TAG_FALSE: _make_tag_only_parser(TAG_FALSE, False),
}

def deserialize(b):
    result, remainder = _TAGS_TO_PARSERS[b[0]](b[1:])

    if len(remainder) == 0:
        return result

    raise Exception('Unable to parse remainder: {}'.format(remainder))
