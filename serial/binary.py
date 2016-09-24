import collections
import functools
import io
import struct

TAG_NULL = 0x00
TAG_TRUE = 0x01
TAG_FALSE = 0x02
TAG_UINT8 = 0x03
TAG_UINT16 = 0x04
TAG_UINT32 = 0x05
TAG_UINT64 = 0x06
TAG_INT8 = 0x10
TAG_INT16 = 0x11
TAG_INT32 = 0x12
TAG_INT64 = 0x13

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

def _make_struct_serializer(fmt):
    fmt = '!B' + fmt
    packer = functools.partial(struct.pack, fmt)

    def serializer(to):
        return packer(to.tag, to.instance)

    return serializer

_TAGS_TO_SERIALIZERS = {
    TAG_NULL: _make_tag_only_serializer(TAG_NULL, None),
    TAG_TRUE: _make_tag_only_serializer(TAG_TRUE, True),
    TAG_FALSE: _make_tag_only_serializer(TAG_FALSE, False),
    TAG_UINT8: _make_struct_serializer('B'),
    TAG_UINT16: _make_struct_serializer('H'),
    TAG_UINT32: _make_struct_serializer('I'),
    TAG_UINT64: _make_struct_serializer('Q'),
    TAG_INT8: _make_struct_serializer('b'),
    TAG_INT16: _make_struct_serializer('h'),
    TAG_INT32: _make_struct_serializer('i'),
    TAG_INT64: _make_struct_serializer('q'),
}

def serialize(to):
    return _TAGS_TO_SERIALIZERS[to.tag](to)

def _make_tag_only_parser(tag, value):
    def parser(b):
        return TaggedObject(tag = tag, instance = value)

    return parser

def _make_struct_deserializer(tag, fmt):
    fmt = '!' + fmt
    size = struct.calcsize(fmt)
    unpacker = functools.partial(struct.unpack, fmt)

    def parser(b):
        b = b.read(size)
        assert len(b) == size
        return TaggedObject(tag = tag, instance = unpacker(b)[0])

    return parser

_TAGS_TO_PARSERS = {
    TAG_NULL: _make_tag_only_parser(TAG_NULL, None),
    TAG_TRUE: _make_tag_only_parser(TAG_TRUE, True),
    TAG_FALSE: _make_tag_only_parser(TAG_FALSE, False),
    TAG_UINT8: _make_struct_deserializer(TAG_UINT8, 'B'),
    TAG_UINT16: _make_struct_deserializer(TAG_UINT16, 'H'),
    TAG_UINT32: _make_struct_deserializer(TAG_UINT32, 'I'),
    TAG_UINT64: _make_struct_deserializer(TAG_UINT64, 'Q'),
    TAG_INT8: _make_struct_deserializer(TAG_INT8, 'b'),
    TAG_INT16: _make_struct_deserializer(TAG_INT16, 'h'),
    TAG_INT32: _make_struct_deserializer(TAG_INT32, 'i'),
    TAG_INT64: _make_struct_deserializer(TAG_INT64, 'q'),
}

def deserialize(b):
    if isinstance(b, bytes):
        b = io.BytesIO(b)

    tag = b.read(1)[0]

    result = _TAGS_TO_PARSERS[tag](b)

    remainder = b.read()

    if len(remainder) == 0:
        return result

    raise Exception('Unable to parse remainder: {}'.format(remainder))
