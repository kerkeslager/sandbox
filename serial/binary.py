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
TAG_BINARY = 0x20
TAG_UTF8 = 0x21
TAG_UTF16 = 0x22
TAG_UTF32 = 0x23
TAG_TUPLE = 0x30

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

def _make_string_serializer(encoder):
    packer = functools.partial(struct.pack, '!BI')

    def serializer(to):
        encoded = encoder(to.instance)
        return packer(to.tag, len(encoded)) + encoded

    return serializer

def _serialize_tuple(to):
    assert isinstance(to.instance, tuple)

    payload = b''.join(serialize(item) for item in to.instance)

    fmt = '!BI'

    return struct.pack('!BI', TAG_TUPLE, len(payload)) + payload


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
    TAG_BINARY: _make_string_serializer(lambda s: s),
    TAG_UTF8: _make_string_serializer(lambda s: s.encode('utf-8')),
    TAG_UTF16: _make_string_serializer(lambda s: s.encode('utf-16')),
    TAG_UTF32: _make_string_serializer(lambda s: s.encode('utf-32')),
    TAG_TUPLE: _serialize_tuple,
}

def serialize(to):
    return _TAGS_TO_SERIALIZERS[to.tag](to)

def _make_tag_only_parser(tag, value):
    def parser(b):
        return 0, TaggedObject(tag = tag, instance = value)

    return parser

def _make_struct_deserializer(tag, fmt):
    fmt = '!' + fmt
    size = struct.calcsize(fmt)
    unpacker = functools.partial(struct.unpack, fmt)

    def parser(b):
        b = b.read(size)
        assert len(b) == size
        return size, TaggedObject(tag = tag, instance = unpacker(b)[0])

    return parser

_LENGTH_FMT = '!I'
_LENGTH_FMT_SIZE = struct.calcsize(_LENGTH_FMT)

def _read_length_then_payload(b):
    length_b = b.read(_LENGTH_FMT_SIZE)
    assert len(length_b) == _LENGTH_FMT_SIZE
    length = struct.unpack(_LENGTH_FMT, length_b)[0]

    payload = b.read(length)
    assert len(payload) == length
    return _LENGTH_FMT_SIZE + length, payload

def _make_string_deserializer(tag, decoder):
    fmt = '!I'
    size = struct.calcsize(fmt)
    unpacker = functools.partial(struct.unpack, fmt)

    def parser(b):
        bytes_read, payload = _read_length_then_payload(b)
        return bytes_read, TaggedObject(tag = tag, instance = decoder(payload))

    return parser

def _deserialize_tuple(b):
    bytes_read, payload = _read_length_then_payload(b)

    payload_stream = io.BytesIO(payload)

    total_bytes_read = 0
    instance = []

    while total_bytes_read < len(payload):
        partial_bytes_read, item = _deserialize_partial(payload_stream)
        total_bytes_read += partial_bytes_read
        instance.append(item)

    return bytes_read, TaggedObject(tag = TAG_TUPLE, instance = tuple(instance))

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
    TAG_BINARY: _make_string_deserializer(TAG_BINARY, lambda b: b),
    TAG_UTF8: _make_string_deserializer(TAG_UTF8, lambda b: b.decode('utf-8')),
    TAG_UTF16: _make_string_deserializer(TAG_UTF16, lambda b: b.decode('utf-16')),
    TAG_UTF32: _make_string_deserializer(TAG_UTF32, lambda b: b.decode('utf-32')),
    TAG_TUPLE: _deserialize_tuple,
}

def _deserialize_partial(b):
    tag = b.read(1)
    assert len(tag) == 1
    bytes_read, to = _TAGS_TO_PARSERS[tag[0]](b)
    return bytes_read + 1, to

def deserialize(b):
    if isinstance(b, bytes):
        b = io.BytesIO(b)

    bytes_read, result = _deserialize_partial(b)

    remainder = b.read()

    if len(remainder) == 0:
        return result

    raise Exception('Unable to parse remainder: {}'.format(remainder))
