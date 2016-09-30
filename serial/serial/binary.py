import functools
import io
import struct

from . import tags

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

    return struct.pack('!BI', tags.TUPLE, len(payload)) + payload


_TAGS_TO_SERIALIZERS = {
    tags.NULL: _make_tag_only_serializer(tags.NULL, None),
    tags.TRUE: _make_tag_only_serializer(tags.TRUE, True),
    tags.FALSE: _make_tag_only_serializer(tags.FALSE, False),
    tags.UINT8: _make_struct_serializer('B'),
    tags.UINT16: _make_struct_serializer('H'),
    tags.UINT32: _make_struct_serializer('I'),
    tags.UINT64: _make_struct_serializer('Q'),
    tags.INT8: _make_struct_serializer('b'),
    tags.INT16: _make_struct_serializer('h'),
    tags.INT32: _make_struct_serializer('i'),
    tags.INT64: _make_struct_serializer('q'),
    tags.BINARY: _make_string_serializer(lambda s: s),
    tags.UTF8: _make_string_serializer(lambda s: s.encode('utf-8')),
    tags.UTF16: _make_string_serializer(lambda s: s.encode('utf-16')),
    tags.UTF32: _make_string_serializer(lambda s: s.encode('utf-32')),
    tags.TUPLE: _serialize_tuple,
}

def serialize(to):
    return _TAGS_TO_SERIALIZERS[to.tag](to)

def _make_tag_only_parser(tag, value):
    def parser(b):
        return 0, tags.TaggedObject(tag = tag, instance = value)

    return parser

def _make_struct_deserializer(tag, fmt):
    fmt = '!' + fmt
    size = struct.calcsize(fmt)
    unpacker = functools.partial(struct.unpack, fmt)

    def parser(b):
        b = b.read(size)
        assert len(b) == size
        return size, tags.TaggedObject(tag = tag, instance = unpacker(b)[0])

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
        return bytes_read, tags.TaggedObject(tag = tag, instance = decoder(payload))

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

    return bytes_read, tags.TaggedObject(tag = tags.TUPLE, instance = tuple(instance))

_TAGS_TO_PARSERS = {
    tags.NULL: _make_tag_only_parser(tags.NULL, None),
    tags.TRUE: _make_tag_only_parser(tags.TRUE, True),
    tags.FALSE: _make_tag_only_parser(tags.FALSE, False),
    tags.UINT8: _make_struct_deserializer(tags.UINT8, 'B'),
    tags.UINT16: _make_struct_deserializer(tags.UINT16, 'H'),
    tags.UINT32: _make_struct_deserializer(tags.UINT32, 'I'),
    tags.UINT64: _make_struct_deserializer(tags.UINT64, 'Q'),
    tags.INT8: _make_struct_deserializer(tags.INT8, 'b'),
    tags.INT16: _make_struct_deserializer(tags.INT16, 'h'),
    tags.INT32: _make_struct_deserializer(tags.INT32, 'i'),
    tags.INT64: _make_struct_deserializer(tags.INT64, 'q'),
    tags.BINARY: _make_string_deserializer(tags.BINARY, lambda b: b),
    tags.UTF8: _make_string_deserializer(tags.UTF8, lambda b: b.decode('utf-8')),
    tags.UTF16: _make_string_deserializer(tags.UTF16, lambda b: b.decode('utf-16')),
    tags.UTF32: _make_string_deserializer(tags.UTF32, lambda b: b.decode('utf-32')),
    tags.TUPLE: _deserialize_tuple,
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
