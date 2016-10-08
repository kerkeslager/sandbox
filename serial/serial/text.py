import binascii
import re

from . import tags

def _make_literal_serializer(expected_value, literal):
    def serializer(to):
        assert to.instance is expected_value
        return literal

    return serializer

def _make_integer_serializer(lower_bound, upper_bound, suffix):
    def _serializer(to):
        assert lower_bound <= to.instance and to.instance < upper_bound
        return '{}{}'.format(to.instance, suffix)

    return _serializer

def _make_unsigned_integer_serializer(bit_length):
    return _make_integer_serializer(0, 2 << (bit_length - 1), 'u{}'.format(bit_length))

def _make_signed_integer_serializer(bit_length):
    upper_bound = 2 << (bit_length - 2)
    lower_bound = -upper_bound
    return _make_integer_serializer(lower_bound, upper_bound, 'i{}'.format(bit_length))

def _serialize_binary(to):
    return 'bin"{}"'.format(binascii.hexlify(to.instance).decode('ascii'))

_ESCAPES = {
    '\\': '\\\\',
    '"': '\\"',
}

def _escape_character(ch):
    return _ESCAPES.get(ch, ch)

def _escape(s):
    return ''.join(_escape_character(ch) for ch in s)

def _make_string_serializer(prefix):
    def serializer(to):
        assert isinstance(to.instance, str)
        return '{}"{}"'.format(prefix, _escape(to.instance))

    return serializer

_SERIALIZERS = {
    tags.NULL: _make_literal_serializer(None, 'null'),
    tags.TRUE: _make_literal_serializer(True, 'true'),
    tags.FALSE: _make_literal_serializer(False, 'false'),
    tags.UINT8: _make_unsigned_integer_serializer(8),
    tags.UINT16: _make_unsigned_integer_serializer(16),
    tags.UINT32: _make_unsigned_integer_serializer(32),
    tags.UINT64: _make_unsigned_integer_serializer(64),
    tags.INT8: _make_signed_integer_serializer(8),
    tags.INT16: _make_signed_integer_serializer(16),
    tags.INT32: _make_signed_integer_serializer(32),
    tags.INT64: _make_signed_integer_serializer(64),
    tags.BINARY: _serialize_binary,
    tags.UTF8: _make_string_serializer('utf8'),
    tags.UTF16: _make_string_serializer('utf16'),
    tags.UTF32: _make_string_serializer('utf32'),
}

def serialize(to):
    return _SERIALIZERS[to.tag](to)

def _make_literal_deserializer(tag, instance, literal):
    def _deserializer(s):
        if s.startswith(literal):
            return True, tags.TaggedObject(tag = tag, instance = instance), s[len(literal):]

        return False, None, None

    return _deserializer

def _make_regex_deserializer(tag, decoder, regex):
    matcher = re.compile(regex).match

    def _deserializer(s):
        match = matcher(s)

        if match is None:
            return False, None, None

        return True, tags.TaggedObject(tag = tag, instance = decoder(match)), s[match.end():]

    return _deserializer

def _make_unsigned_int_deserializer(tag, bit_length):
    bound = 2 << (bit_length - 1)

    def _decoder(match):
        result = int(match.group(1))
        assert result < bound
        return result

    return _make_regex_deserializer(tag, _decoder, r'(\d+)' + 'u{}'.format(bit_length))

def _make_signed_int_deserializer(tag, bit_length):
    upper_bound = 2 << (bit_length - 2)
    lower_bound = -upper_bound

    def _decoder(match):
        result = int(match.group(1))
        assert lower_bound <= result and result < upper_bound
        return result

    return _make_regex_deserializer(tag, _decoder, r'(-?\d+)' + 'i{}'.format(bit_length))

_BINARY_MATCHER = re.compile(r'bin"([\da-f]*)"').match

def _deserialize_binary(s):
    match = _BINARY_MATCHER(s)

    if match is None:
        return False, None, None

    result = tags.TaggedObject(
        tag = tags.BINARY,
        instance = binascii.unhexlify(match.group(1)),
    )

    return True, result, s[match.end():]

def _make_string_matcher(prefix):
    return re.compile(prefix + r'"(([^"]|\\.)*)"').match

_UNESCAPE_CHARACTERS = {
    '\\': '\\',
    '"': '"',
}

def _unescape_character(ch):
    return _UNESCAPE_CHARACTERS[ch]

def _unescape(s):
    characters = []
    escaping = False

    for ch in s:
        if escaping:
            characters.append(_unescape_character(ch))
            escaping = False

        elif ch == '\\':
            escaping = True

        else:
            characters.append(ch)

    return ''.join(characters)

def _make_string_deserializer(tag, prefix):
    matcher = _make_string_matcher(prefix)

    def deserializer(s):
        match = matcher(s)

        if match is None:
            return False, None, None

        result = tags.TaggedObject(
            tag = tag,
            instance = _unescape(match.group(1)),
        )

        return True, result, s[match.end():]

    return deserializer

_DESERIALIZERS = [
    _make_literal_deserializer(tags.NULL, None, 'null'),
    _make_literal_deserializer(tags.TRUE, True, 'true'),
    _make_literal_deserializer(tags.FALSE, False, 'false'),
    _make_unsigned_int_deserializer(tags.UINT8, 8),
    _make_unsigned_int_deserializer(tags.UINT16, 16),
    _make_unsigned_int_deserializer(tags.UINT32, 32),
    _make_unsigned_int_deserializer(tags.UINT64, 64),
    _make_signed_int_deserializer(tags.INT8, 8),
    _make_signed_int_deserializer(tags.INT16, 16),
    _make_signed_int_deserializer(tags.INT32, 32),
    _make_signed_int_deserializer(tags.INT64, 64),
    _deserialize_binary,
    _make_string_deserializer(tags.UTF8, 'utf8'),
    _make_string_deserializer(tags.UTF16, 'utf16'),
    _make_string_deserializer(tags.UTF32, 'utf32'),
]

def deserialize(s):
    for deserializer in _DESERIALIZERS:
        succeeded, result, remaining = deserializer(s)

        if succeeded:
            assert remaining == ''
            return result

    raise Exception()
