import unittest

from serial import binary, tags, text

EXAMPLE_BINARY_REPRESENTATIONS = [
    (tags.TaggedObject(tags.NULL, None), b'\x00'),
    (tags.TaggedObject(tags.TRUE, True), b'\x01'),
    (tags.TaggedObject(tags.FALSE, False), b'\x02'),
    (tags.TaggedObject(tags.UINT8, 7), b'\x03\x07'),
    (tags.TaggedObject(tags.UINT16, 7), b'\x04\x00\x07'),
    (tags.TaggedObject(tags.UINT32, 7), b'\x05\x00\x00\x00\x07'),
    (tags.TaggedObject(tags.UINT64, 7), b'\x06\x00\x00\x00\x00\x00\x00\x00\x07'),
    (tags.TaggedObject(tags.INT8, 7), b'\x10\x07'),
    (tags.TaggedObject(tags.INT16, 7), b'\x11\x00\x07'),
    (tags.TaggedObject(tags.INT32, 7), b'\x12\x00\x00\x00\x07'),
    (tags.TaggedObject(tags.INT64, 7), b'\x13\x00\x00\x00\x00\x00\x00\x00\x07'),
    (tags.TaggedObject(tags.UINT8, 254), b'\x03\xfe'),
    (tags.TaggedObject(tags.UINT16, 65534), b'\x04\xff\xfe'),
    (tags.TaggedObject(tags.UINT32, 4294967294), b'\x05\xff\xff\xff\xfe'),
    (tags.TaggedObject(tags.UINT64, 18446744073709551614), b'\x06\xff\xff\xff\xff\xff\xff\xff\xfe'),
    (tags.TaggedObject(tags.INT8, -2), b'\x10\xfe'),
    (tags.TaggedObject(tags.INT16, -2), b'\x11\xff\xfe'),
    (tags.TaggedObject(tags.INT32, -2), b'\x12\xff\xff\xff\xfe'),
    (tags.TaggedObject(tags.INT64, -2), b'\x13\xff\xff\xff\xff\xff\xff\xff\xfe'),
    (tags.TaggedObject(tags.BINARY, b'\xde\xad\xbe\xef'), b'\x20\x00\x00\x00\x04\xde\xad\xbe\xef'),
    (tags.TaggedObject(tags.UTF8, 'Lol!'), b'\x21\x00\x00\x00\x04Lol!'),
    (tags.TaggedObject(tags.UTF16, 'かわ'), b'\x22\x00\x00\x00\x06\xff\xfeK0\x8f0'),
    (tags.TaggedObject(tags.UTF32, '漢'), b'\x23\x00\x00\x00\x08\xff\xfe\x00\x00"o\x00\x00'),
    (
        tags.TaggedObject(
            tags.TUPLE,
            (
                tags.TaggedObject(
                    tags.TRUE,
                    True,
                ),
                tags.TaggedObject(
                    tags.UINT8,
                    7,
                ),
            ),
        ),
        b'\x30\x00\x00\x00\x03\x01\x03\x07'
    ),
    (
        tags.TaggedObject(
            tag = tags.LIST,
            instance = [
                tags.TaggedObject(tag = tags.UINT8, instance = 9),
                tags.TaggedObject(tag = tags.UINT8, instance = 22),
                tags.TaggedObject(tag = tags.UINT8, instance = 36),
            ],
        ),
        b'\x31\x03\x00\x00\x00\x03\x09\x16\x24',
    ),
    (
        tags.TaggedObject(
            tag = tags.OBJECT,
            instance = [
                (
                    tags.TaggedObject(tag = tags.UTF8, instance = 'foo'),
                    tags.TaggedObject(tag = tags.UTF8, instance = 'bar'),
                ),
                (
                    tags.TaggedObject(tag = tags.UTF8, instance = 'baz'),
                    tags.TaggedObject(tag = tags.UINT8, instance = 42),
                ),
            ],
        ),
        b'\x32\x21\x00\x00\x00\x18\x00\x00\x00\x03foo\x21\x00\x00\x00\x03bar\x00\x00\x00\x03baz\x03\x2a',
    ),
]

class BinarySerializeTests(unittest.TestCase):
    def test_serialize(self):
        for tagged_object, expected in EXAMPLE_BINARY_REPRESENTATIONS:
            actual = binary.serialize(tagged_object)
            self.assertEqual(expected, actual)

class BinaryDeserializeTests(unittest.TestCase):
    def test_deserialize(self):
        for expected, representation in EXAMPLE_BINARY_REPRESENTATIONS:
            actual = binary.deserialize(representation)
            self.assertEqual(expected, actual)

EXAMPLE_TEXT_REPRESENTATIONS = [
    (tags.TaggedObject(tags.NULL, None), 'null'),
    (tags.TaggedObject(tags.TRUE, True), 'true'),
    (tags.TaggedObject(tags.FALSE, False), 'false'),
    (tags.TaggedObject(tags.UINT8, 42), '42u8'),
    (tags.TaggedObject(tags.UINT16, 42), '42u16'),
    (tags.TaggedObject(tags.UINT32, 42), '42u32'),
    (tags.TaggedObject(tags.UINT64, 42), '42u64'),
    (tags.TaggedObject(tags.INT8, 42), '42i8'),
    (tags.TaggedObject(tags.INT16, 42), '42i16'),
    (tags.TaggedObject(tags.INT32, 42), '42i32'),
    (tags.TaggedObject(tags.INT64, 42), '42i64'),
    (tags.TaggedObject(tags.INT8, -2), '-2i8'),
    (tags.TaggedObject(tags.INT16, -2), '-2i16'),
    (tags.TaggedObject(tags.INT32, -2), '-2i32'),
    (tags.TaggedObject(tags.INT64, -2), '-2i64'),
    (tags.TaggedObject(tags.BINARY, b'\x42\xde\xad\xbe\xef'), 'bin"42deadbeef"'),
    (tags.TaggedObject(tags.UTF8, 'Lol!'), 'utf8"Lol!"'),
    (tags.TaggedObject(tags.UTF16, 'かわ'), 'utf16"かわ"'),
    (tags.TaggedObject(tags.UTF32, '漢'), 'utf32"漢"'),
    (
        tags.TaggedObject(
            tag = tags.LIST,
            instance = [
                tags.TaggedObject(tag = tags.UINT8, instance = 9),
                tags.TaggedObject(tag = tags.UINT8, instance = 22),
                tags.TaggedObject(tag = tags.UINT8, instance = 36),
            ],
        ),
        '[\n  9u8,\n  22u8,\n  36u8\n]'
    ),
    (
        tags.TaggedObject(
            tag = tags.OBJECT,
            instance = [
                (
                    tags.TaggedObject(tag = tags.UTF8, instance = 'foo'),
                    tags.TaggedObject(tag = tags.UTF8, instance = 'bar'),
                ),
                (
                    tags.TaggedObject(tag = tags.UTF8, instance = 'baz'),
                    tags.TaggedObject(tag = tags.UINT8, instance = 42),
                ),
            ],
        ),
        '{\n  utf8"foo": utf8"bar",\n  utf8"baz": 42u8\n}',
    ),
]

class TextSerializeTests(unittest.TestCase):
    def test_serialize(self):
        for tagged_object, expected in EXAMPLE_TEXT_REPRESENTATIONS:
            actual = text.serialize(tagged_object)
            self.assertEqual(expected, actual)

class TextDeserializeTests(unittest.TestCase):
    def test_deserialize(self):
        for expected, representation in EXAMPLE_TEXT_REPRESENTATIONS:
            actual = text.deserialize(representation)
            self.assertEqual(expected, actual)

unittest.main()
