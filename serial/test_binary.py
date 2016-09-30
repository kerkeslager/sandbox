import unittest

from serial import binary, tag

EXAMPLE_REPRESENTATIONS = [
    (binary.TaggedObject(tag.NULL, None), b'\x00'),
    (binary.TaggedObject(tag.TRUE, True), b'\x01'),
    (binary.TaggedObject(tag.FALSE, False), b'\x02'),
    (binary.TaggedObject(tag.UINT8, 7), b'\x03\x07'),
    (binary.TaggedObject(tag.UINT16, 7), b'\x04\x00\x07'),
    (binary.TaggedObject(tag.UINT32, 7), b'\x05\x00\x00\x00\x07'),
    (binary.TaggedObject(tag.UINT64, 7), b'\x06\x00\x00\x00\x00\x00\x00\x00\x07'),
    (binary.TaggedObject(tag.INT8, 7), b'\x10\x07'),
    (binary.TaggedObject(tag.INT16, 7), b'\x11\x00\x07'),
    (binary.TaggedObject(tag.INT32, 7), b'\x12\x00\x00\x00\x07'),
    (binary.TaggedObject(tag.INT64, 7), b'\x13\x00\x00\x00\x00\x00\x00\x00\x07'),
    (binary.TaggedObject(tag.UINT8, 254), b'\x03\xfe'),
    (binary.TaggedObject(tag.UINT16, 65534), b'\x04\xff\xfe'),
    (binary.TaggedObject(tag.UINT32, 4294967294), b'\x05\xff\xff\xff\xfe'),
    (binary.TaggedObject(tag.UINT64, 18446744073709551614), b'\x06\xff\xff\xff\xff\xff\xff\xff\xfe'),
    (binary.TaggedObject(tag.INT8, -2), b'\x10\xfe'),
    (binary.TaggedObject(tag.INT16, -2), b'\x11\xff\xfe'),
    (binary.TaggedObject(tag.INT32, -2), b'\x12\xff\xff\xff\xfe'),
    (binary.TaggedObject(tag.INT64, -2), b'\x13\xff\xff\xff\xff\xff\xff\xff\xfe'),
    (binary.TaggedObject(tag.BINARY, b'\xde\xad\xbe\xef'), b'\x20\x00\x00\x00\x04\xde\xad\xbe\xef'),
    (binary.TaggedObject(tag.UTF8, 'Lol!'), b'\x21\x00\x00\x00\x04Lol!'),
    (binary.TaggedObject(tag.UTF16, 'かわ'), b'\x22\x00\x00\x00\x06\xff\xfeK0\x8f0'),
    (binary.TaggedObject(tag.UTF32, '漢'), b'\x23\x00\x00\x00\x08\xff\xfe\x00\x00"o\x00\x00'),
    (
        binary.TaggedObject(
            tag.TUPLE,
            (
                binary.TaggedObject(
                    tag.TRUE,
                    True,
                ),
                binary.TaggedObject(
                    tag.UINT8,
                    7,
                ),
            ),
        ),
        b'\x30\x00\x00\x00\x03\x01\x03\x07'
    ),
]

class SerializeTests(unittest.TestCase):
    def test_serialize(self):
        for tagged_object, expected in EXAMPLE_REPRESENTATIONS:
            actual = binary.serialize(tagged_object)
            self.assertEqual(expected, actual)

class DeserializeTests(unittest.TestCase):
    def test_deserialize(self):
        for expected, representation in EXAMPLE_REPRESENTATIONS:
            actual = binary.deserialize(representation)
            self.assertEqual(expected, actual)

unittest.main()
