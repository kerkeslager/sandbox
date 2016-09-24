import unittest

import binary

EXAMPLE_REPRESENTATIONS = [
    (binary.TAG_NULL, None, b'\x00'),
    (binary.TAG_TRUE, True, b'\x01'),
    (binary.TAG_FALSE, False, b'\x02'),
    (binary.TAG_UINT8, 7, b'\x03\x07'),
    (binary.TAG_UINT16, 7, b'\x04\x00\x07'),
    (binary.TAG_UINT32, 7, b'\x05\x00\x00\x00\x07'),
    (binary.TAG_UINT64, 7, b'\x06\x00\x00\x00\x00\x00\x00\x00\x07'),
    (binary.TAG_INT8, 7, b'\x10\x07'),
    (binary.TAG_INT16, 7, b'\x11\x00\x07'),
    (binary.TAG_INT32, 7, b'\x12\x00\x00\x00\x07'),
    (binary.TAG_INT64, 7, b'\x13\x00\x00\x00\x00\x00\x00\x00\x07'),
    (binary.TAG_UINT8, 254, b'\x03\xfe'),
    (binary.TAG_UINT16, 65534, b'\x04\xff\xfe'),
    (binary.TAG_UINT32, 4294967294, b'\x05\xff\xff\xff\xfe'),
    (binary.TAG_UINT64, 18446744073709551614, b'\x06\xff\xff\xff\xff\xff\xff\xff\xfe'),
    (binary.TAG_INT8, -2, b'\x10\xfe'),
    (binary.TAG_INT16, -2, b'\x11\xff\xfe'),
    (binary.TAG_INT32, -2, b'\x12\xff\xff\xff\xfe'),
    (binary.TAG_INT64, -2, b'\x13\xff\xff\xff\xff\xff\xff\xff\xfe'),
]

class SerializeTests(unittest.TestCase):
    def test_serialize(self):
        for tag, instance, representation in EXAMPLE_REPRESENTATIONS:
            self.assertEqual(
                binary.serialize(binary.TaggedObject(
                    tag = tag,
                    instance = instance,
                )),
                representation,
            )

class DeserializeTests(unittest.TestCase):
    def test_deserialize(self):
        for tag, instance, representation in EXAMPLE_REPRESENTATIONS:
            self.assertEqual(
                binary.deserialize(representation),
                binary.TaggedObject(
                    tag = tag,
                    instance = instance,
                ),
            )

unittest.main()
