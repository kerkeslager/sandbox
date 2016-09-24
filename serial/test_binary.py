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
