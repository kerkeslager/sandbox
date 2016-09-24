import unittest

import binary

class SerializeTests(unittest.TestCase):
    def test_serializes_tag_only_types(self):
        self.assertEqual(
            binary.serialize(binary.TaggedObject(
                tag = binary.TAG_NULL,
                instance = None,
            )),
            b'\x00',
        )
        self.assertEqual(
            binary.serialize(binary.TaggedObject(
                tag = binary.TAG_TRUE,
                instance = True,
            )),
            b'\x01',
        )
        self.assertEqual(
            binary.serialize(binary.TaggedObject(
                tag = binary.TAG_FALSE,
                instance = False,
            )),
            b'\x02',
        )

class DeserializeTests(unittest.TestCase):
    def test_deserializes_tag_only_types(self):
        self.assertEqual(
            binary.deserialize(b'\x00'),
            binary.TaggedObject(
                tag = binary.TAG_NULL,
                instance = None,
            ),
        )
        self.assertEqual(
            binary.deserialize(b'\x01'),
            binary.TaggedObject(
                tag = binary.TAG_TRUE,
                instance = True,
            ),
        )
        self.assertEqual(
            binary.deserialize(b'\x02'),
            binary.TaggedObject(
                tag = binary.TAG_FALSE,
                instance = False,
            )
        )

unittest.main()
