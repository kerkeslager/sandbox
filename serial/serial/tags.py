import collections

NULL = 0x00
TRUE = 0x01
FALSE = 0x02
UINT8 = 0x03
UINT16 = 0x04
UINT32 = 0x05
UINT64 = 0x06
INT8 = 0x10
INT16 = 0x11
INT32 = 0x12
INT64 = 0x13
BINARY = 0x20
UTF8 = 0x21
UTF16 = 0x22
UTF32 = 0x23
TUPLE = 0x30
LIST = 0x31

TaggedObject = collections.namedtuple(
    'TaggedObject',
    [
        'tag',
        'instance',
    ],
)
