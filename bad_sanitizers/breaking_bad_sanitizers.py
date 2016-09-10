import unittest
import urllib.parse

import bad_sanitizers

class TestBreakingStrings(unittest.TestCase):
    def test_breaking_string_for_bad_sanitizer_1(self):
        desired_result = '"><script>alert("foo")</script>'

        breaking_string = '%22>%3Cscript>alert(%22foo%22)</script>'

        print(breaking_string)

        self.assertEqual(
            bad_sanitizers.sanitizer_1(breaking_string),
            desired_result,
        )

unittest.main()
