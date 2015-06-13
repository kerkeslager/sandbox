import os
import os.path
import subprocess
import unittest

CC = 'gcc-4.9'

FILE_PATH = os.path.dirname(os.path.realpath(__file__))
INTEGRATION_TEST_DIRECTORY_PATH = os.path.join(FILE_PATH, 'integration_tests')
os.chdir(FILE_PATH)

integration_test_file_names = [
    fn
    for fn in os.listdir(INTEGRATION_TEST_DIRECTORY_PATH) \
    if fn.endswith('.stt')
]

C_SOURCE_FILE_PATH = os.path.join(FILE_PATH, 'a.c')
BINARY_PATH = os.path.join(FILE_PATH, 'a.out')

def generate_test(input_file_name):
    input_file_path = os.path.join(INTEGRATION_TEST_DIRECTORY_PATH, input_file_name)
    expected_output_file_path = input_file_path[:-4] + '.txt'

    def test(self):
        c_source = subprocess.check_output(['python','stutter.py',input_file_path])

        self.assertIsNotNone(c_source)
        self.assertNotEqual(0, len(c_source))

        with open(C_SOURCE_FILE_PATH,'wb') as c_source_file:
            c_source_file.write(c_source)

        cc_result = subprocess.call([CC, C_SOURCE_FILE_PATH])

        self.assertEqual(0, cc_result)

        with open(expected_output_file_path, 'rb') as expected_output_file:
            expected_output = expected_output_file.read()

        actual_output = subprocess.check_output(BINARY_PATH)

        self.assertEqual(expected_output, actual_output)

    return test

class IntegrationTests(unittest.TestCase):
    def tearDown(self):
        try:
            os.remove(C_SOURCE_FILE_PATH)

        except OSError as e:
            if e.errno != errno.ENOENT:
                raise

        try:
            os.remove(BINARY_PATH)

        except OSError as e:
            if e.errno != errno.ENOENT:
                raise

for integration_test_file_name in integration_test_file_names:
    test = generate_test(integration_test_file_name)
    test_name = 'test_{}'.format(integration_test_file_name[:-4])
    setattr(IntegrationTests, test_name, test)

unittest.main()
