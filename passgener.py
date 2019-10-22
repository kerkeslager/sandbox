import math, os, sys

PASSWORD_MIN_BITS_ENTROPY = 256

def open_dict():
    try:
        return open('/usr/share/dict/words')
    except FileNotFoundError:
        return open(' /usr/dict/words')

with open_dict() as dict_file:
    dict_words = dict_file.readlines()

dict_word_count = len(dict_words)

passphrase_word_count = math.ceil(math.log(2**PASSWORD_MIN_BITS_ENTROPY, dict_word_count))

bytes_entropy_per_word = math.ceil(math.log(dict_word_count, 2**8))

print(bytes_entropy_per_word)

def rand_word():
    try:
        return dict_words[int.from_bytes(os.urandom(bytes_entropy_per_word), byteorder='little')].strip()
    except IndexError:
        return rand_word()

passphrase = ' '.join(rand_word() for i in range(passphrase_word_count))

print(passphrase)
