import re
import urllib.parse

def sanitizer_1(source):
    items_were_deleted = True

    while items_were_deleted:
        start_length = len(source)

        source = ''.join(re.split(r'<\s*split\s*>', source))
        source = source[:50]
        source = ''.join(source.split('"'))

        items_were_deleted = len(source) < start_length

        source = urllib.parse.unquote(source)

    return source
        
