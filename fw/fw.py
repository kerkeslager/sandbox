import collections
from wsgiref.util import setup_testing_defaults

Request = collections.namedtuple(
    'Request',
    [
        'method',
        'path',
        'content_type',
        'content_length',
        'content',
        'headers',
        'environment',
    ],
)

def _environment_to_headers(environment):
    result = collections.OrderedDict()

    for key, value in environment.items():
        if key.startswith('HTTP_'):
            key = key[len('HTTP_'):].lower()
            result[key.lower()] = value

    return result

def request(method, path, **kwargs):
    return Request(
        method = method,
        path = path,
        content_type = kwargs.get('content_type', 'text/plain'),
        content_length = kwargs.get('content_length', 0),
        content = kwargs.get('content', ''),
        headers = kwargs.get('headers', collections.OrderedDict()),
        environment = kwargs.get('environment', {}),
    )

Response = collections.namedtuple(
    'Response',
    [
        'status',
        'headers',
        'content',
        'encoding',
    ],
)

def response(**kwargs):
    return Response(
        status = kwargs.get('status', 200),
        headers = kwargs.get('headers', [('Content-type', 'text/plain; charset=utf-8')]),
        content = kwargs.get('content', ''),
        encoding = kwargs.get('encoding', 'utf-8'),
    )

# From https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
_STATUS_CODES_TO_STRINGS = {
    200: '200 OK',
    404: '404 Not Found',
    405: '405 Method Not Allowed',
}

def _status_code_to_string(status_code):
    return _STATUS_CODES_TO_STRINGS[status_code]

def _wrap_content(content_iterator, encoding):
    if isinstance(content_iterator, str):
        content_iterator = content_iterator.encode(encoding)
        yield content_iterator
        return

    if isinstance(content_iterator, bytes):
        yield content_iterator
        return

    for content_item in content_iterator:
        if isinstance(content_item, str):
            yield content_item.encode(encoding)

        else:
            yield content_item

def application(request_handler):
    def wrapped_request_handler(environment, start_response):
        setup_testing_defaults(environment)

        content_type = environment['CONTENT_TYPE']
        content_length = environment['CONTENT_LENGTH']

        if environment['CONTENT_LENGTH'] == '':
            content_length = 0
            content = ''

        else:
            content_length = int(environment['CONTENT_LENGTH'])
            content = environment['wsgi.input'].read(content_length)

        if content_length == '':
            content_length = len(content)

        else:
            content_length = int(content_length)

        result = request_handler(request(
            environment['REQUEST_METHOD'],
            environment['PATH_INFO'],
            content_type = content_type,
            content_length = content_length,
            content = content,
            headers = _environment_to_headers(environment),
            environment = environment,
        ))

        start_response(
            _status_code_to_string(result.status),
            result.headers,
        )

        return _wrap_content(result.content, result.encoding)

    return wrapped_request_handler

def _route_matcher(route):
    def matcher(path):
        if route == path:
            return True, ()

        return False, None

    return matcher

def path_router(*routes_to_handlers, **kwargs):
    matchers_to_handlers = []

    for route, handler in routes_to_handlers:
        matcher = _route_matcher(route)
        matchers_to_handlers.append((matcher, handler))

    defined_routes = [route for route, handler in routes_to_handlers]
    def default_not_found_handler(request):
        content = 'FILE {} NOT FOUND\n'.format(request.path)
        content += 'The following routes are defined for this router:\n'
        content += '\n'.join(defined_routes)
        return response(status = 404, content = content)

    not_found_handler = kwargs.pop('not_found_handler', default_not_found_handler)

    if any(kwargs):
        raise Exception('Unexpected keyword argument "{}"'.format(list(kwargs.keys())[0]))

    def route(request):
        for matcher, handler in matchers_to_handlers:
            matched, args = matcher(request.path)
            if matched:
                return handler(request, *args)

        return not_found_handler(request)

    return route

# https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
_REQUEST_METHODS = ['OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT']

def method_router(**kwargs):
    allowed_methods = {}

    for request_method in _REQUEST_METHODS:
        if request_method in kwargs:
            allowed_methods[request_method] = kwargs.pop(request_method)

    auto_options = kwargs.pop('auto_options', True)

    def default_options_handler(request):
        return response(
            status = 200,
            headers = [
                ('Allow', ', '.join(allowed_methods.keys())),
                ('Content-Length', '0'),
            ],
        )

    if auto_options and 'OPTIONS' not in allowed_methods:
        allowed_methods['OPTIONS'] = default_options_handler

    def default_method_not_allowed_handler(request):
        content = 'METHOD "{}" NOT ALLOWED\n'.format(request.method)
        content += 'The following methods are allowed for this resource:\n'
        content += '\n'.join(allowed_methods.keys())
        return response(status = 405, content = content)

    method_not_allowed_handler = kwargs.pop(
        'method_not_allowed_handler',
        default_method_not_allowed_handler,
    )

    if any(kwargs):
        raise Exception('Unexpected keyword argument "{}"'.format(list(kwargs.keys())[0]))

    def route(request):
        return allowed_methods.get(request.method, method_not_allowed_handler)(request)

    return route


if __name__ == '__main__':
    import unittest
    from unittest import mock

    class PathRouterTests(unittest.TestCase):
        def test_routes_to_handler(self):
            path = '/path'
            expected_request = request('GET', path)
            expected_response = response(content = 'Expected')
            handler = mock.MagicMock()
            handler.return_value = expected_response

            router = path_router(
                ('/path',   handler),
            )
            actual_response = router(expected_request)

            handler.assert_called_with(expected_request)
            self.assertEqual(expected_response, actual_response)

        def test_routes_to_first_matching_handler(self):
            path = '/path'
            expected_request = request('GET', path)
            expected_response = response(content = 'Expected')
            expected_handler = mock.MagicMock()
            expected_handler.return_value = expected_response
            unexpected_handler = mock.MagicMock()

            router = path_router(
                ('/path',   expected_handler),
                ('/path',   unexpected_handler),
            )
            actual_response = router(expected_request)

            expected_handler.assert_called_with(expected_request)
            self.assertEqual(expected_response, actual_response)

    unittest.main()
