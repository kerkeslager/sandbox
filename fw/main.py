import fw

from wsgiref.simple_server import make_server

def index_get(request):
    return fw.response(content = 'Hello, world')

index = fw.method_router(
    GET = index_get,
)

def request_printer(request):
    ret = [("%s: %s\n" % (key, value)) for key, value in request.environment.items()]

    return fw.response(
        content=ret,
    )

simple_app = fw.path_router(
    ('/',                 index),
    ('/index',            index),
    ('/request_printer',  request_printer),
)

app = fw.application(simple_app)

httpd = make_server('', 8000, app)
print("Serving on port 8000...")
httpd.serve_forever()
