#!/usr/bin/env python

'''
To run this file:

    python stutter.py stutter_code.stt > c_code.c
'''

import re
import string

# Utility functions

def is_integer(s_expression):
    return isinstance(s_expression, int) \
        and not s_expression is True \
        and not s_expression is False

# String to s-expressions

TOKEN = re.compile(r'\s*({})'.format('|'.join('(?P<{}>{})'.format(*token) for token in [
    ('open_parenthese',         r'\('),
    ('close_parenthese',        r'\)'),
    ('integer_literal',         r'\d+'),
    ('unexpected_character',    r'.'),
])))

def parse_all(source):
    stack = []
    items = []

    for token in TOKEN.finditer(source):
        if token.group('open_parenthese'):
            stack.append(items)
            items = []

        elif token.group('close_parenthese'):
            if len(stack) == 0:
                raise Exception('Parenthese closed but not opened')

            stack[-1].append(tuple(items))
            items = stack.pop()

        elif token.group('integer_literal'):
            items.append(int(token.group('integer_literal')))

        elif token.group('unexpected_character'):
            raise Exception('Unexpected character {}'.format(
                token.group('unexpected_character'),
            ))

        else:
            raise Exception()


    if len(stack) > 0:
        raise Exception('Parenthese opened but not closed')

    return items

# C AST Objects

class CType(object):
    def __init__(self, name):
        self.name = name

class CPointerType(CType):
    def __init__(self, pointer_to):
        self.pointer_to = pointer_to

class CArgumentDeclaration(object):
    def __init__(self, type, name):
        assert isinstance(type, CType)
        self.type = type
        self.name = name

class CExpression(object):
    pass

class CIntegerLiteralExpression(CExpression):
    def __init__(self, integer):
        assert isinstance(integer, int)

        # Booleans in Python are integers but we don't want them
        assert not integer is True
        assert not integer is False

        self.integer = integer

class CFunctionCallExpression(CExpression):
    def __init__(self, name, arguments):
        assert all(isinstance(argument, CExpression) for argument in arguments)
        self.name = name
        self.arguments = arguments

class CStatement(object):
    pass

class CExpressionStatement(CStatement):
    def __init__(self, expression):
        self.expression = expression

class CReturnStatement(CStatement):
    def __init__(self, expression):
        self.expression = expression

class CFunctionDeclaration(object):
    def __init__(self, return_type, name, argument_declaration_list, body):
        assert isinstance(return_type, CType)
        assert isinstance(argument_declaration_list, list)
        assert all(isinstance(ad, CArgumentDeclaration) for ad in argument_declaration_list)
        assert isinstance(body, list)
        assert all(isinstance(s, CStatement) for s in body)

        self.return_type = return_type
        self.name = name
        self.argument_declaration_list = argument_declaration_list
        self.body = body

# BEGIN S-expression to C AST layer

def evaluate_to_c(s_expression):
    if is_integer(s_expression):
        return CIntegerLiteralExpression(s_expression)

    raise Exception('Unable to evaluate expression {} to C'.format(s_expression))

def evaluate_all_to_c(s_expressions):
    c_expressions = list(map(evaluate_to_c, s_expressions))
    body = list(map(CExpressionStatement, c_expressions[:-1])) + [CReturnStatement(c_expressions[-1])]
    
    return CFunctionDeclaration(
            CType('int'),
            'main',
            [
                CArgumentDeclaration(CType('int'), 'argc'),
                CArgumentDeclaration(CPointerType(CPointerType(CType('char'))), 'argv'),
            ],
            body,
        )

# BEGIN C AST to C source layer

TAB_WIDTH = 2

def indent(string):
    assert isinstance(string, str)

    def indent_line(line):
        line = line.rstrip()

        if line == '':
            return line

        return ' ' * TAB_WIDTH + line

    return '\n'.join(indent_line(line) for line in string.splitlines())

def generate_pointer_type(pointer_type):
    assert isinstance(pointer_type, CPointerType)
    return '{}*'.format(generate_type(pointer_type.pointer_to))

def generate_type(
        type,
        generate_pointer_type = generate_pointer_type):
    assert isinstance(type, CType)

    if isinstance(type, CPointerType):
        return generate_pointer_type(type)

    return type.name

def generate_argument_declaration(argument_declaration):
    assert isinstance(argument_declaration, CArgumentDeclaration)
    return '{} {}'.format(generate_type(argument_declaration.type), argument_declaration.name)

def generate_argument_declaration_list(argument_declarations):
    return ', '.join(generate_argument_declaration(ad) for ad in argument_declarations)

def generate_integer_literal_expression(expression):
    assert isinstance(expression, CIntegerLiteralExpression)
    return str(expression.integer)

def generate_function_call_expression(expression):
    assert isinstance(expression, CFunctionCallExpression)
    return '{}({})'.format(
        expression.name,
        ', '.join(generate_expression(e) for e in expression.arguments),
    )

def generate_expression(
        expression,
        generate_integer_literal_expression = generate_integer_literal_expression,
        generate_function_call_expression = generate_function_call_expression,
        ):

    if isinstance(expression, CIntegerLiteralExpression):
        return generate_integer_literal_expression(expression)

    if isinstance(expression, CFunctionCallExpression):
        return generate_function_call_expression(expression)

    raise Exception('Expression type {} not implemented'.format(type(expression)))

def generate_expression_statement(statement):
    return '{};'.format(generate_expression(statement.expression))

def generate_return_statement(statement):
    return 'return {};'.format(generate_expression(statement.expression))

def generate_statement(
        statement,
        generate_expression_statement = generate_expression_statement,
        generate_return_statement = generate_return_statement):

    if isinstance(statement, CExpressionStatement):
        return generate_expression_statement(statement)

    if isinstance(statement, CReturnStatement):
        return generate_return_statement(statement)

    raise Exception('Handling for statements of type {} not implemented'.format(type(statement.type)))

def generate_statement_list(statements):
    assert all(isinstance(s, CStatement) for s in statements)
    return '\n'.join(generate_statement(s) for s in statements)

FUNCTION_DEFINITION_TEMPLATE = string.Template(
'''
$return_type $name($argument_declaration_list)
{
$body
}
'''.strip())

def generate_function_declaration(function_declaration):
    assert isinstance(function_declaration, CFunctionDeclaration)
    return FUNCTION_DEFINITION_TEMPLATE.substitute(
        return_type = generate_type(function_declaration.return_type),
        name = function_declaration.name,
        argument_declaration_list = generate_argument_declaration_list(function_declaration.argument_declaration_list),
        body = indent(generate_statement_list(function_declaration.body)),
    )

if __name__ == '__main__':
    import sys
    source_file_name = sys.argv[1]

    with open(source_file_name, 'r') as source_file:
        source = source_file.read()

    result = generate_function_declaration(evaluate_all_to_c(parse_all(source)))
    print(result)
