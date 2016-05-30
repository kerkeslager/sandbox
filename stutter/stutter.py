#!/usr/bin/env python

'''
To run this file:

    python stutter.py stutter_code.stt > c_code.c
'''

import itertools
import re
import string

# Utility functions

def is_integer(s_expression):
    return isinstance(s_expression, int) \
        and not s_expression is True \
        and not s_expression is False

ESCAPE_CHARACTERS = {
    '\\'    : '\\',
    'n'     : '\n',
}

def undelimit_string(s):
    assert len(s) >= 2

    delimiter = s[0]
    assert delimiter == '"' # This is temporary, " is currently the only delimiter
    assert s[-1] == delimiter

    escape_characters = dict(ESCAPE_CHARACTERS)
    escape_characters[delimiter] = delimiter

    s = s[1:-1]

    index = 0
    result = ''

    while index < len(s):
        ch = s[index]

        if ch == '\\':
            index += 1

            # TODO Handle when it's not a valid escape character
            ch = escape_characters[s[index]]
            
        index += 1
        result += ch

    return result

TAB_WIDTH = 4

def indent(string):
    assert isinstance(string, str)

    def indent_line(line):
        line = line.rstrip()

        if line == '':
            return line

        return ' ' * TAB_WIDTH + line

    return '\n'.join(indent_line(line) for line in string.splitlines())

# String to s-expressions

class Symbol(object):
    def __init__(self, string):
        self.string = string

    def __eq__(self, other):
        return self.string == other.string

TOKEN = re.compile(r'\s*({})'.format('|'.join('(?P<{}>{})'.format(*token) for token in [
    ('open_parenthese',         r'\('),
    ('close_parenthese',        r'\)'),
    ('identifier',              r'[a-z\-]+'), # We can expand this as needed
    ('integer_literal',         r'\d+'),
    ('string_literal',          r'"(\\"|[^"])*"'),
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

        elif token.group('identifier'):
            items.append(Symbol(token.group('identifier')))

        elif token.group('integer_literal'):
            items.append(int(token.group('integer_literal')))

        elif token.group('string_literal'):
            items.append(undelimit_string(token.group('string_literal')))

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
    def __init__(self, _type, name):
        assert isinstance(_type, CType)
        self._type = _type
        self.name = name

class CExpression(object):
    pass

class CIntegerLiteralExpression(CExpression):
    def __init__(self, integer):
        assert is_integer(integer)
        self.integer = integer

    def __eq__(self, other):
        assert isinstance(other, CIntegerLiteralExpression)
        return self.integer == other.integer

class CStringLiteralExpression(CExpression):
    def __init__(self, string):
        assert isinstance(string, str)
        self.string = string

    def __eq__(self, other):
        assert isinstance(other, CStringLiteralExpression)
        return self.string == other.string

class CVariableExpression(CExpression):
    def __init__(self, name):
        assert isinstance(name, str)
        self.name = name

    def __eq__(self, other):
        assert isinstance(other, CVariableExpression)
        return self.name == other.name

class CReferenceExpression(CExpression):
    def __init__(self, referee):
        assert isinstance(referee, CVariableExpression)
        self.referee = referee

class CFunctionCallExpression(CExpression):
    def __init__(self, name, arguments):
        assert all(isinstance(argument, CExpression) for argument in arguments)
        self.name = name
        self.arguments = arguments

    def __eq__(self, other):
        assert isinstance(other, CFunctionCallExpression)
        return self.name == other.name and self.arguments == other.arguments

class CStatement(object):
    pass

class CExpressionStatement(CStatement):
    def __init__(self, expression):
        self.expression = expression

class CReturnStatement(CStatement):
    def __init__(self, expression):
        self.expression = expression

class CDefinitionStatement(CStatement):
    def __init__(self, _type, name, definition):
        assert isinstance(_type, CType)
        assert isinstance(name, str)
        assert isinstance(definition, CExpression)

        self._type = _type
        self.name = name
        self.definition = definition

class CFunctionBody(object):
    def __init__(self, statements):
        statements = list(statements)
        assert all(isinstance(s, CStatement) for s in statements)
        self.statements = statements

class CFunctionDeclaration(object):
    def __init__(self, return_type, name, argument_declaration_list, body):
        assert isinstance(return_type, CType)
        assert isinstance(argument_declaration_list, list)
        assert all(isinstance(ad, CArgumentDeclaration) for ad in argument_declaration_list)
        assert isinstance(body, CFunctionBody)

        self.return_type = return_type
        self.name = name
        self.argument_declaration_list = argument_declaration_list
        self.body = body

# BEGIN S-expression to C AST layer

def quote_to_c(s_expression):
    if is_integer(s_expression):
        return CFunctionCallExpression(
            'makeObjectPointerFromInteger',
            [CIntegerLiteralExpression(s_expression)],
        )

    if isinstance(s_expression, str):
        return CFunctionCallExpression(
            'makeObjectPointerFromString',
            [CStringLiteralExpression(s_expression)],
        )

    if isinstance(s_expression, Symbol):
        return CFunctionCallExpression(
            'getSymbol',
            [CStringLiteralExpression(s_expression.string)],
        )

    raise Exception('Not implemented for type {}'.format(type(s_expression)))

def evaluate_application_arguments_to_c(
        arguments,
        quote_to_c = quote_to_c,
    ):
    
    if len(arguments) == 0:
        return CVariableExpression('NULL')

    return CFunctionCallExpression(
        'c_cons',
        (
            quote_to_c(arguments[0]),
            evaluate_application_arguments_to_c(arguments[1:]),
        ),
    )

def evaluate_application_to_c(
        s_expression,
        evaluate_application_arguments_to_c = evaluate_application_arguments_to_c,
    ):

    assert isinstance(s_expression, tuple)
    if isinstance(s_expression[0], Symbol):
        return CFunctionCallExpression(
            s_expression[0].string,
            (
                CReferenceExpression(CVariableExpression('env')),
                evaluate_application_arguments_to_c(s_expression[1:]),
            ),
        )

    raise Exception('Not implemented')

def evaluate_to_c(
        s_expression,
        evaluate_application_to_c = evaluate_application_to_c,
    ):

    if isinstance(s_expression, tuple):
        return evaluate_application_to_c(s_expression)

    if is_integer(s_expression):
        return CIntegerLiteralExpression(s_expression)

    if isinstance(s_expression, str):
        return CStringLiteralExpression(s_expression)

    raise Exception('Unable to evaluate expression {} to C'.format(s_expression))

def evaluate_all_to_c(s_expressions):
    c_expressions = list(map(evaluate_to_c, s_expressions))

    return CFunctionBody(itertools.chain(
        [CDefinitionStatement(
            CPointerType(CType('Environment')),
            'env',
            CVariableExpression('NULL'),
        )],
        map(CExpressionStatement, c_expressions[:-1]),
        [CReturnStatement(c_expressions[-1])],
    ))
    
# BEGIN C AST to C source layer

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
    return '{} {}'.format(
        generate_type(argument_declaration._type),
        argument_declaration.name,
    )

def generate_argument_declaration_list(argument_declarations):
    return ', '.join(generate_argument_declaration(ad) for ad in argument_declarations)

def generate_integer_literal_expression(expression):
    assert isinstance(expression, CIntegerLiteralExpression)
    return str(expression.integer)

C_ESCAPE_SEQUENCES = {
    # Taken from https://en.wikipedia.org/wiki/Escape_sequences_in_C
    '\x07'  : r'\a',
    '\x08'  : r'\b',
    '\x0c'  : r'\f',
    '\x0a'  : r'\n',
    '\x0d'  : r'\r',
    '\x09'  : r'\t',
    '\x0b'  : r'\v',
    '\x5c'  : r'\\',
    '\x27'  : r"\'",
    '\x22'  : r'\"',
    '\x3f'  : r'\?',
}

def generate_string_literal_expression(expression):
    assert isinstance(expression, CStringLiteralExpression)

    result = '"'

    for ch in expression.string:
        result += C_ESCAPE_SEQUENCES.get(ch, ch)

    result += '"'

    return result

def generate_variable_expression(expression):
    assert isinstance(expression, CVariableExpression)
    return expression.name

def generate_reference_expression(expression):
    assert isinstance(expression, CReferenceExpression)
    return '&{}'.format(generate_variable_expression(expression.referee))

def generate_function_call_expression(expression):
    assert isinstance(expression, CFunctionCallExpression)
    return '{}({})'.format(
        expression.name,
        ', '.join(generate_expression(e) for e in expression.arguments),
    )

def generate_expression(
        expression,
        generate_integer_literal_expression = generate_integer_literal_expression,
        generate_string_literal_expression = generate_string_literal_expression,
        generate_variable_expression = generate_variable_expression,
        generate_reference_expression = generate_reference_expression,
        generate_function_call_expression = generate_function_call_expression,
        ):

    if isinstance(expression, CIntegerLiteralExpression):
        return generate_integer_literal_expression(expression)

    if isinstance(expression, CStringLiteralExpression):
        return generate_string_literal_expression(expression)

    if isinstance(expression, CVariableExpression):
        return generate_variable_expression(expression)

    if isinstance(expression, CReferenceExpression):
        return generate_reference_expression(expression)

    if isinstance(expression, CFunctionCallExpression):
        return generate_function_call_expression(expression)

    raise Exception('Expression type {} not implemented'.format(type(expression)))

def generate_expression_statement(statement):
    return '{};'.format(generate_expression(statement.expression))

def generate_return_statement(statement):
    return 'return {};'.format(generate_expression(statement.expression))

def generate_definition_statement(statement):
    return '{} {} = {};'.format(
        generate_type(statement._type),
        statement.name,
        generate_expression(statement.definition),
    )

def generate_statement(
        statement,
        generate_expression_statement = generate_expression_statement,
        generate_return_statement = generate_return_statement,
        generate_definition_statement = generate_definition_statement):

    if isinstance(statement, CExpressionStatement):
        return generate_expression_statement(statement)

    if isinstance(statement, CReturnStatement):
        return generate_return_statement(statement)

    if isinstance(statement, CDefinitionStatement):
        return generate_definition_statement(statement)

    raise Exception('Handling for statements of type {} not implemented'.format(type(statement)))

def generate_function_body(function_body):
    assert isinstance(function_body, CFunctionBody)
    return '\n'.join(generate_statement(s) for s in function_body.statements)

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
        body = indent(generate_function_body(function_declaration.body)),
    )

PROGRAM_TEMPLATE = string.Template(
'''
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Object;
typedef struct Object Object;

enum Type
{
    CELL,
    STRING,
    SYMBOL
};
typedef enum Type Type;

#define MAX_TYPE_STRING_LENGTH 7

void typeToString(Type type, char* target)
{
    switch(type)
    {
        case CELL:
            snprintf(target, MAX_TYPE_STRING_LENGTH, "CELL");
            return;

        case STRING:
            snprintf(target, MAX_TYPE_STRING_LENGTH, "STRING");
            return;

        case SYMBOL:
            snprintf(target, MAX_TYPE_STRING_LENGTH, "%s", "SYMBOL");
            return;

        default:
            fprintf(stderr, "ERROR: Unknown type");
            exit(1);
    }
}

struct Cell;
typedef struct Cell Cell;
struct Cell
{
    Object* left;
    Object* right;
};

struct Environment;
typedef struct Environment Environment;
struct Environment
{
    char* key;
    Object* value;
    Environment* next;
};

Environment makeEnvironment(char* key, Object* value, Environment* next)
{
    Environment result;
    result.key = key;
    result.value = value;
    result.next = next;
    return result;
}

Environment* makeEnvironmentPointerFromEnvironment(Environment env)
{
    Environment* result = malloc(sizeof(Environment));
    *result = env;
    return result;
}

Environment* makeEnvironmentPointer(char* key, Object* value, Environment* next)
{
    return makeEnvironmentPointerFromEnvironment(makeEnvironment(key, value, next));
}

union Instance
{
    Cell cell;
    char* string;
    char* symbol;
};
typedef union Instance Instance;

Instance makeInstanceFromCell(Cell cell)
{
    Instance result;
    result.cell = cell;
    return result;
}

Instance makeInstanceFromString(char* string)
{
    Instance result;
    result.string = string;
    return result;
}

Instance makeInstanceFromSymbol(char* symbol)
{
    Instance result;
    result.symbol = symbol;
    return result;
}

struct Object
{
    Type type;
    Instance instance;
};

Object makeObject(Type t, Instance i)
{
    Object result;
    result.type = t;
    result.instance = i;
    return result;
}

Object makeObjectFromCell(Cell cell)
{
    return makeObject(CELL, makeInstanceFromCell(cell));
}

Object makeObjectFromString(char* string)
{
    return makeObject(STRING, makeInstanceFromString(string));
}

Object makeObjectFromSymbol(char* symbol)
{
    return makeObject(SYMBOL, makeInstanceFromSymbol(symbol));
}

Object* makeObjectPointerFromObject(Object o)
{
    Object* result = malloc(sizeof(Object));
    *result = o;
    return result;
}

Object* makeObjectPointerFromCell(Cell cell)
{
    return makeObjectPointerFromObject(makeObjectFromCell(cell));
}

Object* makeObjectPointerFromString(char* string)
{
    return makeObjectPointerFromObject(makeObjectFromString(string));
}

Object* makeObjectPointerFromSymbol(char* symbol)
{
    return makeObjectPointerFromObject(makeObjectFromSymbol(symbol));
}

Object* getSymbol(char* symbol)
{
    // This will not always be how this is implemented
    return makeObjectPointerFromSymbol(symbol);
}

Cell makeCell(Object* left, Object* right)
{
    Cell result;
    result.left = left;
    result.right = right;
    return result;
}

Object* c_cons(Object* left, Object* right)
{
    Cell cell = makeCell(left, right);
    return makeObjectPointerFromCell(cell);
}

void c_print(Object* stutter_string)
{
    if(stutter_string->type != STRING)
    {
        char typeName[MAX_TYPE_STRING_LENGTH];
        typeToString(stutter_string->type, typeName);
        fprintf(stderr, "ERROR: Expected type STRING, got type %s.", typeName);
        exit(1);
    }

    char* c_string = stutter_string->instance.string;
    printf("%s", c_string);
}

bool c_symbol_equal(char* left, char* right)
{
    return strcmp(left, right) == 0;
}

Object* c_evaluate_symbol(Environment* env, Object* s)
{
    if(env == NULL)
    {
        fprintf(stderr, "ERROR: symbol %s not found.", s->instance.symbol);
        exit(1);
    }

    if(c_symbol_equal(env->key, s->instance.symbol))
    {
        return env->value;
    }

    return c_evaluate_symbol(env->next, s);
}

Object* c_evaluate(Environment** env, Object* o)
{
    switch(o->type)
    {
        case STRING:
            return o;

        case SYMBOL:
            return c_evaluate_symbol(*env, o);

        default:
            break;
    }

    char typeName[MAX_TYPE_STRING_LENGTH];
    typeToString(o->type, typeName);
    fprintf(stderr, "ERROR: Could not evaluate type %s.", typeName);
    exit(1);
}

int countArgs(Object* args)
{
    if(args == NULL) return 0;

    assert(args->type == CELL);
    return 1 + countArgs(args->instance.cell.right);
}

Object* getArg(int index, Object* args)
{
    if(index == 0) return args->instance.cell.left;

    return getArg(index - 1, args->instance.cell.right);
}

void print(Environment** parent, Object* args)
{
    assert(countArgs(args) == 1);
    Object* stutter_string = c_evaluate(parent, getArg(0, args));
    c_print(stutter_string);
}

void define(Environment** parent, Object* args)
{
    assert(countArgs(args) == 2);
    Object* name = getArg(0, args);
    Object* value = c_evaluate(parent, getArg(1, args));

    assert(name->type == SYMBOL);
    *parent = makeEnvironmentPointer(name->instance.symbol, value, *parent);
}

int main(int argc, char** argv)
{
$body
}
'''.strip())

def generate_program(body):
    return PROGRAM_TEMPLATE.substitute(
        body = body,
    )

if __name__ == '__main__':
    import sys
    source_file_name = sys.argv[1]

    with open(source_file_name, 'r') as source_file:
        source = source_file.read()

    result = generate_program(
        indent(generate_function_body(evaluate_all_to_c(parse_all(source)))),
    )

    print(result)
