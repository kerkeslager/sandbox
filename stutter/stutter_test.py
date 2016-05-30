#!/usr/bin/env python

import stutter
import unittest

class IsIntegerTests(unittest.TestCase):
    def test_returns_true_for_integers(self):
        for i in range(10):
            self.assertTrue(stutter.is_integer(i))

    def test_returns_false_for_booleans(self):
        self.assertFalse(stutter.is_integer(True))
        self.assertFalse(stutter.is_integer(False))

    def test_returns_false_for_other_types(self):
        for o in [object(), '', 0.1, [], (), {}, set()]:
            self.assertFalse(stutter.is_integer(o))

class UndelimitStringTests(unittest.TestCase):
    def test_returns_empty_strings(self):
        expected = ''
        actual = stutter.undelimit_string('""')
        
        self.assertEqual(expected, actual)

    def test_returns_strings_without_escapes(self):
        expected = 'Hello, world'
        actual = stutter.undelimit_string('"Hello, world"')
        
        self.assertEqual(expected, actual)

    def test_returns_strings_with_newlines(self):
        expected = 'Hello, world\nGoodbye, cruel world'
        actual = stutter.undelimit_string('"Hello, world\\nGoodbye, cruel world"')
        
        self.assertEqual(expected, actual)

    def test_returns_strings_with_escaped_delimiters(self):
        expected = '"Hello, world"'
        actual = stutter.undelimit_string('"\\"Hello, world\\""')
        
        self.assertEqual(expected, actual)

    def test_returns_strings_with_escaped_escape_characters(self):
        expected = '\\no'
        actual = stutter.undelimit_string('"\\\\no"')
        
        self.assertEqual(expected, actual)

class IndentTests(unittest.TestCase):
    def test_indents_single_line(self):
        expected = '    Hello, world'
        actual = stutter.indent('Hello, world')
        self.assertEqual(expected, actual)

    def test_indents_multiple_lines(self):
        expected = '    Hello, world\n    Goodbye, cruel world'
        actual = stutter.indent('Hello, world\nGoodbye, cruel world')
        self.assertEqual(expected, actual)

    def test_leaves_empty_lines_empty(self):
        expected = '    Hello, world\n\n    Goodbye, cruel world'
        actual = stutter.indent('Hello, world\n \nGoodbye, cruel world')
        self.assertEqual(expected, actual)

    def test_indents_already_indented_lines(self):
        expected = '        Hello, world\n\n        Goodbye, cruel world'
        actual = stutter.indent('    Hello, world\n\n    Goodbye, cruel world')
        self.assertEqual(expected, actual)

class ParseAllTests(unittest.TestCase):
    def test_parses_integers(self):
        expected = [0]
        actual = stutter.parse_all('0')

        self.assertEqual(expected, actual)

    def test_parses_identifiers(self):
        expected = [stutter.Symbol('print')]
        actual = stutter.parse_all('print')

        self.assertEqual(expected, actual)

    def test_parses_identifiers_with_dashes(self):
        expected = [stutter.Symbol('hello-world')]
        actual = stutter.parse_all('hello-world')

        self.assertEqual(expected, actual)

    def test_parses_strings(self):
        expected = ['Hello, world']
        actual = stutter.parse_all('"Hello, world"')

        self.assertEqual(expected, actual)

    def test_parses_strings_with_escaped_delimiters(self):
        expected = ['"Hello, world"']
        actual = stutter.parse_all('"\\"Hello, world\\""')

        self.assertEqual(expected, actual)

    def test_parses_empty_s_expressions(self):
        expected = [()]
        actual = stutter.parse_all('()')

        self.assertEqual(expected, actual)

    def test_parses_s_expressions(self):
        expected = [(0, 1, 2)]
        actual = stutter.parse_all('(0 1 2)')

        self.assertEqual(expected, actual)

    def test_parses_nested_s_expressions(self):
        expected = [(0, (1, (2,)))]
        actual = stutter.parse_all('(0 (1 (2)))')

        self.assertEqual(expected, actual)

    def test_parses_multiple_expressions(self):
        expected = [0, ()]
        actual = stutter.parse_all('0 ()')

        self.assertEqual(expected, actual)

    def test_raises_exception_for_unclosed_parenthese(self):
        self.assertRaises(Exception, stutter.parse_all, '(')

    def test_raises_exception_for_unopened_parenthese(self):
        self.assertRaises(Exception, stutter.parse_all, ')')

class QuoteToCTests(unittest.TestCase):
    def test_quotes_integer_literals(self):
        for i in range(5):
            expected = stutter.CFunctionCallExpression(
                'makeObjectPointerFromInteger',
                [stutter.CIntegerLiteralExpression(i)],
            )
            
            actual = stutter.quote_to_c(i)
            
            self.assertEqual(expected, actual)

    def test_quotes_string_literals(self):
        s = 'Hello, world'
        expected = stutter.CFunctionCallExpression(
            'makeObjectPointerFromString',
            [stutter.CStringLiteralExpression(s)],
        )

        actual = stutter.quote_to_c(s)

        self.assertEqual(expected, actual)

    def test_quotes_symbols(self):
        s = 'symbol'
        expected = stutter.CFunctionCallExpression(
            'getSymbol',
            [stutter.CStringLiteralExpression(s)],
        )

        actual = stutter.quote_to_c(stutter.Symbol(s))

        self.assertEqual(expected, actual)

class EvaluateApplicationArgumentsToCTests(unittest.TestCase):
    def test_evaluates_empty_to_null(self):
        expected = stutter.CVariableExpression('NULL')
        actual = stutter.evaluate_application_arguments_to_c(())

        self.assertEqual(expected, actual)

    def test_evaluates_one_argument_to_cons(self):
        argument = 42

        sentinel = stutter.CStringLiteralExpression('1bd9707e76f8f807f3bad3e39049fea4a36d8ef2f8e2ed471ec755f7adb291d5')

        def mock(argument_to_quote):
            if argument_to_quote == argument:
                return sentinel

        expected = stutter.CFunctionCallExpression(
            'c_cons',
            (sentinel, stutter.CVariableExpression('NULL')),
        )

        actual = stutter.evaluate_application_arguments_to_c(
            (argument,),
            quote_to_c = mock,
        )

        self.assertEqual(expected, actual)

class EvaluateApplicationToCTests(unittest.TestCase):
    def test_evaluates_function_calls_with_no_arguments(self):
        name = 'name'

        sentinel = stutter.CVariableExpression('NULL')

        def mock(arguments):
            assert arguments == ()
            return sentinel

        result = stutter.evaluate_application_to_c(
            (stutter.Symbol(name),),
            evaluate_application_arguments_to_c = mock,
        )

        self.assertEqual(result.name, name)
        self.assertEqual(len(result.arguments), 2)
        self.assertTrue(isinstance(
            result.arguments[0],
            stutter.CReferenceExpression,
        ))
        self.assertIs(result.arguments[1], sentinel)

    def test_evaluates_function_calls_with_arguments(self):
        name = 'print'
        argument = 42 

        sentinel = stutter.CFunctionCallExpression(
            'cons',
            [
                stutter.CFunctionCallExpression(
                    'makeObjectPointerFromInteger',
                    [stutter.CIntegerLiteralExpression(argument)],
                ),
            ],
        )

        def mock(arguments):
            assert arguments == (argument,)
            return sentinel

        result = stutter.evaluate_application_to_c(
            (stutter.Symbol(name), argument),
            evaluate_application_arguments_to_c = mock,
        )

        self.assertEqual(result.name, name)
        self.assertEqual(len(result.arguments), 2)
        self.assertTrue(isinstance(
            result.arguments[0],
            stutter.CReferenceExpression,
        ))
        self.assertIs(result.arguments[1], sentinel)

class EvaluateToCTests(unittest.TestCase):
    def test_evaluates_integers(self):
        for i in range(5):
            result = stutter.evaluate_to_c(i)
            self.assertIsInstance(result, stutter.CIntegerLiteralExpression)
            self.assertEqual(result.integer, i)

    def test_evaluates_string_literals(self):
        s = 'Hello, world'
        result = stutter.evaluate_to_c(s)

        self.assertIsInstance(result, stutter.CStringLiteralExpression)
        self.assertEqual(result.string, s)

    def test_calls_evaluate_application_when_given_an_application(self):
        sentinel = object()
        application = (stutter.Symbol('print'), 'Hello, world')

        def mock(argument):
            if argument == application:
                return sentinel

        result = stutter.evaluate_to_c(
            application,
            evaluate_application_to_c = mock,
        )

        self.assertIs(result, sentinel)

class EvaluateAllToCTests(unittest.TestCase):
    def test_returns_function_body(self):
        result = stutter.evaluate_all_to_c([0])

        self.assertIsInstance(result, stutter.CFunctionBody)

    def test_main_contains_expression_statements_followed_by_return_statement(self):
        result = stutter.evaluate_all_to_c([0,0,0])

        self.assertIsInstance(result.statements[0],stutter.CDefinitionStatement)
        self.assertIsInstance(result.statements[1],stutter.CExpressionStatement)
        self.assertIsInstance(result.statements[2],stutter.CExpressionStatement)
        self.assertIsInstance(result.statements[3],stutter.CReturnStatement)

class GeneratePointerTypeTests(unittest.TestCase):
    def test_basic(self):
        expected = 'int*'
        actual = stutter.generate_pointer_type(stutter.CPointerType(stutter.CType('int')))
        self.assertEqual(expected, actual)

class GenerateTypeTests(unittest.TestCase):
    def test_basic(self):
        expected = 'int'
        actual = stutter.generate_type(stutter.CType('int'))
        self.assertEqual(expected, actual)

    def test_generates_pointer_types(self):
        expected = object()
        actual = stutter.generate_type(
                stutter.CPointerType(stutter.CType('int')),
                generate_pointer_type = lambda x : expected)

        self.assertIs(expected, actual)

class GenerateArgumentDeclarationTests(unittest.TestCase):
    def test_basic(self):
        expected = 'int argc'
        actual = stutter.generate_argument_declaration(
                stutter.CArgumentDeclaration(stutter.CType('int'), 'argc'))
        self.assertEqual(expected, actual)

class GenerateArgumentDeclarationListTests(unittest.TestCase):
    def test_basic(self):
        argument_declarations = [
                stutter.CArgumentDeclaration(stutter.CType('int'),'argc'),
                stutter.CArgumentDeclaration(stutter.CPointerType(stutter.CPointerType(stutter.CType('char'))), 'argv'),
            ]
        expected = 'int argc, char** argv'
        actual = stutter.generate_argument_declaration_list(argument_declarations)
        self.assertEqual(expected, actual)

class GenerateIntegerLiteralExpressionTests(unittest.TestCase):
    def test_basic(self):
        expected = '0'
        actual = stutter.generate_integer_literal_expression(
            stutter.CIntegerLiteralExpression(0),
        )
        self.assertEqual(expected, actual)

class GenerateStringLiteralExpressionTests(unittest.TestCase):
    def test_basic(self):
        expected = '"Hello, world"'
        actual = stutter.generate_string_literal_expression(
            stutter.CStringLiteralExpression('Hello, world'),
        )
        self.assertEqual(expected, actual)

    def test_escapes(self):
        expected = r'"\\\n\"\t"'
        actual = stutter.generate_string_literal_expression(
            stutter.CStringLiteralExpression('\\\n"\t'),
        )
        self.assertEqual(expected, actual)

class GenerateVariableExpressionTests(unittest.TestCase):
    def test_generates(self):
        expected = 'name'
        actual = stutter.generate_variable_expression(
            stutter.CVariableExpression(expected),
        )

        self.assertEqual(expected, actual)

class GenerateReferenceExpressionTests(unittest.TestCase):
    def test_generates(self):
        expected = '&name';
        actual = stutter.generate_reference_expression(
            stutter.CReferenceExpression(stutter.CVariableExpression('name')),
        )

        self.assertEqual(expected, actual)

class GenerateFunctionCallExpressionTests(unittest.TestCase):
    def test_no_arguments(self):
        expected = 'name()'
        actual = stutter.generate_function_call_expression(
            stutter.CFunctionCallExpression('name', []),
        )
        self.assertEqual(expected, actual)

    def test_one_argument(self):
        expected = 'name(0)'
        actual = stutter.generate_function_call_expression(
            stutter.CFunctionCallExpression(
                'name',
                [
                    stutter.CIntegerLiteralExpression(0),
                ],
            ),
        )
        self.assertEqual(expected, actual)

    def test_many_arguments(self):
        expected = 'name(0, 1)'
        actual = stutter.generate_function_call_expression(
            stutter.CFunctionCallExpression(
                'name',
                [
                    stutter.CIntegerLiteralExpression(0),
                    stutter.CIntegerLiteralExpression(1),
                ],
            ),
        )
        self.assertEqual(expected, actual)

class GenerateExpressionTests(unittest.TestCase):
    def test_generates_integer_literal_expressions(self):
        expected = object()
        actual = stutter.generate_expression(
                stutter.CIntegerLiteralExpression(0),
                generate_integer_literal_expression = lambda x : expected)

        self.assertIs(expected, actual)

    def test_generates_string_literal_expressions(self):
        expected = object()
        actual = stutter.generate_expression(
                stutter.CStringLiteralExpression('Hello, world'),
                generate_string_literal_expression = lambda x : expected)

        self.assertIs(expected, actual)

    def test_generates_variable_expression(self):
        expected = object()
        actual = stutter.generate_expression(
                stutter.CVariableExpression('name'),
                generate_variable_expression = lambda x : expected)

        self.assertIs(expected, actual)

    def test_generates_variable_expression(self):
        expected = object()
        actual = stutter.generate_expression(
                stutter.CReferenceExpression(stutter.CVariableExpression('name')),
                generate_reference_expression = lambda x : expected)

        self.assertIs(expected, actual)

    def test_generates_function_call_expression(self):
        expected = object()
        actual = stutter.generate_expression(
                stutter.CFunctionCallExpression('name',[]),
                generate_function_call_expression = lambda x : expected)

        self.assertIs(expected, actual)

class GenerateStatement(unittest.TestCase):
    def test_generates_expression_statement(self):
        return_statement = stutter.CExpressionStatement('0')

        expected = object()
        actual = stutter.generate_statement(
            return_statement,
            generate_expression_statement = lambda _ : expected)

        self.assertIs(expected, actual)

    def test_generates_return_statement(self):
        return_statement = stutter.CReturnStatement(stutter.CIntegerLiteralExpression(0))

        expected = object()
        actual = stutter.generate_statement(
            return_statement,
            generate_return_statement = lambda _ : expected)

        self.assertIs(expected, actual)

    def test_generates_definition_statement(self):
        definition_statement = stutter.CDefinitionStatement(
            stutter.CType('int'),
            'number',
            stutter.CIntegerLiteralExpression(0),
        )

        expected = object()
        actual = stutter.generate_statement(
            definition_statement,
            generate_definition_statement = lambda _ : expected)

        self.assertIs(expected, actual)

class GenerateExpressionStatementTests(unittest.TestCase):
    def test_generates_return_statement(self):
        expression_statement = stutter.CExpressionStatement(stutter.CIntegerLiteralExpression(0))

        expected = '0;'
        actual = stutter.generate_expression_statement(expression_statement)

        self.assertEqual(expected, actual)

class GenerateReturnStatementTests(unittest.TestCase):
    def test_generates_return_statement(self):
        return_statement = stutter.CReturnStatement(stutter.CIntegerLiteralExpression(0))

        expected = 'return 0;'
        actual = stutter.generate_return_statement(return_statement)

        self.assertEqual(expected, actual)

class GenerateDefinitionStatementTests(unittest.TestCase):
    def test_generates_definition_statement(self):
        definition_statement = stutter.CDefinitionStatement(
            stutter.CType('int'),
            'number',
            stutter.CIntegerLiteralExpression(0),
        )

        expected = 'int number = 0;'
        actual = stutter.generate_definition_statement(definition_statement)

        self.assertEqual(expected, actual)

class GenerateFunctionDeclarationTests(unittest.TestCase):
    def test_basic(self):
        return_type = stutter.CType('int')
        argument_declarations = [
                stutter.CArgumentDeclaration(stutter.CType('int'),'argc'),
                stutter.CArgumentDeclaration(stutter.CPointerType(stutter.CPointerType(stutter.CType('char'))), 'argv'),
            ]

        function_declaration = stutter.CFunctionDeclaration(
            return_type,
            'main',
            argument_declarations,
            stutter.CFunctionBody(
                [stutter.CReturnStatement(stutter.CIntegerLiteralExpression(0))],
            ),
        )

        expected = 'int main(int argc, char** argv)\n{\n    return 0;\n}'
        actual = stutter.generate_function_declaration(function_declaration)
        self.assertEqual(expected, actual)

unittest.main()
