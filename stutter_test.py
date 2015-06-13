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

class ParseAllTests(unittest.TestCase):
    def test_parses_integers(self):
        expected = [0]
        actual = stutter.parse_all('0')

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

class EvaluateToCTests(unittest.TestCase):
    def test_evaluates_integers(self):
        for i in range(5):
            result = stutter.evaluate_to_c(i)
            self.assertIsInstance(result, stutter.CIntegerLiteralExpression)
            self.assertEqual(result.integer, i)

class EvaluateAllToCTests(unittest.TestCase):
    def test_returns_main(self):
        result = stutter.evaluate_all_to_c([0])

        self.assertIsInstance(result, stutter.CFunctionDeclaration)
        self.assertEqual(result.name, 'main')

    def test_main_contains_expression_statements_followed_by_return_statement(self):
        result = stutter.evaluate_all_to_c([0,0,0])

        self.assertIsInstance(result.body[0],stutter.CExpressionStatement)
        self.assertIsInstance(result.body[1],stutter.CExpressionStatement)
        self.assertIsInstance(result.body[2],stutter.CReturnStatement)

class IndentTests(unittest.TestCase):
    def test_indents_single_line(self):
        expected = '  Hello, world'
        actual = stutter.indent('Hello, world')
        self.assertEqual(expected, actual)

    def test_indents_multiple_lines(self):
        expected = '  Hello, world\n  Goodbye, cruel world'
        actual = stutter.indent('Hello, world\nGoodbye, cruel world')
        self.assertEqual(expected, actual)

    def test_leaves_empty_lines_empty(self):
        expected = '  Hello, world\n\n  Goodbye, cruel world'
        actual = stutter.indent('Hello, world\n \nGoodbye, cruel world')
        self.assertEqual(expected, actual)

    def test_indents_already_indented_lines(self):
        expected = '    Hello, world\n\n    Goodbye, cruel world'
        actual = stutter.indent('  Hello, world\n \n  Goodbye, cruel world')
        self.assertEqual(expected, actual)

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
                [stutter.CReturnStatement(stutter.CIntegerLiteralExpression(0))])

        expected = 'int main(int argc, char** argv)\n{\n  return 0;\n}'
        actual = stutter.generate_function_declaration(function_declaration)
        self.assertEqual(expected, actual)

unittest.main()
