import ply.lex as lex

import sys

tokens = ['ID','INT','FLOAT','IN','OUT','EQUAL','BIGGER','BIGGEREQUAL','SMALLER','SMALLEREQUAL','RET','IF','THEN','ELSE','WHILE','FUNC','CALL']

literals = [';','=','+','-','*','/','%','{','}','&','|',':']

t_IF = r'<if>'

t_THEN = r'<then>'

t_ELSE = r'<else>'

t_FUNC = r'<func>'

t_WHILE = r'<while>'

t_CALL = r'<call>'

t_RET = r'<ret>'

t_IN = r'<<'

t_OUT = r'>>'

t_EQUAL = r'=='

t_BIGGER = r'>'

t_BIGGEREQUAL = r'>='

t_SMALLER = r'<'

t_SMALLEREQUAL = r'<='

t_FLOAT = r'\d+\.\d+'

t_INT = r'\d+'

#t_STRING = r'\"[^\"]*\"'

t_ID = r'[a-zA-Z]([a-zA-Z]|\d)*'

t_ignore = " \t\n"

def t_error(t):
    print("Caráter ilegal: ", t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()