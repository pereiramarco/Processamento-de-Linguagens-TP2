import ply.lex as lex

tokens = ['ID','INT','FLOAT','STRING','READ','WRITE','EQUAL', 'NOTEQUAL','BIGGER','BIGGEREQUAL',
'SMALLER','SMALLEREQUAL','RET','IF','DO','ELSE','WHILE','FUNC','CALL','REPEAT','UNTIL','FOR']

literals = [';','=','+','-','*','/','%','{','}','&','|',':','(',')','[',']']

t_IF = r'<if>'

t_DO = r'<do>'

t_ELSE = r'<else>'

t_FUNC = r'<func>'

t_WHILE = r'<while>'

t_REPEAT = r'<repeat>'

t_UNTIL = r'<until>'

t_FOR = r'<for>'

t_CALL = r'<call>'

t_RET = r'<ret>'

t_READ = r'<read>'

t_WRITE = r'<write>'

t_EQUAL = r'=='

t_NOTEQUAL = r'!='

t_BIGGER = r'>'

t_BIGGEREQUAL = r'>='

t_SMALLER = r'<'

t_SMALLEREQUAL = r'<='

t_FLOAT = r'\d+\.\d+'

t_INT = r'\d+'

t_STRING = r'\"([^"]|\\\")*\"'

t_ID = r'[a-zA-Z]([a-zA-Z]|_|\d)*'

t_ignore = " \t\n"

def t_error(t):
    print("Caráter ilegal: ", t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()