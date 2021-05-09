'''
T:{ID,INT,FLOAT,IN,OUT,EQUAL,BIGGER,BIGGEREQUAL,SMALLER,SMALLEREQUAL,RET,IF,THEN,ELSE,
    WHILE,FUNC,CALL,';','=','+','-','*','/','%','{','}','&','|',':'}

N:{Inicio,Variaveis,Codigo,Declaracoes,Declaracao,Type,Expr,Term,Factor,Funcao,DecFunc,
    ListInstrucao,Instrucao,Atribuicao,Leitura,Escrita,Condicao,Ciclo,Var,ExprCond,Then,
    Else,Cond,While,Ret}

S: Inicio

P:{
    p0: Inicio => Variaveis Codigo
    p1: Variaveis : '{' Declaracoes '}'
    p2: Declaracoes : Declaracoes Declaracao
    p3: Declaracao : Type ID '=' Expr ';'
    p4:             | Type ID ';'
    p5: Type : ID
    p6: Expr : Expr '+' Term
    p7:      | Expr '-' Term
    p8:      | Term
    p9: Term : Term '*' Factor
    p10:     | Term '/' Factor
    p11:     | Term '%' Factor
    p12:     | Factor
    p13: Factor : INT
    p14:        | '-' INT
    p15:        | FLOAT
    p16:        | '-' FLOAT
    p17:        | ID
    p18:        | '-' ID
    p19:        | CALL ID
    p20: Codigo : Codigo Funcao
    p21:        | Codigo Instrucao
    p22:        | 
    p23: Funcao : DecFunc '{' ListInstrucao Ret '}'
    p24: DecFunc : FUNC ID
    p25: ListInstrucao : ListInstrucao Instrucao
    p26: ListInstrucao : 
    p27: Instrucao : Atribuicao
    p28: Instrucao : Leitura
    p29: Instrucao : Escrita
    p30: Instrucao : Condicao
    p31: Instrucao : Ciclo
    p32: Atribuicao : Var '=' Expr ';'
    p33: Var : ID
    p34: Leitura : Var IN ';'
    p35: Escrita : OUT Var ';'
    p36: Condicao : IF ExprCond Then ListInstrucao ';'
    p37: ExprCond : ExprCond '&' Cond 
    p38: ExprCond : ExprCond '|' Cond
    p39: ExprCond : Cond
    p40: Cond : Expr EQUAL Expr
    p41: Cond : Expr BIGGER Expr
    p42: Cond : Expr BIGGEREQUAL Expr
    p43: Cond : Expr SMALLER Expr
    p44: Cond : Expr SMALLEREQUAL Expr
    p45: Then : THEN
    p46: Condicao : IF ExprCond Then ListInstrucao Else ListInstrucao ';'
    p47: Else : ELSE
    p48: Ciclo : While ExprCond Then '{' ListInstrucao '}'
    p49: Ret : RET Expr ';'
}
'''

import ply.yacc as yacc

from interpretador_lex import tokens,literals

def p_Inicio(p):
    "Inicio : Variaveis Codigo"

def p_Variaveis(p):
    "Variaveis : '{' Declaracoes '}'"

def p_Declaracoes(p):
    "Declaracoes : Declaracoes Declaracao"

def p_Declaracoes_paragem(p):
    "Declaracoes : "
    pass

def p_Declaracao(p):
    "Declaracao : Type ID '=' Expr ';'"
    linhaDec=p.parser.localizacao.get(p[2].strip())
    if linhaDec!=None:
        p.parser.success=False
        print("Variável",p[2],"foi redeclarada na linha",p.parser.var+1,"das declarações depois de ter sido declarada na linha",linhaDec+1)
    p.parser.localizacao.update({p[2].strip() : p.parser.var})
    p.parser.types.update({p[2] : p.parser.current_type})
    p.parser.compiled +=p[4]
    p.parser.var+=1
    p.parser.current_type="None"

def p_Declaracao_simples(p):
    "Declaracao : Type ID ';'"
    linhaDec=p.parser.localizacao.get(p[2].strip())
    if linhaDec!=None:
        p.parser.success=False
        print("Variável",p[2],"foi redeclarada na linha",p.parser.var+1,"das declarações depois de ter sido declarada na linha",linhaDec+1)
    p.parser.localizacao.update({p[2].strip() : p.parser.var})
    p.parser.types.update({p[2] : p.parser.current_type})
    if p.parser.current_type=="":
        p.parser.compiled+= "pushi 0\n"
    elif p.parser.current_type=="f":
        p.parser.compiled+= "pushf 0.0\n"
    p.parser.var+=1
    p.parser.current_type="None"
    
def p_Type(p):
    "Type : ID"
    if p[1]=="int" :
        p.parser.current_type=""
    elif p[1]=="float" :
        p.parser.current_type="f"
    else :
        print("ERROR: Unknown type \"",p[1],"\"")
        p.parser.success=False

def p_Codigo_decFunc(p):
    "Codigo : Codigo Funcao"

def p_Codigo_Instrucao(p):
    "Codigo : Codigo Instrucao"

def p_Codigo_paragem(p):
    "Codigo : "

def p_Funcao(p):
    "Funcao : DecFunc '{' ListInstrucao Ret '}'"

def p_DecFunc(p):
    "DecFunc : FUNC ID"
    p.parser.compiled+="jump af"+str(p.parser.label_in) +"\n"+ p[2].strip() +" :\n"
    p.parser.label_in+=1
    
def p_Ret(p):
    "Ret : RET Expr ';'"
    p.parser.compiled+=p[2] + "storel -1\nreturn\naf" + str(p.parser.label_out) + " :\n"
    p.parser.label_out-=1

def p_ListInstrucao(p):
    "ListInstrucao : ListInstrucao Instrucao"

def p_ListInstrucao_paragem(p):
    "ListInstrucao : "

def p_Instrucao_atribuicao(p):
    "Instrucao : Atribuicao"

def p_Instrucao_reading(p):
    "Instrucao : Leitura"

def p_Instrucao_writing(p):
    "Instrucao : Escrita"

def p_Instrucao_condicao(p):
    "Instrucao : Condicao"

def p_Instrucao_ciclo(p):
    "Instrucao : Ciclo"

def p_Leitura(p):
    "Leitura : Var IN ';'"
    t = p.parser.types.get(p[1].strip())
    p.parser.compiled+="read\n"
    p.parser.compiled+="dup 1\n"
    if t=="" :
        p.parser.compiled+="atoi\n"
    elif t=="f":
        p.parser.compiled+="atof\n"
    p.parser.compiled+="storeg " + str(p.parser.localizacao.get(p[1].strip())) +"\n"
    p.parser.compiled+="free\n"
    p.parser.current_type="None"

def p_Escrita(p):
    "Escrita : OUT Var ';'"
    t = p.parser.types.get(p[2].strip())
    p.parser.compiled+="pushg " + str(p.parser.localizacao.get(p[2].strip())) +"\n"
    if t=="" :
        p.parser.compiled+="writei\n"
    elif t=="f" :
        p.parser.compiled+="writef\n"
    p.parser.current_type="None"

def p_Atribuicao(p):
    "Atribuicao : Var '=' Expr ';'"
    p.parser.compiled+=p[3]+"storeg " + str(p.parser.localizacao.get(p[1])) +"\n"
    p.parser.current_type="None"

def p_Condicao_if(p):
    "Condicao : IF ExprCond Then ListInstrucao ';'"
    p.parser.compiled+="e"+str(p.parser.label_out-1) + " :\n"
    p.parser.label_out-=1

def p_Condicao_else(p):
    "Condicao : IF ExprCond Then ListInstrucao Else ListInstrucao ';'"
    p.parser.compiled+="t"+str(p.parser.label_out-1)+" :\n"
    p.parser.label_out-=1

def p_Ciclo(p):
    "Ciclo : While ExprCond Then '{' ListInstrucao '}'"
    p.parser.compiled+="jump c" + str(p.parser.label_out-1) + "\n" + "e"+str(p.parser.label_out-1) + " :\n"
    p.parser.label_out-=1

def p_While(p):
    "While : WHILE"
    p.parser.compiled+="c"+str(p.parser.label_in) +" :\n"

def p_Then(p):
    "Then : THEN"
    p.parser.compiled+="jz e" + str(p.parser.label_in) + "\n"
    p.parser.label_in+=1
    p.parser.label_out+=1
    p.parser.current_type="None"

def p_Else(p):
    "Else : ELSE"
    p.parser.compiled+="jump t" + str(p.parser.label_out-1) +"\n"+"e"+str(p.parser.label_out-1) + " :\n"

def p_ExprCond_and(p):
    "ExprCond : ExprCond '&' Cond"
    p.parser.compiled+="mul\n"

def p_ExprCond_or(p):
    "ExprCond : ExprCond '|' Cond"
    p.parser.compiled+="add\n"

def p_ExprCond_paragem(p):
    "ExprCond : Cond"

def p_Cond_equals(p):
    "Cond : Expr EQUAL Expr"
    p.parser.compiled += p[1] + p[3] + "equal\n"

def p_Cond_bigger(p):
    "Cond : Expr BIGGER Expr"
    p.parser.compiled += p[1] + p[3] + p.parser.current_type + "sup\n"

def p_Cond_bigger_equal(p):
    "Cond : Expr BIGGEREQUAL Expr"
    p.parser.compiled += p[1] + p[3] + p.parser.current_type + "supeq\n"

def p_Cond_smaller(p):
    "Cond : Expr SMALLER Expr"
    p.parser.compiled += p[1] + p[3] + p.parser.current_type + "inf\n"

def p_Cond_smaller_equal(p):
    "Cond : Expr SMALLEREQUAL Expr"
    p.parser.compiled += p[1] + p[3] + p.parser.current_type + "infeq\n"

def p_Var(p):
    "Var : ID"
    t = p.parser.types.get(p[1].strip())
    p.parser.current_type=t
    if t==None:
        print("Variável ",p[1]," não declarada")
        p.parser.success=False
    p[0]=p[1]

def p_Expr_add(p):
    "Expr : Expr '+' Term"
    p[0] = p[1] + p[3] + p.parser.current_type + "add\n"

def p_Expr_sub(p):
    "Expr : Expr '-' Term"
    p[0] = p[1] + p[3] + p.parser.current_type + "sub\n"

def p_Expr_paragem(p):
    "Expr : Term"
    p[0] = p[1]

def p_Term_mul(p):
    "Term : Term '*' Factor"
    p[0] = p[1] + p[3] + p.parser.current_type + "mul\n"

def p_Term_div(p):
    "Term : Term '/' Factor"
    p[0] = p[1] + p[3]+ p.parser.current_type + "div\n"

def p_Term_mod(p):
    "Term : Term '%' Factor"
    if p.parser.current_type=="f":
        print("ERRO: A operação % não pode ser utilizada para floats")
        p.parser.success=False
    p[0] = p[1] + p[3] + "mod\n"

def p_Term_paragem(p):
    "Term : Factor"
    p[0] = p[1]

def convertType(p,s):
    r = ""
    if s=="f":
        if p.parser.current_type=="None" :
            p.parser.current_type="f"
        elif p.parser.current_type=="":
            r = "ftoi\n"
    elif s=="":
        if p.parser.current_type=="None" :
            p.parser.current_type=""
        elif p.parser.current_type=="f" :
            r = "itof\n"
    return r

def p_Factor(p):
    "Factor : INT"
    p[0] = "pushi " + p[1] + "\n" + convertType(p,"")

def p_Factor_neg(p):
    "Factor : '-' INT"
    p[0] = "pushi " + p[2] + "\n" + "pushi -1\n" + "mul\n" + convertType(p,"")

def p_FactorF(p):
    "Factor : FLOAT"
    p[0] = "pushf " + p[1] + "\n" + convertType(p,"f")

def p_FactorF_neg(p):
    "Factor : '-' FLOAT"
    p[0] = "pushf " + p[2] + "\n" + "pushf -1.0\n" + "fmul\n" + convertType(p,"f")

def p_Factor_var(p):
    "Factor : ID"
    k = p.parser.localizacao.get(p[1].strip())
    t = p.parser.types.get(p[1].strip())
    p[0] = "pushg " + str(k) +"\n" + convertType(p,t)

def p_Factor_var_neg(p):
    "Factor : '-' ID"
    k = p.parser.localizacao.get(p[2].strip())
    t = p.parser.types.get(p[2].strip())
    p[0] = "pushg " + str(k) +"\n" + convertType(p,t)
    if p.parser.current_type=="" :
        p[0] += "pushi -1\n" + "mul\n"
    elif p.parser.current_type=="f" :
        p[0] += "pushf -1.0\n" + "fmul\n"

def p_Factor_func(p):
    "Factor : CALL ID"
    if p.parser.current_type=="" :
        p[0]="pushi 0\n"
    elif p.parser.current_type=="f":
        p[0]="pushf 0.0\n"
    p[0]+="pusha "+p[2].strip()+"\ncall\n"

def p_error(p):
    print("Erro sintático: ", p)
    parser.success = False

# Build the parser

parser = yacc.yacc()

# Read input and parse it by line

import sys

if len(sys.argv)==1 : #Verifica se foi fornecido um ficheiro para compilar
    print("Nenhum ficheiro escolhido para compilar")
else :
    print("Compiling: ",sys.argv[1]) # Imprime ficheiro de onde está a ler

read = open(sys.argv[1], "r")

if len(sys.argv)==2 : #Verifica se foi fornecido um ficheiro destino para o código pseudo-máquina
    filename = "a.vm"
else :
    filename = sys.argv[2]

parser.compiled = "" #Texto compilado guarda-se na string para escrever no fim da compilação caso tenha sido successful

parser.localizacao = {} #Localização das variáveis globais inicialmente declaradas na stack

parser.types={} #Tipos de cada variável declarada

parser.var=0 #Número de variáveis globais declaradas (Serve para incrementar a localização delas)

parser.success=True #Determina se a compilação foi bem sucedida

parser.current_type="None" #Determina o tipo da operação atual, pode ser ""(Inteiro) "f"(Float) "None"(Não definido)

parser.label_in=0 #Serve para declarar as etiquetas dos ciclos, funções e condições

parser.label_out=0 #Serve para declarar as etiquetas dos ciclos, funções e condições

content=""
for linha in read:
    if not(linha.strip().startswith('#')) : #Remove qualquer linha começada por # pois estas linhas são de comentário
        content += linha
parser.parse(content)

if parser.success:
    print("Compilação bem sucedida")
    print("Creating: ",filename)
    write = open(filename, "w+")
    write.write(parser.compiled)
else:
    print("Compilação mal sucedida")