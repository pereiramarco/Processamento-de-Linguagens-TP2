'''
T:{ID,INT,FLOAT,IN,OUT,EQUAL,BIGGER,BIGGEREQUAL,SMALLER,SMALLEREQUAL,RET,IF,DO,ELSE,
    WHILE,FUNC,CALL,';','=','+','-','*','/','%','{','}','&','|',':'}

N:{Inicio,Variaveis,Codigo,Declaracoes,Declaracao,Type,Expr,Term,Factor,Funcao,DecFunc,
    ListInstrucao,Instrucao,Atribuicao,Leitura,Escrita,Condicao,Ciclo,Var,ExprCond,Do,
    Else,Cond,While,Ret}

S: Inicio

P:{
    p0: Inicio => Start Variaveis Codigo
    p1: Variaveis => '{' Declaracoes '}'
    p2: Declaracoes => Declaracao Declaracoes
    p3: Declaracao => Type ID ContinuacaoDeclaracao
    p4: ContinuacaoDeclaracao => '=' Expr ';'
    p5:                       | 
    p6: Type => ID
    p7: Expr => Expr ContinuacaoExpr
    p8:      | Term
    p9: ContinuacaoExpr => '+' Term
    p10:                | '-' Term
    p11: Term => Term ContinuacaoTerm
    p12:      | Factor
    p13: ContinuacaoTerm => '*' Factor
    p14:                 | '/' Factor
    p15:                 | '%' Factor
    p16: Factor => INT
    p17:        | '-' INT
    p18:        | FLOAT
    p19:        | '-' FLOAT
    p20:        | ID
    p21:        | '-' ID
    p22:        | CALL ID
    p23: Codigo => Funcao Codigo 
    p24:        | Instrucao Codigo 
    p25:        | 
    p26: Funcao => DecFunc '{' ListInstrucao Ret '}'
    p27: DecFunc => FUNC ID
    p28: Ret => RET Expr ';' 
    p29: ListInstrucao => Instrucao ListInstrucao
    p30: ListInstrucao => 
    p31: Instrucao => Atribuicao
    p32:           | Leitura
    p33:           | Escrita
    p34:           | Condicao
    p35:           | Ciclo
    p36: Atribuicao => Var '=' Expr ';'
    p37: Var => ID
    p38: Leitura => READ Var ';'
    p39: Escrita => WRITE Var ';'
    p40: Condicao => IF ExprCond Do ListInstrucao ContinuacaoIf
    p41: CondicaoIf => Else ListInstrucao ';'
    p42:            | ';'
    p43: Do => DO
    p44: Else => ELSE
    p45: ExprCond => ExprCond ContinuacaoExprCond
    p46:          | Cond
    p47: ContinuacaoExprCond => '&' Cond 
    p48:                     | '|' Cond
    p49: Cond => Expr ContinuacaoCond
    p50: ContinuacaoCond => EQUAL Expr
    p51:                 | BIGGER Expr
    p52:                 | BIGGEREQUAL Expr
    p53:                 | SMALLER Expr
    p54:                 | SMALLEREQUAL Expr
    p55: Ciclo => CicloWhile
    p56:       | CicloRepeat       
    p57:       | CicloFor
    p58: CicloWhile => While ExprCond Do '{' ListInstrucao '}'
    p59: While => WHILE
    p60: CicloRepeat => Repeat '{' ListInstrucao '}' Until ExprCond
    p61: Repeat => REPEAT
    p62: Until => UNTIL
    p63: CicloFor => FOR '(' AtribuicaoInicial ExprCond EndCond Atribuicao ')' '{' ListInstrucao '}'
    p64: AtribuicaoInicial => Atribuicao
    p65: EndCond => ';'
}
'''

import ply.yacc as yacc

from interpretador_lex import tokens,literals

def p_Inicio(p):
    "Inicio : Start Variaveis Codigo"

def p_Start(p):
    "Start : "
    p.parser.compiled+="start\n"

def p_Variaveis(p):
    "Variaveis : '{' Declaracoes '}'"

def p_Declaracoes(p):
    "Declaracoes : Declaracao Declaracoes"

def p_Declaracoes_paragem(p):
    "Declaracoes : "

def getAndIncFuncVarCounter(p,inc):
    varNumAtual = p.parser.funcVarCounter.get(p.parser.funcaoAtual)
    p.parser.funcVarCounter[p.parser.funcaoAtual]=varNumAtual+inc
    return varNumAtual

def addVarToFunc(p,varname,inc):
    p.parser.funcVars[p.parser.funcaoAtual][varname] = getAndIncFuncVarCounter(p,inc)
    p.parser.funcVarTypes[p.parser.funcaoAtual][varname] = p.parser.current_type

def addVar(p,varName,inc):
    linhaDec = p.parser.funcVars.get(p.parser.funcaoAtual).get(varName)
    if linhaDec == None:
        linhaDec = p.parser.funcVars.get("0").get(varName)
        if linhaDec == None:
            addVarToFunc(p,varName,inc) 
        else:
            p.parser.success=False
            print("Variável ",varName," declarada localmente na função ",p.parser.funcaoAtual," depois de ter sido declarada globalmente na ",linhaDec+1, "ª declaração")
    else:
        p.parser.success=False
        if p.parser.funcaoAtual=="0":
            print("Variável global ",varName," foi redeclarada na linha",p.parser.funcVarCounter.get(p.parser.funcaoAtual)+1,"das declarações depois de ter sido declarada na linha",linhaDec+1)        
        else:
            print("Variável ",varName," redeclarada na função ",p.parser.funcaoAtual," como ",p.parser.funcVarCounter.get(p.parser.funcaoAtual),"ª declaração depois de ter sido declarada como",linhaDec+1,"ª") 


def p_Declaracao(p):
    "Declaracao : Type ContinuacaoDeclaracao"
    p.parser.current_type=None

def p_Type(p):
    "Type : ID"
    if p[1]=="int" :
        p.parser.current_type=""
    elif p[1]=="float" :
        p.parser.current_type="f"
    else :
        print("ERROR: Tipo Desconhecido \"",p[1],"\"")
        p.parser.success=False

def p_ContinuacaoDeclaracao(p):
    "ContinuacaoDeclaracao : ID ContContinuacaoDeclaracao"
    varName = p[1].strip()
    addVar(p,varName,1)
    p.parser.compiled+= p[2]

def p_ContinuacaoDeclaracao_array(p):
    "ContinuacaoDeclaracao : Array ID ';'"
    varName = p[2].strip()
    array=p[1].strip().split(":")
    lines = int(array[0])
    columns = int(array[1])
    s = p.parser.funcArrays.get(p.parser.funcaoAtual)
    if s==None:
        p.parser.funcArrays.update({p.parser.funcaoAtual : {}})
    else:
        p.parser.funcArrays[p.parser.funcaoAtual][varName]=[lines,columns]
    addVar(p,varName,lines*columns)

def p_ContContinuacaoDeclaracao(p):
    "ContContinuacaoDeclaracao : '=' Expr ';'"
    p[0]=p[2]

def p_ContContinuacaoDeclaracao_simples(p):
    "ContContinuacaoDeclaracao : ';'"
    if p.parser.current_type=="":
        p[0]= "pushi 0\n"
    elif p.parser.current_type=="f":
        p[0]= "pushf 0.0\n"

def p_Array(p):
    "Array : '[' INT ']' ContArray"
    amount = int(p[2]) * int(p[4])
    p[0] = p[2]+":"+p[4]
    if p.parser.current_type=="":
        string = "pushi 0\n"
    elif p.parser.current_type=="f":
        string = "pushf 0.0\n"
    while amount>0:
        p.parser.compiled+=string
        amount-=1

def p_ContArray(p):
    "ContArray : '[' INT ']'"
    p[0] = p[2]

def p_ContArray_paragem(p) :
    "ContArray : " 
    p[0] = "1"

def p_Expr_begin(p):
    "Expr : Expr ContinuacaoExpr"
    p[0] = p[1] + p[2]

def p_Expr_paragem(p):
    "Expr : Term"
    p[0] = p[1]

def p_ContinuacaoExpr_add(p):
    "ContinuacaoExpr : '+' Term"
    p[0] = p[2] + p.parser.current_type + "add\n"

def p_Expr_sub(p):
    "ContinuacaoExpr : '-' Term"
    p[0] = p[2] + p.parser.current_type + "sub\n"

def p_Term_begin(p):
    "Term : Term ContinuacaoTerm"
    p[0] = p[1] + p[2]

def p_Term_paragem(p):
    "Term : Factor"
    p[0] = p[1]

def p_ContinuacaoTerm_mul(p):
    "ContinuacaoTerm : '*' Factor"
    p[0] = p[2] + p.parser.current_type + "mul\n"

def p_ContinuacaoTerm_div(p):
    "ContinuacaoTerm : '/' Factor"
    p[0] = p[2] + p.parser.current_type + "div\n"

def p_ContinuacaoTerm_mod(p):
    "ContinuacaoTerm : '%' Factor"
    if p.parser.current_type=="f":
        print("ERRO: A operação % não pode ser utilizada para floats")
        p.parser.success=False
        p[0]="0"
    else :
        p[0] = p[2] + "mod\n"

def convertType(p,s):
    r = ""
    if s=="f":
        if p.parser.current_type==None :
            p.parser.current_type="f"
        elif p.parser.current_type=="":
            r = "ftoi\n"
    elif s=="":
        if p.parser.current_type==None :
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
    "Factor : ID ContinuacaoFactor"
    varName=p[1].strip()
    if (p[2] != None):
        array = p[2].strip().split(":")
        line = int(array[0])
        if len(array)>1:
            column = int(array[1])
        else:
            column = 0
        k = p.parser.funcArrays.get(p.parser.funcaoAtual).get(varName)
        if k == None or p.parser.funcaoAtual=="0":
            k = p.parser.funcArrays.get("0").get(varName)
            if k==None:
                p.parser.success=False
                p[0]="0"
                print("Array",p[1],"não declarado")
        if line<0:
            print("Linha acedida no array tem indice negativo")
        if column<0:
            print("Coluna acedida no array tem indice negativo")
        if k[0]<=line:
            p.parser.success=False
            p[0]="0"
            print("Array",p[1],"tem",k[0],"linhas e foi acedida a linha número ",line-1)
        elif k[1]<=column:
            p.parser.success=False
            p[0]="0"
            print("Array",p[1],"tem",k[1],"colunas e foi acedida a coluna número ",column-1)
        else:
            pos = p.parser.funcVars.get(p.parser.funcaoAtual).get(varName)
            t = p.parser.funcVarTypes.get(p.parser.funcaoAtual).get(varName)
            if pos == None or p.parser.funcaoAtual=="0":
                pos = p.parser.funcVars.get("0").get(varName)
                t = p.parser.funcVarTypes.get("0").get(varName)
                place=pos+line*k[1]+column
                p[0] = "pushg " + str(place) + "\n" + convertType(p,t)
            else:
                place=pos+line*k[1]+column
                p[0] = "pushl " + str(place) + "\n" + convertType(p,t)
    else:
        k = p.parser.funcVars.get(p.parser.funcaoAtual).get(varName)
        t = p.parser.funcVarTypes.get(p.parser.funcaoAtual).get(varName)
        if k == None or p.parser.funcaoAtual=="0":
            k = p.parser.funcVars.get("0").get(varName)
            t = p.parser.funcVarTypes.get("0").get(varName)
            if k == None:
                p.parser.success=False
                p[0]="0"
                print("Variavel ",varName," não declarada")
            else:
                p[0] = "pushg " + str(k) + "\n" + convertType(p,t)
        else:
            p[0] = "pushl " + str(k) + "\n" + convertType(p,t)

def p_ContinuacaoFactor_var_array(p):
    "ContinuacaoFactor : '[' INT ']' ContContinuacaoFactor"
    p[0] = p[2] + ":" + p[4]

def p_ContinuacaoFactor_var(p):
    "ContinuacaoFactor : "
    p[0] = None

def p_ContContinuacaoFactor_var_array_duplo(p):
    "ContContinuacaoFactor : '[' INT ']'"
    p[0]=p[2]

def p_ContContinuacaoFactor_var_array_simples(p):
    "ContContinuacaoFactor : "
    p[0]="0"

def p_Factor_func(p):
    "Factor : CALL ID"
    if p.parser.current_type=="" :
        p[0]="pushi 0\n"
    elif p.parser.current_type=="f":
        p[0]="pushf 0.0\n"
    p[0]+="pusha "+p[2].strip()+"\ncall\n"

def p_Codigo_decFunc(p):
    "Codigo : Funcao Codigo"

def p_Codigo_Instrucao(p):
    "Codigo : Instrucao Codigo"

def p_Codigo_paragem(p):
    "Codigo : "

def p_Funcao(p):
    "Funcao : DecFunc '{' Variaveis ListInstrucao Ret '}'"
    p.parser.funcaoAtual = "0"

def p_DecFunc(p):
    "DecFunc : FUNC ID"
    p.parser.stack.append(p.parser.label)
    p.parser.label+=1
    p.parser.compiled+= "jump af" + str(p.parser.stack[-1]) + "\n" + p[2].strip() + " :\n"
    p.parser.funcaoAtual = p[2].strip()
    p.parser.funcVarCounter[p.parser.funcaoAtual] = 0
    p.parser.funcVars[p.parser.funcaoAtual]={}
    p.parser.funcVarTypes[p.parser.funcaoAtual]={}
    
def p_Ret(p):
    "Ret : RET Expr ';'"
    p.parser.compiled+= p[2] + "storel -1\nreturn\naf" + str(p.parser.stack.pop()) + " :\n"

def p_ListInstrucao(p):
    "ListInstrucao : Instrucao ListInstrucao"

def p_ListInstrucao_paragem(p):
    "ListInstrucao : "

def p_Instrucao_atribuicao(p):
    "Instrucao : Atribuicao"
    p.parser.compiled+=p[1]

def p_Instrucao_reading(p):
    "Instrucao : Leitura"

def p_Instrucao_writing(p):
    "Instrucao : Escrita"

def p_Instrucao_condicao(p):
    "Instrucao : Condicao"

def p_Instrucao_ciclo(p):
    "Instrucao : Ciclo"

def p_Atribuicao(p):
    "Atribuicao : Var '=' Expr ';'"
    p[0] = p[3]
    array=p[1].strip().split(":")
    k = p.parser.funcVars.get(p.parser.funcaoAtual).get(array[0].strip())
    v=p.parser.funcaoAtual
    if k==None or p.parser.funcaoAtual=="0":
        v="0"
        k = p.parser.funcVars.get("0").get(array[0].strip())
        if k==None:
            p.parser.success=False
            print("Variável", array[0],"não declarada")
        p[0]+= "storeg "
    else:
        p[0]+= "storel "
    if len(array)==1:
        p[0]+= str(k) + "\n"
    else:
        info = p.parser.funcArrays.get(v).get(array[0].strip())
        if int(info[0])<=int(array[1]):
            p.parser.success=False
            print("Array",array[0],"tem",info[0],"linhas e foi acedida a linha número ",int(array[1])-1)
        elif int(info[1])<=int(array[2]):
            p.parser.success=False
            print("Array",array[0],"tem",info[1],"colunas e foi acedida a coluna número ",int(array[2])-1)
        else:
            p[0]+=str(k+int(array[1])*int(info[1])+int(array[2])) +"\n"   

    p.parser.current_type=None

def p_Var(p):
    "Var : ID ContinuacaoVar"
    t = p.parser.funcVarTypes.get(p.parser.funcaoAtual).get(p[1].strip())
    if t==None:
        t = p.parser.funcVarTypes.get("0").get(p[1].strip())
        if t==None:
            print("Variável ",p[1]," não declarada")
            p.parser.success=False
    p.parser.current_type=t
    p[0]=p[1]+p[2]

def p_ContinuacaoVar_array(p):
    "ContinuacaoVar : '[' INT ']' ContContinuacaoVar"
    p[0]=":"+p[2]+p[4]

def p_ContinuacaoVar_simples(p):
    "ContinuacaoVar : "
    p[0]=""

def p_ContContinuacaoVar_array_duplo(p):
    "ContContinuacaoVar : '[' INT ']'"
    p[0]=":" + p[2]

def p_ContContinuacaoVar_array_simples(p):
    "ContContinuacaoVar : "
    p[0]=":0"

def p_Leitura(p):
    "Leitura : READ Var ';'"
    t = p.parser.funcVarTypes.get(p.parser.funcaoAtual).get(p[2].strip())
    l = p.parser.funcVars.get(p.parser.funcaoAtual).get(p[2].strip())
    v=0
    if l == None or p.parser.funcaoAtual == "0":
        l = p.parser.funcVars.get("0").get(p[2].strip())
        t = p.parser.funcVarTypes.get("0").get(p[2].strip())
        v=1
    p.parser.compiled+="read\n"
    p.parser.compiled+="dup 1\n"
    if t=="" :
        p.parser.compiled+="atoi\n"
    elif t=="f":
        p.parser.compiled+="atof\n"
    if v==1:
        p.parser.compiled+="storeg " 
    else:
        p.parser.compiled+="storel "
    p.parser.compiled+= str(l) +"\nfree\n"
    p.parser.current_type=None

def p_Escrita(p):
    "Escrita : WRITE Var ';'"
    t = p.parser.funcVarTypes.get(p.parser.funcaoAtual).get(p[2].strip())
    l = p.parser.funcVars.get(p.parser.funcaoAtual).get(p[2].strip())
    if l == None or p.parser.funcaoAtual=="0":
        l = p.parser.funcVars.get("0").get(p[2].strip())
        t = p.parser.funcVarTypes.get("0").get(p[2].strip())
        p.parser.compiled+="pushl " + str(l) +"\n"
    else:
        p.parser.compiled+="pushg " + str(l) +"\n"
    if t=="" :
        p.parser.compiled+="writei\n"
    elif t=="f" :
        p.parser.compiled+="writef\n"
    p.parser.current_type=None

def p_Condicao_if(p):
    "Condicao : IF ExprCond Do ListInstrucao ContinuacaoIf"

def p_Continuacao_if(p):
    "ContinuacaoIf : Else ListInstrucao ';'"
    p.parser.compiled+="t"+str(p.parser.stack.pop())+" :\n"

def p_Continuacao_if_simples(p):
    "ContinuacaoIf : ';'"
    p.parser.compiled+="e"+str(p.parser.stack.pop()) + " :\n"

def p_Do(p):
    "Do : DO"
    p.parser.stack.append(p.parser.label)
    p.parser.compiled+="jz e" + str(p.parser.stack[-1]) + "\n"
    p.parser.label+=1
    p.parser.current_type=None

def p_Else(p):
    "Else : ELSE"
    p.parser.compiled+="jump t" + str(p.parser.stack[-1]) +"\n"+"e"+str(p.parser.stack[-1]) + " :\n"

def p_ExprCond_begin(p):
    "ExprCond : ExprCond ContinuacaoExprCond"

def p_ExprCond_paragem(p):
    "ExprCond : Cond"

def p_ContinuacaoExprCond_and(p):
    "ContinuacaoExprCond : '&' Cond"
    p.parser.compiled+="mul\n"

def p_ContinuacaoExprCond_or(p):
    "ContinuacaoExprCond : '|' Cond"
    p.parser.compiled+="add\n"

def p_Cond_begin(p):
    "Cond : Expr ContinuacaoCond"
    p.parser.compiled+= p[1] + p[2]

def p_ContinuacaoCond_equals(p):
    "ContinuacaoCond : EQUAL Expr"
    p[0]= p[2] + "equal\n"

def p_ContinuacaoCond_bigger(p):
    "ContinuacaoCond : BIGGER Expr"
    p[0]= p[2] + p.parser.current_type + "sup\n"

def p_ContinuacaoCond_bigger_equal(p):
    "ContinuacaoCond : BIGGEREQUAL Expr"
    p[0]= p[2] + p.parser.current_type + "supeq\n"

def p_ContinuacaoCond_smaller(p):
    "ContinuacaoCond : SMALLER Expr"
    p[0]= p[2] + p.parser.current_type + "inf\n"

def p_ContinuacaoCond_smaller_equal(p):
    "ContinuacaoCond : SMALLEREQUAL Expr"
    p[0]= p[2] + p.parser.current_type + "infeq\n"

def p_Ciclo_while(p):
    "Ciclo : CicloWhile"

def p_Ciclo_repeat(p):
    "Ciclo : CicloRepeat"

def p_Ciclo_for(p):
    "Ciclo : CicloFor"

def p_CicloWhile(p):
    "CicloWhile : While ExprCond Do '{' ListInstrucao '}'"
    pop1 = p.parser.stack.pop()
    pop2 = p.parser.stack.pop()
    p.parser.compiled+="jump c" + str(pop2) + "\n" + "e"+str(pop1) + " :\n"

def p_While(p):
    "While : WHILE"
    p.parser.stack.append(p.parser.label)
    p.parser.compiled+="c"+str(p.parser.stack[-1]) +" :\n"
    p.parser.label+=1

def p_CicloRepeat(p):
    "CicloRepeat : Repeat '{' ListInstrucao '}' Until ExprCond"
    p.parser.compiled+="jz c" + str(p[5]) + "\n"

def p_Repeat(p): 
    "Repeat : REPEAT"
    p.parser.stack.append(p.parser.label)
    p.parser.compiled+="jump u" + str(p.parser.stack[-1]) +"\n" + "c" + str(p.parser.stack[-1]) + ": \n"
    p.parser.label+=1
    p.parser.current_type = None

def p_Until(p):
    "Until : UNTIL"
    p[0] = p.parser.stack.pop()
    p.parser.compiled+= "u"+str(p[0]) + " :\n"

def p_CicloFor(p):
    "CicloFor : FOR '(' AtribuicaoInicial ExprCond EndCond Atribuicao ')' '{' ListInstrucao '}'"
    pop1 = p.parser.stack.pop()
    pop2 = p.parser.stack.pop()
    p.parser.compiled+=p[6]+"jump f" + str(pop2) + "\nff"+str(pop1) +" :\n"

def p_AtribuicaoInicial(p):
    "AtribuicaoInicial : Atribuicao"
    p.parser.stack.append(p.parser.label)
    p.parser.compiled+=p[1]+"f"+ str(p.parser.stack[-1]) +" :\n"
    p.parser.label+=1

def p_EndCond(p):
    "EndCond : ';'"
    p.parser.stack.append(p.parser.label)
    p.parser.compiled+="jz ff"+str(p.parser.stack[-1]) + "\n"
    p.parser.label+=1

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

parser.success=True #Determina se a compilação foi bem sucedida

parser.current_type=None #Determina o tipo da operação atual, pode ser ""(Inteiro) "f"(Float) None(Não definido)

parser.funcaoAtual="0" #Determina a função onde se encontra atualmente de modo a poder produzir melhores mensagens de erro e controlar as declarações locais ("0" significa global)

parser.funcVars={} #Guarda todas as variáveis das funções declaradas
parser.funcVars.update({"0" : {}})

parser.funcVarTypes={} #Guarda todos os tipos das variáveis de funções declaradas
parser.funcVarTypes.update({"0" : {}})

parser.funcVarCounter={} #Guarda o contador de variáveis de cada função
parser.funcVarCounter.update({"0" : 0})

parser.funcArrays={} #Guarda um map para cada função com um tuplo de linhas e colunas que os arrays nessa função declarados têm
parser.funcArrays.update({"0" : {}}) 

parser.label=0 #Serve para declarar as etiquetas dos ciclos, funções e condições

parser.stack=[] 

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