'''
T:{ID,INT,FLOAT,STRING,READ,WRITE,EQUAL,BIGGER,BIGGEREQUAL,SMALLER,SMALLEREQUAL,RET,IF,DO,ELSE,
    WHILE,FUNC,CALL,REPEAT,UNTIL,FOR,';','=','+','-','*','/','%','{','}','&','|',':','[',']'}

N:{Inicio,Start,Variaveis,Codigo,Declaracoes,Declaracao,
    Type,Array,ContArray,Expr,Term,Factor,Funcao,
    DecFunc,Ret,ListInstrucao,Instrucao,Atribuicao,Var,
    ContContinuacaoVar,Leitura,Escrita,Condicao,Ciclo,
    ExprCond,Do,Else,Cond,Ciclo,CicloWhile,While,Repeat,Until,
    CicloRepeat,CicloFor,AtribuicaoInicial}

S: Inicio

P:{
    Rule 0 S’ ->Inicio
    Rule 1 Inicio -> Variaveis Start Codigo
    Rule 2 Start -> 
    Rule 3 Variaveis -> Declaracoes
    Rule 4 Declaracoes -> Declaracoes Declaracao
    Rule 5 Declaracoes -> <empty>
    Rule 6 Declaracao -> Type ID ;
    Rule 7 Declaracao -> Type ID = Expr ;
    Rule 8 Declaracao -> Type Array ID ;
    Rule 9 Type -> ID
    Rule 10 Array -> '[' INT ']' ContArray
    Rule 11 ContArray -> [ INT ]
    Rule 12 ContArray -> <empty>
    Rule 13 Expr -> Expr + Term
    Rule 14 Expr -> Expr - Term
    Rule 15 Expr -> Term
    Rule 16 Term -> Term * Factor
    Rule 17 Term -> Term / Factor
    Rule 18 Term -> Term % Factor
    Rule 19 Term -> Factor
    Rule 20 Factor -> INT
    Rule 21 Factor -> - INT
    Rule 22 Factor -> FLOAT
    Rule 23 Factor -> - FLOAT
    Rule 24 Factor -> STRINGInstrucao
    Rule 26 Factor -> ID
    Rule 30 Codigo -> Codigo Funcao
    Rule 31 Codigo -> Codigo Instrucao
    Rule 32 Codigo -> <empty>
    Rule 33 Funcao -> DecFunc '{' Variaveis ListInstrucao EndFunc
    Rule 34 DecFunc -> FUNC ID
    Rule 35 EndFunc -> '}'
    Rule 36 Ret -> RET Expr ;
    Rule 37 ListInstrucao -> ListInstrucao Instrucao
    Rule 38 ListInstrucao ->
    Rule 39 Instrucao -> Atribuicao
    Rule 40 Instrucao -> Leitura
    Rule 41 Instrucao -> Escrita
    Rule 42 Instrucao -> Condicao
    Rule 43 Instrucao -> Ciclo
    Rule 44 Instrucao -> Ret
    Rule 45 Atribuicao -> Var = Expr ;
    Rule 46 Var -> ID ContinuacaoVar
    Rule 47 ContinuacaoVar -> '[' INT ']' ContContinuacaoVar
    Rule 48 ContinuacaoVar -> 
    Rule 49 ContContinuacaoVar -> '[' INT ']'
    Rule 50 ContContinuacaoVar -> 
    Rule 51 Leitura -> READ Var ;
    Rule 52 Escrita -> WRITE Expr ;
    Rule 53 Condicao -> IF ExprCond Do ListInstrucao ;
    Rule 54 Condicao -> IF ExprCond Do ListInstrucao Else ListInstrucao ;
    Rule 55 Do -> DO
    Rule 56 Else -> ELSE
    Rule 57 ExprCond ->ExprCond & Cond
    Rule 58 ExprCond ->ExprCond | Cond
    Rule 59 ExprCond -> Cond
    Rule 60 Cond -> Expr EQUAL Expr
    Rule 61 Cond -> Expr NOTEQUAL Expr
    Rule 62 Cond -> Expr BIGGER Expr
    Rule 63 Cond -> Expr SMALLER Expr
    Rule 64 Cond -> Expr BIGGEREQUAL Expr
    Rule 65 Cond ->Expr SMALLEREQUAL Expr
    Rule 66 Ciclo -> CicloWhile 
    Rule 67 Ciclo -> CicloRepeat
    Rule 68 Ciclo -> CicloFor
    Rule 69 CicloWhile -> While ExprCond Do '{' ListInstrucao '}'
    Rule 70 While -> WHILE
    Rule 71 CicloRepeat -> Repeat ListInstrucao Until ExprCond
    Rule 72 Repeat -> REPEAT
    Rule 73 Until -> UNTIL
    Rule 74 CicloFor -> FOR '(' AtribuicaoInicial ExprCond EndCond Atribuicao ')' ListInstrucao
    Rule 75 AtribuicaoInicial -> Atribuicao
    Rule 76 EndCond -> ;
}
'''

import ply.yacc as yacc

from interpretador_lex import tokens,literals

def p_Inicio(p):
    "Inicio : Variaveis Start Codigo"

def p_Start(p):
    "Start : "
    p.parser.compiled+="start\n"

def p_Variaveis(p):
    "Variaveis : '{' Declaracoes '}'"

def p_Declaracoes(p):
    "Declaracoes : Declaracoes Declaracao"

def p_Declaracoes_paragem(p):
    "Declaracoes : "

def getAndIncFuncVarCounter(p,inc):
    varNumAtual = p.parser.func_var_counter.get(p.parser.current_func)
    p.parser.func_var_counter[p.parser.current_func] = varNumAtual + inc
    return varNumAtual

def addVarToFunc(p,varname,inc):
    p.parser.func_vars[p.parser.current_func][varname] = getAndIncFuncVarCounter(p,inc)
    p.parser.func_var_types[p.parser.current_func][varname] = p.parser.current_type

def addVar(p,varName,inc):
    linhaDec = p.parser.func_vars.get(p.parser.current_func).get(varName)
    if linhaDec == None: #não foi declarado na função atual
        linhaDec = p.parser.func_vars.get("0").get(varName) #pode ter sido declarada globalmente
        if linhaDec is None: #não foi declarada globalmente nem localmente logo pode ser adicionada
            addVarToFunc(p,varName,inc)
        else: #foi declarada globalmente e está a ser redeclarada localmente
            p.parser.success=False
            error("Variável "+varName+" declarada localmente na função "+p.parser.current_func+" depois de ter sido declarada globalmente na "+str(linhaDec+1)+ "ª declaração",p)
    else:
        if p.parser.current_func=="0": #está a ser redeclarada globalmente
            error("Variável global "+varName+" foi redeclarada na linha"+str(p.parser.func_var_counter.get(p.parser.current_func)+1)+"das declarações depois de ter sido declarada na linha"+str(linhaDec+1),p)        
        else: #está a ser redeclarada localmente
            error("Variável "+varName+" redeclarada na função "+p.parser.current_func+" como "+str(p.parser.func_var_counter.get(p.parser.current_func))+"ª declaração depois de ter sido declarada como"+str(linhaDec+1)+"ª",p) 


def p_Declaracao_var_simple(p):
    "Declaracao : Type ID ';'"
    varName = p[2].strip()
    addVar(p,varName,1)
    if p.parser.current_type=="":
        p.parser.compiled+= "pushi 0\n"
    elif p.parser.current_type=="f":
        p.parser.compiled+= "pushf 0.0\n"
    elif p.parser.current_type=="s":
        p.parser.compiled+= "pushs \"\"\n"
    p.parser.current_type=None

def p_Declaracao_var_complex(p):
    "Declaracao : Type ID '=' Expr ';'"
    varName = p[2].strip()
    addVar(p,varName,1)
    p.parser.compiled+=p[4]
    p.parser.current_type=None

def p_Declaracao_array(p):
    "Declaracao : Type Array ID ';'"
    varName = p[3].strip()
    array=p[2].strip().split(":")
    lines = int(array[0])
    columns = int(array[1])
    s = p.parser.func_array_info.get(p.parser.current_func)
    if s is None:
        p.parser.func_array_info.update({p.parser.current_func : {}})
    p.parser.func_array_info[p.parser.current_func][varName]=[lines,columns]
    addVar(p,varName,lines*columns)
    p.parser.current_type=None

def p_Type(p):
    "Type : ID"
    if p[1]=="int" :
        p.parser.current_type=""
    elif p[1]=="float" :
        p.parser.current_type="f"
    elif p[1]=="string" :
        p.parser.current_type="s"
    else :
        error("ERROR: Tipo Desconhecido \""+p[1]+"\"",p)

def p_Array(p):
    "Array : '[' INT ']' ContArray"
    amount = int(p[2]) * int(p[4])
    p[0] = p[2]+":"+p[4]
    if p.parser.current_type=="":
        string = "pushi 0\n"
    elif p.parser.current_type=="f":
        string = "pushf 0.0\n"
    elif p.parser.current_type=="s":
        string="pushs \"\"\n"
    while amount>0:
        p.parser.compiled+=string
        amount-=1

def p_ContArray(p):
    "ContArray : '[' INT ']'"
    p[0] = p[2]

def p_ContArray_paragem(p) :
    "ContArray : " 
    p[0] = "1"

def p_Expr_add(p):
    "Expr : Expr '+' Term"
    p[0] = p[3] + p[1]
    if p.parser.current_type=="s":
        p[0]+= "concat\n"
    else:
        p[0]+= p.parser.current_type + "add\n"

def p_Expr_sub(p):
    "Expr : Expr '-' Term"
    if p.parser.current_type=="s":
        error("ERRO: A operação '-' não pode ser utilizada para strings",p)
    else:
        p[0] = p[1] + p[3] + p.parser.current_type + "sub\n"

def p_Expr_paragem(p):
    "Expr : Term"
    p[0] = p[1]

def p_Term_mul(p):
    "Term : Term '*' Factor"
    if p.parser.current_type=="s":
        error("ERRO: A operação '/' não pode ser utilizada para strings",p)
    else:
        p[0] = p[1] + p[3] + p.parser.current_type + "mul\n"

def p_Term_div(p):
    "Term : Term '/' Factor"
    if p.parser.current_type=="s":
        error("ERRO: A operação '/' não pode ser utilizada para strings",p)
    else:
        p[0] = p[1] + p[3] + p.parser.current_type + "div\n"

def p_Term_mod(p):
    "Term : Term '%' Factor"
    if p.parser.current_type=="f":
        error("ERRO: A operação % não pode ser utilizada para floats", p)
    elif p.parser.current_type=="s":
        error("ERRO: A operação % não pode ser utilizada para strings", p)
    else:
        p[0] = p[1] + p[3] + "mod\n"

def p_Term_paragem(p):
    "Term : Factor"
    p[0] = p[1]

def convertType(p,s):
    r = ""
    if s=="f":
        if p.parser.current_type is None :
            p.parser.current_type="f"
        elif p.parser.current_type=="":
            r = "ftoi\n"
        elif p.parser.current_type=="s":
            r = "strf\n"
    elif s=="":
        if p.parser.current_type is None:
            p.parser.current_type=""
        elif p.parser.current_type=="f" :
            r = "itof\n"
        elif p.parser.current_type=="s":
            r = "stri\n"
    elif s=="s":
        if p.parser.current_type is None:
            p.parser.current_type="s"
        elif p.parser.current_type=="":
            r = "atoi\n"
        elif p.parser.current_type=="f":
            r = "atof\n"
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

def p_FactorS(p):
    "Factor : STRING"
    p[0] = "pushs " + p[1] + "\n" + convertType(p,"s")

def p_Factor_func(p):
    "Factor : CALL ID"
    if p.parser.current_type=="" :
        p[0]="pushi 0\n"
    elif p.parser.current_type=="f":
        p[0]="pushf 0.0\n"
    elif p.parser.current_type=="s":
        p[0]="pushs \"\"\n"
    p[0]+="pusha "+p[2].strip()+"\ncall\n"

def p_Factor_var_simple(p):
    "Factor : ID"
    varName=p[1].strip()
    k = p.parser.func_vars.get(p.parser.current_func).get(varName)
    t = p.parser.func_var_types.get(p.parser.current_func).get(varName)
    if k is None or p.parser.current_func == "0":
        k = p.parser.func_vars.get("0").get(varName)
        t = p.parser.func_var_types.get("0").get(varName)
        if k is None:
            error("Variavel "+varName+" não declarada",p)
        else:
            p[0] = "pushg " + str(k) + "\n" + convertType(p,t)
    else:
        p[0] = "pushl " + str(k) + "\n" + convertType(p,t)


def p_Factor_var_array(p):
    "Factor : ID '[' INT ']' ArrayCol"
    varName=p[1].strip()
    line = int(p[3])
    column = int(p[5])
    k = p.parser.func_array_info.get(p.parser.current_func).get(varName)
    if k is None or p.parser.current_func == "0":
        k = p.parser.func_array_info.get("0").get(varName)
    if k is None:
        error("Array"+p[1]+"não declarado",p)
    if line<0:
        error("Linha acedida no array tem índice negativo",p)
    if column<0:
        error("Coluna acedida no array tem índice negativo",p)
    if k[0]<=line:
        error("Array"+p[1]+"tem"+k[0]+"linhas e foi acedida a linha número "+line-1,p)
    elif k[1]<=column:
        error("Array"+p[1]+"tem"+k[1]+"colunas e foi acedida a coluna número "+column-1,p)
    else:
        pos = p.parser.func_vars.get(p.parser.current_func).get(varName)
        t = p.parser.func_var_types.get(p.parser.current_func).get(varName)
        if pos is None or p.parser.current_func == "0":
            pos = p.parser.func_vars.get("0").get(varName)
            t = p.parser.func_var_types.get("0").get(varName)
            place=pos+line*k[1]+column
            p[0] = "pushg " + str(place) + "\n" + convertType(p,t)
        else:
            place=pos+line*k[1]+column
            p[0] = "pushl " + str(place) + "\n" + convertType(p,t)

def p_ArrayCol_cols(p):
    "ArrayCol : '[' INT ']'"
    p[0]=p[2]

def p_ArrayCol_no_cols(p):
    "ArrayCol : "
    p[0]="0"

def p_Codigo_decFunc(p):
    "Codigo : Codigo Funcao"

def p_Codigo_Instrucao(p):
    "Codigo : Codigo Instrucao"

def p_Codigo_paragem(p):
    "Codigo : "

def p_Funcao(p):
    "Funcao : DecFunc '{' Variaveis ListInstrucao EndFunc"
    p.parser.current_func = "0"

def p_DecFunc(p):
    "DecFunc : FUNC ID" #Adiciona todas as informações da função para seres atualizadas aquando da interpretação da mesma
    p.parser.stack.append(p.parser.label)
    p.parser.label+=1
    p.parser.compiled+= "jump af" + str(p.parser.stack[-1]) + "\n" + p[2].strip() + " :\n"
    p.parser.current_func = p[2].strip()
    p.parser.func_var_counter[p.parser.current_func] = 0
    p.parser.func_vars[p.parser.current_func]={}
    p.parser.func_var_types[p.parser.current_func]={}

def p_EndFunc(p):
    "EndFunc : '}'"
    p.parser.compiled+="af" + str(p.parser.stack.pop()) + " :\n"
    p.parser.current_type=None
    
def p_Ret(p):
    "Ret : RET Expr ';'"
    if p.parser.current_func=="0":
        error("Linha de retorno de valor colocada globalmente",p)
    p.parser.compiled+= p[2] + "storel -1\nreturn\n"
    

def p_ListInstrucao(p):
    "ListInstrucao : ListInstrucao Instrucao"

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

def p_Instrucao_return(p):
    "Instrucao : Ret"

def atribuiValor(p,array):
    stringResultante = ""
    k = p.parser.func_vars.get(p.parser.current_func).get(array[0].strip())
    v=p.parser.current_func
    if k is None or p.parser.current_func == "0":
        v="0"
        k = p.parser.func_vars.get("0").get(array[0].strip())
        if k is None:
            error("Variável"+ array[0]+"não declarada",p)
        stringResultante+= "storeg "
    else:
        stringResultante+= "storel "
    if len(array)==1:
        stringResultante+= str(k) + "\n"
    else:
        info = p.parser.func_array_info.get(v).get(array[0].strip())
        if int(info[0])<=int(array[1]):
            error("Array"+array[0]+"tem"+info[0]+"linhas e foi acedida a linha número "+str(int(array[1])-1),p)
        elif int(info[1])<=int(array[2]):
            error("Array"+array[0]+"tem"+info[1]+"colunas e foi acedida a coluna número "+str(int(array[2])-1),p)
        else:
            stringResultante+=str(k+int(array[1])*int(info[1])+int(array[2])) +"\n"
    return stringResultante

def p_Atribuicao(p):
    "Atribuicao : Var '=' Expr ';'"
    p[0] = p[3]
    array=p[1].strip().split(":")
    p[0]+=atribuiValor(p,array)
    p.parser.current_type=None

def p_Var(p):
    "Var : ID ContinuacaoVar"
    t = p.parser.func_var_types.get(p.parser.current_func).get(p[1].strip())
    if t==None:
        t = p.parser.func_var_types.get("0").get(p[1].strip())
    if t is None:
        error("Variável "+p[1]+" não declarada",p)
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
    array = p[2].strip().split(":")
    t = p.parser.func_var_types.get(p.parser.current_func).get(array[0])
    if t is None or p.parser.current_func == "0":
        t = p.parser.func_var_types.get("0").get(array[0])
    p.parser.compiled+="read\n"
    p.parser.compiled+="dup 1\n"
    if t=="" :
        p.parser.compiled+="atoi\n"
    elif t=="f":
        p.parser.compiled+="atof\n"
    p.parser.compiled+= atribuiValor(p,array) + "free\n"
    p.parser.current_type=None

def p_Escrita(p):
    "Escrita : WRITE Expr ';'"
    p.parser.compiled+=p[2]
    if p.parser.current_type=="" :
        p.parser.compiled+="writei\n"
    elif p.parser.current_type=="f" :
        p.parser.compiled+="writef\n"
    elif p.parser.current_type=="s":
        p.parser.compiled+="writes\n"
    p.parser.current_type=None

def p_Condicao_if(p):
    "Condicao : IF ExprCond Do '{' ListInstrucao '}' ';'"
    p.parser.compiled+="e"+str(p.parser.stack.pop()) + " :\n"

def p_Condicao_if_else(p):
    "Condicao : IF ExprCond Do '{' ListInstrucao '}' Else '{' ListInstrucao '}' ';'"
    p.parser.compiled+="t"+str(p.parser.stack.pop())+" :\n"

def p_Do(p):
    "Do : DO"
    p.parser.stack.append(p.parser.label)
    p.parser.compiled+="jz e" + str(p.parser.stack[-1]) + "\n"
    p.parser.label+=1
    p.parser.current_type=None

def p_Else(p):
    "Else : ELSE"
    p.parser.compiled+="jump t" + str(p.parser.stack[-1]) +"\n"+"e"+str(p.parser.stack[-1]) + " :\n"

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
    p.parser.compiled+= p[1] + p[3] + "equal\n"

def p_Cond_not_equals(p):
    "Cond : Expr NOTEQUAL Expr"
    p.parser.compiled+= p[1] + p[3] + "equal\nnot\n"

def p_Cond_bigger(p):
    "Cond : Expr BIGGER Expr"
    p.parser.compiled+= p[1] + p[3] + "sup\n"

def p_Cond_smaller(p):
    "Cond : Expr SMALLER Expr"
    p.parser.compiled+= p[1] + p[3] + "inf\n"

def p_Cond_biggerequal(p):
    "Cond : Expr BIGGEREQUAL Expr"
    p.parser.compiled+= p[1] + p[3] + "supeq\n"

def p_Cond_smallerequal(p):
    "Cond : Expr SMALLEREQUAL Expr"
    p.parser.compiled+= p[1] + p[3] + "infeq\n"

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
    "CicloRepeat : Repeat ListInstrucao Until ExprCond"
    p.parser.compiled+="jz c" + str(p[3]) + "\n"

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
    print("Erro sintático: ", p) #Imprime erros de gramática
    parser.success = False

def error(arg0, p): #Imprime erros de compilação
    print(arg0)
    p.parser.success=False
    p[0]="0"

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

parser.success=True #Determina se a compilação foi bem sucedida

parser.compiled = "" #Texto compilado guarda-se na string para escrever no fim da compilação caso tenha sido successful

parser.current_type=None #Determina o tipo da operação atual, pode ser ""(Inteiro) "f"(Float) "s"(String) None(Não definido)

parser.current_func="0" #Determina a função onde se encontra atualmente de modo a poder produzir melhores mensagens de erro e controlar as declarações locais ("0" significa global)

parser.func_var_counter={} #Guarda o contador de variáveis de cada função
parser.func_var_counter.update({"0" : 0})

parser.func_vars={} #Guarda todas as variáveis das funções declaradas
parser.func_vars.update({"0" : {}})

parser.func_var_types={} #Guarda todos os tipos das variáveis de funções declaradas
parser.func_var_types.update({"0" : {}})

parser.func_array_info={} #Guarda um map para cada função com um tuplo de linhas e colunas que os arrays nessa função declarados têm
parser.func_array_info.update({"0" : {}}) 

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