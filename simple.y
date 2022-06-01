%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAXSTLEN 128
#define YYSTYPE char*

extern int yylineno;
extern int indexSP;
extern int indexTabP;
extern int column;
extern char* yytext;
extern char* yylval;
extern char line_buffer[1024];
extern int tokenCounter;
extern char* filename;
typedef enum{TYPE, ID} semanticRecType;
char typeBuffer[MAXSTLEN*4];
int typeBufferIndex = 0;

typedef struct SemanticRec
{
    semanticRecType type;
    char value[MAXSTLEN*2];
} semanticRec;

semanticRec semanticPile[MAXSTLEN*4];

typedef struct SymbolRec
{
    char type[MAXSTLEN*2];
    char id[MAXSTLEN*2];
} symbolRec;

typedef struct SymbolTab
{
	symbolRec records[MAXSTLEN*4];
	int index;
} symbolTab;

symbolTab symbolTables[MAXSTLEN];

#define YYERROR_VERBOSE 1

void concatType(char* type){
	for(int i = 0; i < strlen(type); i++){
		typeBuffer[typeBufferIndex] = type[i];
		typeBufferIndex++;
	}
	typeBuffer[typeBufferIndex] = ' ';
	typeBufferIndex++;
}

void clear_buffer(void){
    memset(typeBuffer, 0, sizeof typeBuffer);
    typeBufferIndex = 0;
}

void pushSP(semanticRec rs1){
	if(indexSP == MAXSTLEN*4){
		fprintf(stdout, "Semantic Stack Overflow");
		exit(-1);
	} else {
		semanticPile[indexSP] = rs1;
		indexSP++;
	}
}

void popSP(){
	semanticPile[indexSP-1] = semanticPile[indexSP];
	indexSP--;
}

semanticRec retrieveSP(semanticRecType type){
	int i;
	for(i = indexSP-1; semanticPile[i].type != type && i>0; i--);
	return semanticPile[i];
}

void deleteSP(semanticRecType type){
	int i;
	for(i = indexSP-1; semanticPile[i].type != type; i--);
	for(i; i < indexSP; i++){
		semanticPile[i] = semanticPile[i+1];
	}
}

void update(semanticRecType type, char* value){
	int i;
	for(i = indexSP-1; semanticPile[i].type != type; i--);
	strcpy(semanticPile[i].value, value);
}

semanticRec createRS(semanticRecType type){
	semanticRec rs;
	rs.type = type;
	return rs;
}

void openContext(){
	symbolTab sTab;
	symbolTables[indexTabP] = sTab;
	indexTabP++;
	symbolTables[indexTabP-1].index = 0;
}

void closeContext(){
	symbolRec* records = symbolTables[indexTabP-1].records;
	fprintf(stdout,"Symbol Table \n");
	for(int i = 0; i < symbolTables[indexTabP-1].index; i++){
		fprintf(stdout,"%d- Type: %s,\tID:%s\n", i, records[i].type, records[i].id);
	}
	indexTabP--;
}

int lookUpTopTS(char* id){
	for(int i = 0; i < symbolTables[indexTabP-1].index; i++){
		if(!strcmp(symbolTables[indexTabP-1].records[i].id, id)) return 0;
	}
	return 1;
}

int checkUndecl(char* token){
	int result = !lookUpTopTS(token);
	if(result){
		fprintf(stderr,"In file %s\n", filename);
		fprintf(stderr,"error: %s already declared, in line: %d, in column: %d\n", token, yylineno, column);
		fprintf(stderr,"%s \n", line_buffer);
		for(int i = 0; i < column + tokenCounter - 2; i++)
			fprintf(stderr,"_");
		fprintf(stderr,"^\n");
	}
	return !result;
}

int lookUpTS(char* id){
	for(int i = indexTabP-1; i >= 0; i--){
		for(int j = 0; j < symbolTables[i].index; j++){
			if(!strcmp(symbolTables[i].records[j].id, id)) return 1;
		}
	}
	return 0;
}

void checkDecl(char* token){
	if(!lookUpTS(token)){
		fprintf(stderr,"In file %s\n", filename);
		fprintf(stderr,"error: %s undeclared, in line: %d, in column: %d\n", token, yylineno, column);
		fprintf(stderr,"%s \n", line_buffer);
		for(int i = 0; i < column + tokenCounter - 2; i++)
			fprintf(stderr,"_");
		fprintf(stderr,"^\n");
	}
}

void saveType(char* currentToken){
	currentToken[strlen(currentToken)-1] = '\0';
	semanticRec rs;
	rs = createRS(TYPE);
	strcpy(rs.value, currentToken);
	pushSP(rs);
	clear_buffer();
}

void saveID(char* currentToken){
	semanticRec rs;
	rs = createRS(ID);
	strcpy(rs.value, currentToken);
	pushSP(rs);
}

void insertTS(char* pId, char* pType){
	symbolTab table = symbolTables[indexTabP-1];
	int tableIndex = symbolTables[indexTabP-1].index;
	char* id = table.records[tableIndex].id;
	char* type = table.records[tableIndex].type;
	strcpy(id, pId);
	strcpy(type, pType);
	strcpy(symbolTables[indexTabP-1].records[symbolTables[indexTabP-1].index].id, id);
	strcpy(symbolTables[indexTabP-1].records[symbolTables[indexTabP-1].index].type, type);
	symbolTables[indexTabP-1].index++;
}

void endDecl(){
	semanticRec rs;
	rs = retrieveSP(TYPE);
	char* type = rs.value;
	while(semanticPile[indexSP-1].type == ID){
		insertTS(semanticPile[indexSP-1].value, type);
		popSP();
	}
	popSP();
}

void yyerror(const char *str)
{
    fprintf(stderr,"In file %s\n", filename);
    fprintf(stderr,"error: %s, in line: %d, in column: %d\n", str, yylineno, column);
    fprintf(stderr,"%s \n", line_buffer);
	for(int i = 0; i < column + tokenCounter - 2; i++)
        fprintf(stderr,"_");
    fprintf(stderr,"^\n");
}
%}

%token IDENTIFIER

%token	I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token	PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token	AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token	SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token	XOR_ASSIGN OR_ASSIGN
%token	TYPEDEF_NAME ENUMERATION_CONSTANT

%token	TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token	CONST RESTRICT VOLATILE
%token	BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token	COMPLEX IMAGINARY 
%token	STRUCT UNION ENUM ELLIPSIS

%token	CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token	ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%start init

%define parse.error verbose


%%

id
	: IDENTIFIER {$$ = strdup(yytext);}

checkIdDecl
	: id {checkDecl($1);}

primary_expression
	: id {checkDecl($1);}
	| constant
	| string
	| '(' expression ')'
	| generic_selection
	;

constant
	: I_CONSTANT		/* includes character_constant */
	| F_CONSTANT
	| ENUMERATION_CONSTANT	/* after it has been defined as such */
	;

enumeration_constant		/* before it has been defined as such */
	: id {saveID($1);}
	;

string
	: STRING_LITERAL
	| FUNC_NAME
	;

generic_selection
	: GENERIC '(' assignment_expression ',' generic_assoc_list ')'
	;

generic_assoc_list
	: generic_association
	| generic_assoc_list ',' generic_association
	;

generic_association
	: type_name ':' assignment_expression
	| DEFAULT ':' assignment_expression
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')'
	| postfix_expression '(' argument_expression_list ')'
	| postfix_expression '.' id {checkDecl($1);}
	| postfix_expression PTR_OP id {checkDecl($1);}
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	| '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}'
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list ',' assignment_expression
	;

unary_expression
	: postfix_expression
	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'
	| ALIGNOF '(' type_name ')'
	;

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;

cast_expression
	: unary_expression
	| '(' type_name ')' cast_expression
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression
	| multiplicative_expression '/' cast_expression
	| multiplicative_expression '%' cast_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression
	| additive_expression '-' multiplicative_expression
	;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression
	| shift_expression RIGHT_OP additive_expression
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression
	| relational_expression '>' shift_expression
	| relational_expression LE_OP shift_expression
	| relational_expression GE_OP shift_expression
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	;

and_expression
	: equality_expression
	| and_expression '&' equality_expression
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;

constant_expression
	: conditional_expression	/* with constraints */
	;

declaration
	: declaration_specifiers ';' {endDecl();}
	| declaration_specifiers init_declarator_list ';' {endDecl();}
	| static_assert_declaration {endDecl();}
	| error ';'
	;

declaration_specifiers
	: storage_class_specifier {concatType($1);} declaration_specifiers
	| storage_class_specifier {concatType($1); saveType(typeBuffer);}
	| type_specifier {concatType($1);} declaration_specifiers
	| type_specifier {concatType($1); saveType(typeBuffer);}
	| type_qualifier {concatType($1);} declaration_specifiers
	| type_qualifier {concatType($1); saveType(typeBuffer);}
	| function_specifier {concatType($1);} declaration_specifiers
	| function_specifier {concatType($1); saveType(typeBuffer);}
	| alignment_specifier {concatType($1);} declaration_specifiers
	| alignment_specifier {concatType($1); saveType(typeBuffer);}
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator '=' initializer
	| declarator
	;

storage_class_specifier
	: TYPEDEF {$$ = strdup(yytext);}	/* identifiers must be flagged as TYPEDEF_NAME */
	| EXTERN {$$ = strdup(yytext);}
	| STATIC {$$ = strdup(yytext);}
	| THREAD_LOCAL {$$ = strdup(yytext);}
	| AUTO {$$ = strdup(yytext);}
	| REGISTER {$$ = strdup(yytext);}
	;

enum
	: ENUM {$$ = strdup(yytext);}

saveEnum
	: enum {concatType($1); saveType(typeBuffer);}

type_specifier
	: VOID {$$ = strdup(yytext);}
	| CHAR {$$ = strdup(yytext);}
	| SHORT {$$ = strdup(yytext);}
	| INT {$$ = strdup(yytext);}
	| LONG {$$ = strdup(yytext);}
	| FLOAT {$$ = strdup(yytext);}
	| DOUBLE {$$ = strdup(yytext);}
	| SIGNED {$$ = strdup(yytext);}
	| UNSIGNED {$$ = strdup(yytext);}
	| BOOL {$$ = strdup(yytext);}
	| COMPLEX {$$ = strdup(yytext);}
	| IMAGINARY	 {$$ = strdup(yytext);}  	/* non-mandated extension */
	| atomic_type_specifier {$$ = strdup("");}
	| struct_or_union_specifier {endDecl(); $$ = strdup("struct_or_union");}
	| enum_specifier {endDecl(); $$ = strdup("enum");}
	| TYPEDEF_NAME  {$$ = strdup(yytext);}	/* after it has been defined as such */
	;

saveStructUnion
	: struct_or_union {concatType($1); saveType(typeBuffer);}

struct_or_union_specifier
	: saveStructUnion '{' {openContext();} struct_declaration_list '}' {closeContext();}
	| saveStructUnion save_id  '{' {openContext();} struct_declaration_list '}' {closeContext();}
	| saveStructUnion save_id
	;

struct_or_union
	: STRUCT {$$ = strdup(yytext);}
	| UNION {$$ = strdup(yytext);}
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list ';' {endDecl();}	/* for anonymous struct/union */
	| specifier_qualifier_list struct_declarator_list ';' {endDecl();}
	| static_assert_declaration {endDecl();}
	;

specifier_qualifier_list
	: type_specifier {concatType($1);} specifier_qualifier_list
	| type_specifier {concatType($1); saveType(typeBuffer);}
	| type_qualifier {concatType($1);} specifier_qualifier_list
	| type_qualifier {concatType($1); saveType(typeBuffer);}
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: ':' constant_expression
	| declarator ':' constant_expression
	| declarator
	;

save_id
	: id {saveID($1);}

saveInt
	: '{' {concatType("int"); saveType(typeBuffer);}

enum_specifier
	: saveEnum saveInt enumerator_list '}' {endDecl();}
	| saveEnum saveInt enumerator_list ',' '}' {endDecl();}
	| saveEnum save_id saveInt enumerator_list '}' {endDecl();}
	| saveEnum save_id saveInt enumerator_list ',' '}' {endDecl();}
	| saveEnum save_id
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator	/* identifiers must be flagged as ENUMERATION_CONSTANT */
	: enumeration_constant '=' constant_expression
	| enumeration_constant
	;

atomic_type_specifier
	: ATOMIC '(' type_name ')'
	;

type_qualifier
	: CONST {$$ = strdup(yytext);}
	| RESTRICT {$$ = strdup(yytext);}
	| VOLATILE {$$ = strdup(yytext);}
	| ATOMIC {$$ = strdup(yytext);}
	;

function_specifier
	: INLINE {$$ = strdup(yytext);}
	| NORETURN {$$ = strdup(yytext);}
	;

alignment_specifier
	: ALIGNAS '(' type_name ')'
	| ALIGNAS '(' constant_expression ')'
	;

declarator
	: pointer direct_declarator
	| direct_declarator
	;

direct_declarator
	: id {if(checkUndecl($1)) saveID($1);}
	| '(' declarator ')'
	| direct_declarator '[' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_declarator '[' STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	| direct_declarator '[' assignment_expression ']'
	| direct_declarator '(' {openContext();} parameter_type_list ')'
	| direct_declarator '(' ')'
	| direct_declarator '(' identifier_list ')'
	;

pointer
	: '*' type_qualifier_list pointer
	| '*' type_qualifier_list
	| '*' pointer
	| '*'
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;


parameter_type_list
	: parameter_list ',' ELLIPSIS
	| parameter_list
	;

parameter_list
	: parameter_declaration {endDecl();}
	| parameter_list ',' parameter_declaration {endDecl();}
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

identifier_list
	: id
	| identifier_list ',' id
	;

type_name
	: specifier_qualifier_list abstract_declarator
	| specifier_qualifier_list
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' '*' ']'
	| '[' STATIC type_qualifier_list assignment_expression ']'
	| '[' STATIC assignment_expression ']'
	| '[' type_qualifier_list STATIC assignment_expression ']'
	| '[' type_qualifier_list assignment_expression ']'
	| '[' type_qualifier_list ']'
	| '[' assignment_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' '*' ']'
	| direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list ']'
	| direct_abstract_declarator '[' assignment_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

initializer
	: '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	| assignment_expression
	;

initializer_list
	: designation initializer
	| initializer
	| initializer_list ',' designation initializer
	| initializer_list ',' initializer
	;

designation
	: designator_list '='
	;

designator_list
	: designator
	| designator_list designator
	;

designator
	: '[' constant_expression ']'
	| '.' id
	;

static_assert_declaration
	: STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
	;

statement
	: {saveType("label");} labeled_statement
	| compound_statement
	| expression_statement
	| {openContext();} selection_statement
	| {openContext();} iteration_statement
	| jump_statement
	;

labeled_statement
	: id {saveID($1);} ':' statement {endDecl();}
	| CASE constant_expression ':' {openContext();} statement
	| DEFAULT ':' {openContext();} statement
	;

compound_statement
	: '{' '}' {closeContext();}
	| '{' block_item_list '}' {closeContext();}
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

block_item
	: declaration
	| {openContext();} statement
	;

expression_statement
	: ';'
	| expression ';'
	;

selection_statement
	: IF '(' expression ')' statement ELSE statement
	| IF '(' expression ')' statement
	| SWITCH '(' expression ')' statement
	;

iteration_statement
	: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	| FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement
	| FOR '(' declaration expression_statement expression ')' statement
	;

jump_statement
	: GOTO checkIdDecl ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';'
	| RETURN expression ';'
	;

init
	: {openContext();} translation_unit {closeContext();}
	;

translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement {endDecl();}
	| declaration_specifiers declarator compound_statement {endDecl();}
	| error compound_statement
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

%%
