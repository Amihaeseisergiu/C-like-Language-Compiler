%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tema.h"

extern FILE* yyin;
extern char* yytext;
extern int yylineno;
node* table, *currentscope, *parmtable, *currentparmscope;
parameter* parmlist = NULL;
char* currentdecltype;
%}

%union {
int intval;
char charval;
float floatval;
char* stringval;
struct data* info;
}

%type <info>expr
%type <info>function_eval
%type <info>not
%type <stringval>tip
%type <info>functioncall
%type <intval>vec
%type <stringval>declwitype

%token INT FLOAT CHAR STRING BOOL <stringval>ID <stringval>STRINGVAL <intval>NR <charval>CHARVAL <floatval>FLOATVAL MAIN TRUE FALSE FOR IF ELSE WHILE CONST CONTAINER EVAL RETURN ATR AND OR EQEQ NEQ GEQ LEQ GT LW NOT PLS MIN MUL DIV MOD
%start start
%right NOT
%left MOD
%left PLS MIN
%left MUL DIV
%left AND OR NEQ EQEQ GEQ LEQ GT LW
%%
start: 
	 {
		table = new_node(new_struct(yylineno, "scope", "global", NULL, 0, NULL));
		currentscope = table;
		parmtable = new_node(new_struct(yylineno, "scope", "globalparameters", NULL, 0, NULL));
		currentparmscope = parmtable;
	 } 
	  starttypes 
	 {
		printf("program corect sintactic\n");
		FILE* f = fopen("symbol_table.txt", "w");
		fclose(f);
		f = fopen("symbol_table.txt", "a"); 
		print_tree(f, table);
		fclose(f);
	 }
     ; 

starttypes : global_scope main
		   | main
		   ;

main : tip MAIN '(' lista_parametrii ')' {currentscope = add_child(table, new_struct(yylineno, "scope", "main", NULL, 0, NULL));} bloc { currentscope = currentscope->parent;}
	 | tip MAIN '(' ')' {currentscope = add_child(table, new_struct(yylineno, "scope", "main", NULL, 0, NULL));} bloc {currentscope = currentscope->parent;}
	 ;

global_scope : declaratii ';'
			 | global_scope declaratii ';'
             | functie
             | global_scope functie
             | container
             | global_scope container
			 ;

declaratii : declwitype {currentdecltype = strdup($1);} ',' multipledecl
		   | declwitype
		   ;

multipledecl : declwotype
			 | multipledecl ',' declwotype
			 ;

declwitype : tip ID 
			 {
			 	if(find_variable(currentscope, $2)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable\033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $2, find_variable(currentscope, $2)); return 0;} 
				add_child(currentscope, new_struct(yylineno, $1, $2, NULL, 0, NULL)); free($2);
				$$ = strdup($1);
			 }
           | tip ID vec 
			 {
				if(find_variable(currentscope, $2)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable\033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $2, find_variable(currentscope, $2)); return 0;}
			 	add_child(currentscope, new_struct(yylineno, $1, $2, NULL, $3, NULL)); free($2);
				$$ = strdup($1);
			 }
		   | tip ID ATR expr 
			 {
				if(find_variable(currentscope, $2)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable \033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $2, find_variable(currentscope, $2)); return 0;}
				if(strstr($1, $4->type))
				{
					if(strstr($1, "int") || strstr($1,"bool"))
					{
						char value[32];
						snprintf(value, 32, "%d", $4->value.ival); 
						add_child(currentscope, new_struct(yylineno, $1, $2, value, 0, NULL)); 
						free($2); free($4);
					}
					else if(strstr($1, "float"))
					{
						char value[32];
						snprintf(value, 32, "%f", $4->value.fval); 
						add_child(currentscope, new_struct(yylineno, $1, $2, value, 0, NULL));
						free($2); free($4);
					}
					else if(strstr($1, "string"))
					{ 
						add_child(currentscope, new_struct(yylineno, $1, $2, $4->value.sval, 0, NULL)); 
						free($2); free($4);
					}
					else if(strstr($1, "char"))
					{
						char value[32];
						snprintf(value, 32, "%d", $4->value.cval); 
						add_child(currentscope, new_struct(yylineno, $1, $2, value, 0, NULL));
						free($2); free($4);
					}
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but assignment is of type \033[0;36m%s\033[0m\n", yylineno, $1, $4->type); return 0;}
				$$ = strdup($1);
			 }
           | tip ID vec ATR expr 
			 {
				if(find_variable(currentscope, $2)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable \033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $2, find_variable(currentscope, $2)); return 0;}
			 	if(strstr($1, $5->type))
				{
					if(strstr($1, "int") || strstr($1,"bool"))
					{
						char value[32];
						snprintf(value, 32, "%d", $5->value.ival); 
						node* tmp = add_child(currentscope, new_struct(yylineno, $1, $2, value, $3, NULL)); 
						for(int i = 0; i < tmp->data.dimlimit; i++)
							tmp->data.dimensions.idim[i] = $5->value.ival; 
						free($2); free($5);
					}
					else if(strstr($1, "float"))
					{
						char value[32];
						snprintf(value, 32, "%f", $5->value.fval); 
						node* tmp = add_child(currentscope, new_struct(yylineno, $1, $2, value, $3, NULL));
						for(int i = 0; i < tmp->data.dimlimit; i++)
							tmp->data.dimensions.fdim[i] = $5->value.fval;
						free($2); free($5);
					}
					else if(strstr($1, "string"))
					{ 
						node * tmp = add_child(currentscope, new_struct(yylineno, $1, $2, $5->value.sval, $3, NULL)); 
						for(int i = 0; i < tmp->data.dimlimit; i++)
						{
							tmp->data.dimensions.sdim[i] = (char* )malloc(strlen($5->value.sval));
							strcpy(tmp->data.dimensions.sdim[i], $5->value.sval);
						}
						free($2); free($5);
					}
					else if(strstr($1, "char"))
					{
						char value[32];
						snprintf(value, 32, "%d", $5->value.cval); 
						node* tmp = add_child(currentscope, new_struct(yylineno, $1, $2, value, $3, NULL));
						for(int i = 0; i < tmp->data.dimlimit; i++)
							tmp->data.dimensions.cdim[i] = $5->value.cval;
						free($2); free($5);
					}
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but assignment is of type \033[0;36m%s\033[0m\n", yylineno, $1, $5->type); return 0;}
				$$ = strdup($1);
			 }
		   | tip ID vec ATR '{' vecpar '}' 
			 {
				if(find_variable(currentscope, $2)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable \033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $2, find_variable(currentscope, $2)); return 0;}
			 	add_child(currentscope, new_struct(yylineno, $1, $2, NULL, $3, NULL)); free($2);
				$$ = strdup($1);
			 }
	   	   ;

declwotype : ID 
			 {
			 	if(find_variable(currentscope, $1)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable\033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $1, find_variable(currentscope, $1)); return 0;} 
				add_child(currentscope, new_struct(yylineno, currentdecltype, $1, NULL, 0, NULL)); free($1);
			 }
           | ID vec 
			 {
				if(find_variable(currentscope, $1)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable\033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $1, find_variable(currentscope, $1)); return 0;}
			 	add_child(currentscope, new_struct(yylineno, currentdecltype, $1, NULL, $2, NULL)); free($1);
			 }
		   | ID ATR expr 
			 {
				if(find_variable(currentscope, $1)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable \033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $1, find_variable(currentscope, $1)); return 0;}
				if(strstr(currentdecltype, $3->type))
				{
					if(strstr(currentdecltype, "int") || strstr(currentdecltype,"bool"))
					{
						char value[32];
						snprintf(value, 32, "%d", $3->value.ival); 
						add_child(currentscope, new_struct(yylineno, currentdecltype, $1, value, 0, NULL)); 
						free($1); free($3);
					}
					else if(strstr(currentdecltype, "float"))
					{
						char value[32];
						snprintf(value, 32, "%f", $3->value.fval); 
						add_child(currentscope, new_struct(yylineno, currentdecltype, $1, value, 0, NULL));
						free($1); free($3);
					}
					else if(strstr(currentdecltype, "string"))
					{ 
						add_child(currentscope, new_struct(yylineno, currentdecltype, $1, $3->value.sval, 0, NULL)); 
						free($1); free($3);
					}
					else if(strstr(currentdecltype, "char"))
					{
						char value[32];
						snprintf(value, 32, "%d", $3->value.cval); 
						add_child(currentscope, new_struct(yylineno, currentdecltype, $1, value, 0, NULL));
						free($1); free($3);
					}
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but assignment is of type \033[0;36m%s\033[0m\n", yylineno, currentdecltype, $3->type); return 0;}
			 }
           | ID vec ATR expr 
			 {
				if(find_variable(currentscope, $1)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable \033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $1, find_variable(currentscope, $1)); return 0;}
			 	if(strstr(currentdecltype, $4->type))
				{
					if(strstr(currentdecltype, "int") || strstr(currentdecltype,"bool"))
					{
						char value[32];
						snprintf(value, 32, "%d", $4->value.ival); 
						node* tmp = add_child(currentscope, new_struct(yylineno, currentdecltype, $1, value, $2, NULL)); 
						for(int i = 0; i < tmp->data.dimlimit; i++)
							tmp->data.dimensions.idim[i] = $4->value.ival; 
						free($1); free($4);
					}
					else if(strstr(currentdecltype, "float"))
					{
						char value[32];
						snprintf(value, 32, "%f", $4->value.fval); 
						node* tmp = add_child(currentscope, new_struct(yylineno, currentdecltype, $1, value, $2, NULL));
						for(int i = 0; i < tmp->data.dimlimit; i++)
							tmp->data.dimensions.fdim[i] = $4->value.fval;
						free($1); free($4);
					}
					else if(strstr(currentdecltype, "string"))
					{ 
						node * tmp = add_child(currentscope, new_struct(yylineno, currentdecltype, $1, $4->value.sval, $2, NULL)); 
						for(int i = 0; i < tmp->data.dimlimit; i++)
						{
							tmp->data.dimensions.sdim[i] = (char* )malloc(strlen($4->value.sval));
							strcpy(tmp->data.dimensions.sdim[i], $4->value.sval);
						}
						free($1); free($4);
					}
					else if(strstr(currentdecltype, "char"))
					{
						char value[32];
						snprintf(value, 32, "%d", $4->value.cval); 
						node* tmp = add_child(currentscope, new_struct(yylineno, currentdecltype, $1, value, $2, NULL));
						for(int i = 0; i < tmp->data.dimlimit; i++)
							tmp->data.dimensions.cdim[i] = $4->value.cval;
						free($1); free($4);
					}
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but assignment is of type \033[0;36m%s\033[0m\n", yylineno, currentdecltype, $4->type); return 0;}
			 }
		   | ID vec ATR '{' vecpar '}' 
			 {
				if(find_variable(currentscope, $1)) { printf("\033[1;91m[error: %d]\033[0;93m redeclaration of variable \033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $1, find_variable(currentscope, $1)); return 0;}
			 	add_child(currentscope, new_struct(yylineno, currentdecltype, $1, NULL, $2, NULL)); free($1);
			 }
	   	   ;

tip : INT {$$ = strdup("int");}
    | FLOAT {$$ = strdup("float");}
    | CHAR {$$ = strdup("char");}
    | STRING {$$ = strdup("string");}
    | BOOL {$$ = strdup("bool");}
    | CONST INT {$$ = strdup("const int");}
    | CONST FLOAT {$$ = strdup("const float");}
    | CONST CHAR {$$ = strdup("const char");}
    | CONST STRING {$$ = strdup("const string");}
    | CONST BOOL {$$ = strdup("const bool");}
    ;

assignments : ID ATR expr
			{
				node* var = get_variable(currentscope, $1);
				if(var)
				{
					if(!var->data.isconst)
					{
						if(strstr(var->data.type, $3->type))
						{
							if(strstr($3->type, "int") || strstr($3->type,"bool")){ var->data.value.ival = $3->value.ival; free($3); }
							else if(strstr($3->type, "float")){ var->data.value.fval = $3->value.fval; free($3);}
							else if(strstr($3->type, "string")){ free(var->data.value.sval); var->data.value.sval = malloc(sizeof($3->value.sval)); strcpy(var->data.value.sval, $3->value.sval); free($3); }
							else if(strstr($3->type, "char")){ var->data.value.cval = $3->value.cval; free($3); }
						}
						else { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but assignment is of type \033[0;36m%s\033[0m\n", yylineno, var->data.type, $3->type); return 0;}
					}
					else { printf("\033[1;91m[error: %d]\033[0;36m %s\033[0;93m is declared as a constant value\033[0m\n", yylineno, var->data.name); return 0;}
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
			}
			| ID vec ATR expr
			{
				node* var = get_variable(currentscope, $1);
				if(var)
				{
					if(!var->data.isconst)
					{
						if(strstr(var->data.type, $4->type))
						{
							if($2 < var->data.dimlimit)
							{
								if(strstr($4->type, "int") || strstr($4->type,"bool")){ var->data.dimensions.idim[$2] = $4->value.ival; free($4); }
								else if(strstr($4->type, "float")){ var->data.dimensions.fdim[$2] = $4->value.fval; free($4);}
								else if(strstr($4->type, "string")){ free(var->data.dimensions.sdim[$2]); var->data.dimensions.sdim[$2] = (char *)malloc(sizeof($4->value.sval)); strcpy(var->data.dimensions.sdim[$2], $4->value.sval); free($4); }
								else if(strstr($4->type, "char")){ var->data.dimensions.cdim[$2] = $4->value.cval; free($4); }
							}
							else { printf("\033[1;91m[error: %d]\033[0;93m Buffer overflow\033[0m\n", yylineno); return 0;}
						}
						else { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but assignment is of type \033[0;36m%s\033[0m\n", yylineno, var->data.type, $4->type); return 0;}
					}
					else { printf("\033[1;91m[error: %d]\033[0;36m %s\033[0;93m is declared as a constant value\033[0m\n", yylineno, var->data.name); return 0;}
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
			}
            | ID vec ATR '{' vecpar '}'
			{
				node* var = get_variable(currentscope, $1);
				if(var && var->data.isconst) { printf("\033[1;91m[error: %d]\033[0;36m %s\033[0;93m is declared as a constant value\033[0m\n", yylineno, var->data.name); return 0;}
				if(!var)  { printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
			}
            ;

vecpar : vecpar ',' expr
       | expr
       ;

vec : vec '[' NR ']' {$$ *= $3;}
    | '[' NR ']' {$$ = $2;}
    ;

lista_parametrii : tip ID { add_parameter(&parmlist, $1, $2); free($1); free($2);}
                 | tip ID vec { add_parameter(&parmlist, $1, $2); free($1); free($2);}
                 | tip ID ATR expr { add_parameter(&parmlist, $1, $2); free($1); free($2);}
                 | tip ID vec ATR expr { add_parameter(&parmlist, $1, $2); free($1); free($2);}
                 | tip ID vec ATR '{' vecpar '}' { add_parameter(&parmlist, $1, $2); free($1); free($2);}
                 | tip ID '(' lista_parametrii ')' { add_parameter(&parmlist, $1, $2); free($1); free($2);}
				 | lista_parametrii ',' tip ID { add_parameter(&parmlist, $3, $4); free($3); free($4);}
				 | lista_parametrii ',' tip ID vec { add_parameter(&parmlist, $3, $4); free($3); free($4);}
                 | lista_parametrii ',' tip ID ATR expr { add_parameter(&parmlist, $3, $4); free($3); free($4);}
                 | lista_parametrii ',' tip ID vec ATR expr { add_parameter(&parmlist, $3, $4); free($3); free($4);}
                 | lista_parametrii ',' tip ID vec ATR '{' vecpar '}' { add_parameter(&parmlist, $3, $4); free($3); free($4);}
                 | lista_parametrii ',' tip ID '(' lista_parametrii ')' { add_parameter(&parmlist, $3, $4); free($3); free($4);}
				 ;

functie : tip ID { parmlist = NULL; } '(' lista_parametrii ')' 
		{
			if(find_function(currentscope, $2, parmlist)) 
			{ 
				printf("\033[1;91m[error: %d]\033[0;93m redeclaration of function \033[0;36m%s\033[0;93m from line %d\033[0m\n", yylineno, $2, find_function(currentscope, $2, parmlist));
				return 0;
			}
			currentscope = add_child(currentscope, new_struct(yylineno, $1, $2, NULL, 0, parmlist)); free_list(parmlist); parmlist = NULL;
		} bloc {currentscope = currentscope->parent; }
		;

functioncall : ID { currentparmscope = add_child(currentparmscope, new_struct(yylineno, "functioncall", $1, NULL, 0, NULL)); } '(' call_list ')'
			 {
				node* function = get_function(currentscope, $1);
				if(!function)
				{
					printf("\033[1;91m[error: %d]\033[0;93m undeclared function \033[0;36m%s\033[0m\n", yylineno, $1);
					return 0;
				}
				else
				{
					parameter* tmp1 = function->data.parameters; node *tmp2 = currentparmscope->child;
					if(list_size(tmp1) != list_size2(tmp2))
					{
						printf("\033[1;91m[error: %d]\033[0;93m \033[0;36m%s\033[0;93m invalid number of parameters (need \033[0;36m%d \033[0;93mhave \033[0;36m%d\033[0;93m)\033[0m\n", yylineno, function->data.name, list_size(tmp1), list_size2(tmp2));
						return 0;
					}
					while(tmp1 && tmp2)
					{
						if(strcmp(tmp1->type, tmp2->data.type) != 0  && strcmp(tmp2->data.type, "functioncall") != 0)
						{
							printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but parameter is of type \033[0;36m%s\033[0m\n", yylineno, tmp1->type, tmp2->data.type);
							return 0;
						}
						tmp1 = tmp1->next;
						tmp2 = tmp2->next;
					}
				}
				currentparmscope = currentparmscope->parent;
				$$ = &function->data;
			 }
			 | ID '.' ID {currentparmscope = add_child(currentparmscope, new_struct(yylineno, "functioncall", $3, NULL, 0, NULL));} '(' call_list ')'
			 {
				node* var1 = get_variable(currentscope, $1);
				node* function = get_function_scope(var1, $3);	
				if(var1 && function)
				{
					parameter* tmp1 = function->data.parameters; node *tmp2 = currentparmscope->child;
					if(list_size(tmp1) != list_size2(tmp2))
					{
						printf("\033[1;91m[error: %d]\033[0;93m \033[0;36m%s\033[0;93m invalid number of parameters (need \033[0;36m%d \033[0;93mhave \033[0;36m%d\033[0;93m)\033[0m\n", yylineno, function->data.name, list_size(tmp1), list_size2(tmp2));
						return 0;
					}
					while(tmp1 && tmp2)
					{
						if(strcmp(tmp1->type, tmp2->data.type) != 0  && strcmp(tmp2->data.type, "functioncall") != 0)
						{
							printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but parameter is of type \033[0;36m%s\033[0m\n", yylineno, tmp1->type, tmp2->data.type);
							return 0;
						}
						tmp1 = tmp1->next;
						tmp2 = tmp2->next;
					}
				}
				else
				{
					if(!var1) printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $1);
					else if(!function) printf("\033[1;91m[error: %d]\033[0;93m undeclared function \033[0;36m%s\033[0m\n", yylineno, $1);
					return 0;
				}

				currentparmscope = currentparmscope->parent;
				$$ = &function->data;
			 }
             | function_eval { $$ = $1; }
             ;

function_eval : EVAL '(' expr ')' 
			  {
			  	if(strstr($3->type, "int") || strstr($3->type, "bool")) printf("Valoarea expresiei Eval %d\n", $3->value.ival);
				else if(strstr($3->type, "float")) { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but parameter is of type \033[0;36m%s\033[0m\n", yylineno, "int", $3->type); return 0;}
				else if(strstr($3->type, "string")) { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but parameter is of type \033[0;36m%s\033[0m\n", yylineno, "int", $3->type); return 0;}
				else if(strstr($3->type, "char")) { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but parameter is of type \033[0;36m%s\033[0m\n", yylineno, "int", $3->type); return 0;}
				struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
				tmp->type = malloc(5); strcpy(tmp->type, "void"); tmp->name = malloc(5); strcpy(tmp->name, "eval"); tmp->isconst = 0; tmp->line = yylineno; tmp->isfunction = 1;
				tmp->parameters = (parameter*)malloc(sizeof(parameter));
				tmp->parameters->next = NULL;
				tmp->parameters->type = malloc(4);
				strcpy(tmp->parameters->type, "int");
				tmp->parameters->name = malloc(5);
				strcpy(tmp->parameters->name, "expr");
				$$ = tmp;
			  }
              ;

call_list : call_list ',' expr { add_child(currentparmscope, new_struct(yylineno, $3->type, "expression", NULL, 0, NULL)); }
          | expr { add_child(currentparmscope, new_struct(yylineno, $1->type, "expression", NULL, 0, NULL));}
  		  ;

bloc : '{' program '}'
     | '{' '}'
	 ;

program :  declaratii ';'
     	| program declaratii ';'
        | assignments ';'
        | program assignments ';'
        | functie
        | program functie
        | functioncall ';'
        | program functioncall ';'
        | container
        | program container
        | statement
		| program statement
		| return
		| program return
     	;

containerblock : declaratii ';'
               | containerblock declaratii ';'
               | functie
               | containerblock functie
               | container
               | containerblock container
               ;

container : CONTAINER ID {currentscope = add_child(currentscope, new_struct(yylineno, "scope", $2, NULL, 0, NULL));} '{' containerblock '}' ';' { currentscope = currentscope->parent;}
	   	  ; 

comparatie : comparatie OR comparatie
           | comparatie AND comparatie 
		   | comparatie LEQ comparatie
           | comparatie GEQ comparatie
           | comparatie LW comparatie
           | comparatie GT comparatie
           | comparatie EQEQ comparatie
           | NOT comparatie
           | '(' comparatie ')'
           | comparatie PLS comparatie
           | comparatie MIN comparatie
           | comparatie MUL comparatie
           | comparatie DIV comparatie
           | comparatie MOD comparatie
           | ID
		   {
		   		node* var = get_variable(currentscope, $1);
				if(var)
				{
					if(!var->data.initialized) { printf("\033[1;91m[error: %d]\033[0;93m uninitialized variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
		   }
           | NR
           | STRINGVAL
           | CHARVAL
           | FLOATVAL
           | TRUE
           | FALSE
		   ;

expr : expr PLS expr
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($1->type));
		strcpy(tmp->type, $1->type); 
		if(strstr($1->type, "int") || strstr($1->type, "bool"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.ival = $1->value.ival + $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.ival = $1->value.ival + $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.ival = $1->value.ival + sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.ival = $1->value.ival + $3->value.cval;
		}
		else if(strstr($1->type, "float"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.fval = $1->value.fval + $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.fval = $1->value.fval + $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.fval = $1->value.fval + sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.fval = $1->value.fval + $3->value.cval;
		}
		else if(strstr($1->type, "string"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool"))
			{
				char buffer[10];
				snprintf(buffer, 10, "%d", $3->value.ival);
				tmp->value.sval = malloc(sizeof($1->value.sval)+sizeof(buffer)+1);
				strcpy(tmp->value.sval, $1->value.sval);
				strcat(tmp->value.sval, buffer);
			}
			else if(strstr($3->type, "float"))
			{
				char buffer[10];
				snprintf(buffer, 10, "%f", $3->value.fval);
				tmp->value.sval = malloc(sizeof($1->value.sval)+sizeof(buffer)+1);
				strcpy(tmp->value.sval, $1->value.sval);
				strcat(tmp->value.sval, buffer);
			}
			else if(strstr($3->type, "string"))
			{
				tmp->value.sval = malloc(sizeof($1->value.sval)+sizeof($3->value.sval)+1);
				strcpy(tmp->value.sval, $1->value.sval);
				strcat(tmp->value.sval, $3->value.sval);
			}
			else if(strstr($3->type, "char"))
			{
				tmp->value.sval = malloc(sizeof($1->value.sval)+2);
				strcpy(tmp->value.sval, $1->value.sval);
				tmp->value.sval[strlen(tmp->value.sval)] = $3->value.cval;
				tmp->value.sval[strlen(tmp->value.sval)+1] = '\0';
			}
		}
		else if(strstr($1->type, "char"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.cval = $1->value.cval + $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.cval = $1->value.cval + $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.cval = $1->value.cval + sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.cval = $1->value.cval + $3->value.cval;
		}
		$$ = tmp;
		free($1); free($3);
	 }
  	 | expr MIN expr
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($1->type));
		strcpy(tmp->type, $1->type); 
		if(strstr($1->type, "int") || strstr($1->type, "bool"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.ival = $1->value.ival - $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.ival = $1->value.ival - $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.ival = $1->value.ival - sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.ival = $1->value.ival - $3->value.cval;
		}
		else if(strstr($1->type, "float"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.fval = $1->value.fval - $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.fval = $1->value.fval - $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.fval = $1->value.fval - sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.fval = $1->value.fval - $3->value.cval;
		}
		else if(strstr($1->type, "string"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool"))
			{
				char buffer[32];
				snprintf(buffer, 32, "%d", $3->value.ival);
				tmp->value.sval = malloc(sizeof($1->value.sval));
				strcpy(tmp->value.sval, $1->value.sval);
				while( strrem(tmp->value.sval, buffer) );
			}
			else if(strstr($3->type, "float"))
			{
				char buffer[32];
				snprintf(buffer, 32, "%g", $3->value.fval);
				tmp->value.sval = malloc(sizeof($1->value.sval));
				strcpy(tmp->value.sval, $1->value.sval);
				while( strrem(tmp->value.sval, buffer) );
			}
			else if(strstr($3->type, "string"))
			{
				tmp->value.sval = malloc(sizeof($1->value.sval));
				strcpy(tmp->value.sval, $1->value.sval);
				while( strrem(tmp->value.sval, $3->value.sval) );
			}
			else if(strstr($3->type, "char"))
			{
				tmp->value.sval = malloc(sizeof($1->value.sval));
				strcpy(tmp->value.sval, $1->value.sval);
				char buffer[2];
				buffer[0] = $3->value.cval;
				buffer[1] = '\0';
				while( strrem(tmp->value.sval, buffer) );
			}
		}
		else if(strstr($1->type, "char"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.cval = $1->value.cval - $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.cval = $1->value.cval - $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.cval = $1->value.cval - sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.cval = $1->value.cval - $3->value.cval;
		}
		$$ = tmp;
		free($1); free($3);
	 }
  	 | expr MUL expr
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($1->type));
		strcpy(tmp->type, $1->type); 
		if(strstr($1->type, "int") || strstr($1->type, "bool"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.ival = $1->value.ival * $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.ival = $1->value.ival * $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.ival = $1->value.ival * sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.ival = $1->value.ival * $3->value.cval;
		}
		else if(strstr($1->type, "float"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.fval = $1->value.fval * $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.fval = $1->value.fval * $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.fval = $1->value.fval * sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.fval = $1->value.fval * $3->value.cval;
		}
		else if(strstr($1->type, "string"))
		{
			printf("\033[1;91merror: %d]\033[0;93m undefined behaviour multiplication of strings\033[0m\n", yylineno);
			return 0;
		}
		else if(strstr($1->type, "char"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.cval = $1->value.cval * $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.cval = $1->value.cval * $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.cval = $1->value.cval * sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.cval = $1->value.cval * $3->value.cval;
		}
		$$ = tmp;
		free($1); free($3);
	 }
  	 | expr DIV expr
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($1->type));
		strcpy(tmp->type, $1->type); 
		if(strstr($1->type, "int") || strstr($1->type, "bool"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.ival = $1->value.ival / $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.ival = $1->value.ival / $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.ival = $1->value.ival / sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.ival = $1->value.ival / $3->value.cval;
		}
		else if(strstr($1->type, "float"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.fval = $1->value.fval / $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.fval = $1->value.fval / $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.fval = $1->value.fval / sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.fval = $1->value.fval / $3->value.cval;
		}
		else if(strstr($1->type, "string"))
		{
			printf("\033[1;91merror: %d]\033[0;93m undefined behaviour division of strings\033[0m\n", yylineno);
			return 0;
		}
		else if(strstr($1->type, "char"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.cval = $1->value.cval / $3->value.ival;
			else if(strstr($3->type, "float")) tmp->value.cval = $1->value.cval / $3->value.fval;
			else if(strstr($3->type, "string")) tmp->value.cval = $1->value.cval / sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.cval = $1->value.cval / $3->value.cval;
		}
		$$ = tmp;
		free($1); free($3);
	 }
  	 | expr MOD expr
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($1->type));
		strcpy(tmp->type, $1->type); 
		if(strstr($1->type, "int") || strstr($1->type, "bool"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.ival = $1->value.ival % $3->value.ival;
			else if(strstr($3->type, "float")) {printf("\033[1;91merror: %d]\033[0;93m undefined behaviour modulus of floats\033[0m\n", yylineno); return 0; }
			else if(strstr($3->type, "string")) tmp->value.ival = $1->value.ival % sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.ival = $1->value.ival % $3->value.cval;
		}
		else if(strstr($1->type, "float"))
		{
			printf("\033[1;91merror: %d]\033[0;93m undefined behaviour modulus of floats\033[0m\n", yylineno);
			return 0;
		}
		else if(strstr($1->type, "string"))
		{
			printf("\033[1;91merror: %d]\033[0;93m undefined behaviour modulus of strings\033[0m\n", yylineno);
			return 0;
		}
		else if(strstr($1->type, "char"))
		{
			if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.cval = $1->value.cval % $3->value.ival;
			else if(strstr($3->type, "float")) {printf("\033[1;91merror: %d]\033[0;93m undefined behaviour modulus of floats\033[0m\n", yylineno); return 0; }
			else if(strstr($3->type, "string")) tmp->value.cval = $1->value.cval % sizeof($3->value.sval);
			else if(strstr($3->type, "char")) tmp->value.cval = $1->value.cval % $3->value.cval;
		}
		$$ = tmp;
		free($1); free($3);
	 }
     | NOT '(' expr ')'
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($3->type));
		strcpy(tmp->type, $3->type); 
		if(strstr($3->type, "int") || strstr($3->type, "bool")) tmp->value.ival = !$3->value.ival;
		else if(strstr($3->type, "float")) tmp->value.fval = !$3->value.fval;
		else if(strstr($3->type, "string"))
		{
			char value[strlen($3->value.sval)];
			strcpy(value, $3->value.sval);
			tmp->value.sval = malloc(sizeof(value));
			strcpy(tmp->value.sval, strrev(value));
		}
		else if(strstr($3->type, "char")) tmp->value.cval = !$3->value.cval;
		$$ = tmp;
		free($3);
	 }
  	 | '(' expr ')'
	 {
		$$ = $2;
	 }
  	 | ID
	 {
	 	node* var = get_variable(currentscope, $1);
		if(var)
		{
			if(var->data.initialized) 
			{
				struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
				tmp->type = malloc(sizeof(var->data.type));
				strcpy(tmp->type, var->data.type); 
				if(strstr(var->data.type, "int") || strstr(var->data.type, "bool")) tmp->value.ival = var->data.value.ival;
				else if(strstr(var->data.type, "float")) tmp->value.fval = var->data.value.fval;
				else if(strstr(var->data.type, "string"))
				{
					tmp->value.sval = malloc(sizeof(var->data.value.sval));
					strcpy(tmp->value.sval, var->data.value.sval);
				}
				else if(strstr(var->data.type, "char")) tmp->value.cval = var->data.value.cval;
				$$ = tmp;
			}
			else { printf("\033[1;91m[error: %d]\033[0;93m uninitialized variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
		}
		else { printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
	 }
	 | ID '.' ID
	 {
		node* var1 = get_variable(currentscope, $1);
		node* var2 = get_variable_scope(var1, $3);
		if(var1 && var2)
		{
			if(var2->data.initialized)
			{
				struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
				tmp->type = malloc(sizeof(var2->data.type));
				strcpy(tmp->type, var2->data.type); 
				if(strstr(var2->data.type, "int") || strstr(var2->data.type, "bool")) tmp->value.ival = var2->data.value.ival;
				else if(strstr(var2->data.type, "float")) tmp->value.fval = var2->data.value.fval;
				else if(strstr(var2->data.type, "string"))
				{
					tmp->value.sval = malloc(sizeof(var2->data.value.sval));
					strcpy(tmp->value.sval, var2->data.value.sval);
				}
				else if(strstr(var2->data.type, "char")) tmp->value.cval = var2->data.value.cval;
				$$ = tmp;
			}
			else { printf("\033[1;91m[error: %d]\033[0;93m uninitialized variable \033[0;36m%s\033[0m\n", yylineno, $3); return 0;}
		}
		else 
		{
			if(!var1) printf("\033[1;91m[error: %d]\033[0;93m undeclared container \033[0;36m%s\033[0m\n", yylineno, $1);
			else if(!var2) printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $3);
			return 0;
		}
	 }
	 | functioncall
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($1->type));
		strcpy(tmp->type, $1->type); 
		if(strstr($1->type, "int") || strstr($1->type, "bool")) tmp->value.ival = $1->value.ival;
		else if(strstr($1->type, "float")) tmp->value.fval = $1->value.fval;
		else if(strstr($1->type, "string"))
		{
			tmp->value.sval = malloc(sizeof($1->value.sval));
			strcpy(tmp->value.sval, $1->value.sval);
		}
		else if(strstr($1->type, "char")) tmp->value.cval = $1->value.cval;
		$$ = tmp;
	 }
	 | ID vec
	 {
		node* var = get_variable(currentscope, $1);
		if(var)
		{
			if(var->data.initialized) 
			{
				if($2 < var->data.dimlimit)
				{
					struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
					tmp->type = malloc(sizeof(var->data.type));
					strcpy(tmp->type, var->data.type); 
					if(strstr(var->data.type, "int") || strstr(var->data.type, "bool")) tmp->value.ival = var->data.dimensions.idim[$2];
					else if(strstr(var->data.type, "float")) tmp->value.fval = var->data.dimensions.fdim[$2];
					else if(strstr(var->data.type, "string"))
					{
						tmp->value.sval = malloc(sizeof(var->data.dimensions.sdim[$2]));
						strcpy(tmp->value.sval, var->data.dimensions.sdim[$2]);
					}
					else if(strstr(var->data.type, "char")) tmp->value.cval = var->data.dimensions.cdim[$2];
					$$ = tmp;
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m Buffer overflow\033[0m\n", yylineno); return 0;}
			}
			else { printf("\033[1;91m[error: %d]\033[0;93m uninitialized variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
		}
		else { printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $1); return 0;}
	 }
	 | ID '.' ID vec
	 {
		node* var1 = get_variable(currentscope, $1);
		node* var2 = get_variable_scope(var1, $3);
		if(var1 && var2)
		{
			if(var2->data.initialized)
			{
				if($4 < var2->data.dimlimit)
				{
					struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
					tmp->type = malloc(sizeof(var2->data.type));
					strcpy(tmp->type, var2->data.type); 
					if(strstr(var2->data.type, "int") || strstr(var2->data.type, "bool")) tmp->value.ival = var2->data.dimensions.idim[$4];
					else if(strstr(var2->data.type, "float")) tmp->value.fval = var2->data.dimensions.fdim[$4];
					else if(strstr(var2->data.type, "string"))
					{
						tmp->value.sval = malloc(sizeof(var2->data.dimensions.sdim[$4]));
						strcpy(tmp->value.sval, var2->data.dimensions.sdim[$4]);
					}
					else if(strstr(var2->data.type, "char")) tmp->value.cval = var2->data.dimensions.cdim[$4];
					$$ = tmp;
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m Buffer overflow\033[0m\n", yylineno); return 0;}
			}
			else { printf("\033[1;91m[error: %d]\033[0;93m uninitialized variable \033[0;36m%s\033[0m\n", yylineno, $3); return 0;}
		}
		else 
		{
			if(!var1) printf("\033[1;91m[error: %d]\033[0;93m undeclared container \033[0;36m%s\033[0m\n", yylineno, $1);
			else if(!var2) printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $3);
			return 0;
		}
	 }
  	 | NR
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(4);
		strcpy(tmp->type, "int"); 
		tmp->value.ival = $1;
		$$ = tmp;
	 }
     | CHARVAL
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(5);
		strcpy(tmp->type, "char"); 
		tmp->value.cval = $1;
		$$ = tmp;
	 }
	 | FLOATVAL
     {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(6);
		strcpy(tmp->type, "float"); 
		tmp->value.fval = $1;
		$$ = tmp;
	 }
	 | STRINGVAL
	 {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(7);
		strcpy(tmp->type, "string");
		tmp->value.sval = malloc(sizeof($1)); 
		strcpy(tmp->value.sval, $1);
		$$ = tmp;
	 }
     | TRUE
     {
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(5);
		strcpy(tmp->type, "bool"); 
		tmp->value.ival = 1;
		$$ = tmp;
     }
     | FALSE
     {
	 	struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(5);
		strcpy(tmp->type, "bool"); 
		tmp->value.ival = 0;
		$$ = tmp;
     }
	 | not {$$ = $1;}
  	 ;

not : NOT not
	{
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(sizeof($2->type));
		strcpy(tmp->type, $2->type); 
		if(strstr($2->type, "int") || strstr($2->type, "bool")) tmp->value.ival = !$2->value.ival;
		else if(strstr($2->type, "float")) tmp->value.fval = !$2->value.fval;
		else if(strstr($2->type, "string"))
		{
			char value[strlen($2->value.sval)];
			strcpy(value, $2->value.sval);
			tmp->value.sval = malloc(sizeof(value));
			strcpy(tmp->value.sval, strrev(value));
		}
		else if(strstr($2->type, "char")) tmp->value.cval = !$2->value.cval;
		$$ = tmp;
		free($2);
	}
    | NOT NR
	{
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(4);
		strcpy(tmp->type, "int"); 
		tmp->value.ival = !$2;
		$$ = tmp;
	}
	| NOT ID
    {
 		node* var = get_variable(currentscope, $2);
		if(var)
		{
			if(var->data.initialized)
			{
				struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
				tmp->type = malloc(sizeof(var->data.type));
				strcpy(tmp->type, var->data.type); 
				if(strstr(var->data.type, "int") || strstr(var->data.type, "bool")) tmp->value.ival = !var->data.value.ival;
				else if(strstr(var->data.type, "float")) tmp->value.fval = !var->data.value.fval;
				else if(strstr(var->data.type, "string"))
				{
					printf("\033[1;91merror: %d]\033[0;93m undefined behaviour negation of strings\033[0m\n", yylineno);
					return 0;
				}
				else if(strstr(var->data.type, "char")) tmp->value.cval = !var->data.value.cval;
				$$ = tmp;
			}
			else { printf("\033[1;91m[error: %d]\033[0;93m uninitialized variable \033[0;36m%s\033[0m\n", yylineno, $2); return 0;}
		}
		else { printf("\033[1;91m[error: %d]\033[0;93m undeclared variable \033[0;36m%s\033[0m\n", yylineno, $2); return 0;}
    }
    | NOT STRINGVAL
	{
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(7);
		strcpy(tmp->type, "string");
		char value[strlen($2)];
		strcpy(value, $2);
		tmp->value.sval = malloc(sizeof(value));
		strcpy(tmp->value.sval, strrev(value));
		$$ = tmp;
	}
    | NOT CHARVAL
	{
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(5);
		strcpy(tmp->type, "char"); 
		tmp->value.cval = !$2;
		$$ = tmp;
	}
	| NOT FLOATVAL
	{
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(6);
		strcpy(tmp->type, "float"); 
		tmp->value.fval = !$2;
		$$ = tmp;
	}
    | NOT TRUE
	{
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(5);
		strcpy(tmp->type, "bool"); 
		tmp->value.ival = 0;
		$$ = tmp;
	}
    | NOT FALSE
	{
		struct data* tmp = (struct data*)(malloc(sizeof(struct data)));
		tmp->type = malloc(5);
		strcpy(tmp->type, "bool"); 
		tmp->value.ival = 1;
		$$ = tmp;
	}
	;

statement : FOR {currentscope = add_child(currentscope, new_struct(yylineno, "scope", "for", NULL, 0, NULL));} fortypes { currentscope = currentscope->parent;}
          | if
          | WHILE '(' comparatie ')' {currentscope = add_child(currentscope, new_struct(yylineno, "scope", "while", NULL, 0, NULL));} bloc { currentscope = currentscope->parent;}
          ;

if : {currentscope = add_child(currentscope, new_struct(yylineno, "scope", "if", NULL, 0, NULL));} iftypes { currentscope = currentscope->parent;}
   ;

iftypes : IF '(' comparatie ')' bloc
        | IF '(' comparatie ')' bloc {currentscope = currentscope->parent; currentscope = add_child(currentscope, new_struct(yylineno, "scope", "else", NULL, 0, NULL)); } elsetypes
		;

elsetypes : ELSE bloc
		  | ELSE if
		  ;

fortypes : '(' declaratii ';' comparatie ';' assignments ')' bloc
	     | '(' assignments ';' comparatie ';' assignments ')' bloc
		 | '(' ';' ';' ';' ')' bloc
		 ;

return : RETURN expr ';' 
	   { 
			node* function = get_parent_function(currentscope);
			if(function)
			{
				if(strstr(function->data.type, $2->type))
				{
					if(strstr($2->type, "int") || strstr($2->type,"bool")){ function->data.value.ival = $2->value.ival; function->data.address.iad = &function->data.value.ival; function->data.initialized = 1; free($2); }
					else if(strstr($2->type, "float")){ function->data.value.fval = $2->value.fval; function->data.address.fad = &function->data.value.fval; function->data.initialized = 1; free($2);}
					else if(strstr($2->type, "string")){ free(function->data.value.sval); function->data.value.sval = malloc(sizeof($2->value.sval)); strcpy(function->data.value.sval, $2->value.sval); function->data.address.sad = &function->data.value.sval; function->data.initialized = 1; free($2); }
					else if(strstr($2->type, "char")){ function->data.value.cval = $2->value.cval; function->data.address.cad = &function->data.value.cval; function->data.initialized = 1; free($2); }
				}
				else { printf("\033[1;91m[error: %d]\033[0;93m expected \033[0;36m%s\033[0;93m but return is of type \033[0;36m%s\033[0m\n", yylineno, function->data.type, $2->type); return 0;}
			}
	   }
	   ;

%%
int yyerror(char * s){
printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
yyin=fopen(argv[1],"r");
yyparse();
} 
