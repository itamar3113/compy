%{

#include "nodes.hpp"
#include "output.hpp"

// bison declarations
extern int yylineno;
extern int yylex();

void yyerror(const char*);

// root of the AST, set by the parser and used by other parts of the compiler
std::shared_ptr<ast::Node> program;

using namespace std;

// TODO: Place any additional declarations here
%}

//Tokens
%token  ID
%token  NUM
%token  NUMB
%token  STRING
%token  TRUE FALSE
%token VOID INT BYTE BOOL
%token RETURN IF ELSE WHILE BREAK CONTINUE
%token SC COMMA LBRACE RBRACE LPAREN RPAREN
%token B
%token PLUS MINUS TIMES DIVIDE
%token AND OR
%token NOT
%token EQ_OP NE_OP LT_OP GT_OP LE_OP GE_OP
%token ASSIGN


// Precedence
%right ASSIGN
%left OR
%left AND
%left EQ_OP NE_OP 
%left LT_OP GT_OP LE_OP GE_OP
%left PLUS MINUS 
%left TIMES DIVIDE
%right NOT
%right ELSE

%%
// While reducing the start variable, set the root of the AST
Program:  Funcs { program = $1; }
;

Funcs:  { $$ = std::make_shared<ast::Funcs>(); } |
	FuncDecl Funcs 
	{ auto funcs = dynamic_pointer_cast<ast::Funcs>($2); funcs->push_front(dynamic_pointer_cast<ast::FuncDecl>($1)); $$ = funcs; };

FuncDecl: RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE
	 { $$ = make_shared<ast::FuncDecl>(dynamic_pointer_cast<ast::ID>($2),
        dynamic_pointer_cast<ast::Type>($1),
        dynamic_pointer_cast<ast::Formals>($4),
        dynamic_pointer_cast<ast::Statements>($7)); };

RetType: Type {$$ = std::dynamic_pointer_cast<ast::Type>($1);}
    |    VOID {$$=std::make_shared<ast::Type>(ast::BuiltInType::VOID);};

Formals: { $$ = std::make_shared<ast::Formals>(); } | 
	 FormalsList 
	 { $$ = dynamic_pointer_cast<ast::Formals>($1); };

FormalsList: FormalDecl
	     { $$ = make_shared<ast::Formals>(dynamic_pointer_cast<ast::Formal>($1)); } |
	     FormalDecl COMMA FormalsList
	     { auto formals = dynamic_pointer_cast<ast::Formals>($3);
            formals->push_front(dynamic_pointer_cast<ast::Formal>($1));
             $$ = formals; };

FormalDecl: Type ID
	    { $$ = make_shared<ast::Formal>(dynamic_pointer_cast<ast::ID>($2), dynamic_pointer_cast<ast::Type>($1)); };

Statements: Statement
	    { $$ = make_shared<ast::Statements>(dynamic_pointer_cast<ast::Statement>($1)); } |
	    Statements Statement
	    { auto sts = dynamic_pointer_cast<ast::Statements>($1);
           sts->push_back(dynamic_pointer_cast<ast::Statement>($2));
           $$ = sts; };
	
Statement: LBRACE Statements RBRACE
	   { $$ = $2; } |
	   Type ID SC
	   { $$ = make_shared<ast::VarDecl>(dynamic_pointer_cast<ast::ID>($2),
          dynamic_pointer_cast<ast::Type>($1)); } |
	   Type ID ASSIGN Exp SC
	   { $$ = make_shared<ast::VarDecl>(dynamic_pointer_cast<ast::ID>($2),
          dynamic_pointer_cast<ast::Type>($1),
           dynamic_pointer_cast<ast::Exp>($4)); } |
	   ID ASSIGN Exp SC
	   { $$ = make_shared<ast::Assign>(dynamic_pointer_cast<ast::ID>($1),
         dynamic_pointer_cast<ast::Exp>($3)); } |
	   Call SC
	   { $$ = $1; } | 
	   RETURN SC
	   { $$ = make_shared<ast::Return>(); } |
	   RETURN Exp SC                 
	   { $$ = std::make_shared<ast::Return>(dynamic_pointer_cast<ast::Exp>($2)); } |
	   IF LPAREN Exp RPAREN Statement
	   { $$ = std::make_shared<ast::If>(dynamic_pointer_cast<ast::Exp>($3),
         dynamic_pointer_cast<ast::Statement>($5)); } |
	   IF LPAREN Exp RPAREN Statement ELSE Statement  
	   { $$ = std::make_shared<ast::If>(dynamic_pointer_cast<ast::Exp>($3),
         dynamic_pointer_cast<ast::Statement>($5), dynamic_pointer_cast<ast::Statement>($7)); } |
	   WHILE LPAREN Exp RPAREN Statement 
	   { $$ = std::make_shared<ast::While>(dynamic_pointer_cast<ast::Exp>($3),
         dynamic_pointer_cast<ast::Statement>($5)); } |
	   BREAK SC
	   {$$ = std::make_shared<ast::Break>(); } |
	   CONTINUE SC
	   { $$ = std::make_shared<ast::Continue>(); };
	
Call: ID LPAREN ExpList RPAREN
      { $$ = make_shared<ast::Call>(dynamic_pointer_cast<ast::ID>($1),
      dynamic_pointer_cast<ast::ExpList>($3)); } |
      ID LPAREN RPAREN
      { $$ = make_shared<ast::Call>(dynamic_pointer_cast<ast::ID>($1)); };

ExpList: Exp
	 { $$ = make_shared<ast::ExpList>(dynamic_pointer_cast<ast::Exp>($1)); } |
	 Exp COMMA ExpList
	 { auto exps = dynamic_pointer_cast<ast::ExpList>($3);
        exps->push_front(dynamic_pointer_cast<ast::Exp>($1));
        $$ = exps; };

Type: INT { $$ = make_shared<ast::Type>(ast::BuiltInType::INT); } |
      BYTE { $$ = make_shared<ast::Type>(ast::BuiltInType::BYTE); } |
      BOOL { $$ = make_shared<ast::Type>(ast::BuiltInType::BOOL); };

Exp: LPAREN Exp RPAREN
     { $$ = $2; } |
     Exp PLUS Exp
     { $$ = make_shared<ast::BinOp>(dynamic_pointer_cast<ast::Exp>($1),
      dynamic_pointer_cast<ast::Exp>($3), ast::BinOpType::ADD); } |
     Exp MINUS Exp
     { $$ = make_shared<ast::BinOp>(dynamic_pointer_cast<ast::Exp>($1),
      dynamic_pointer_cast<ast::Exp>($3), ast::BinOpType::SUB); } |
     Exp TIMES Exp
     { $$ = make_shared<ast::BinOp>(dynamic_pointer_cast<ast::Exp>($1),
      dynamic_pointer_cast<ast::Exp>($3), ast::BinOpType::MUL); } |
     Exp DIVIDE Exp
     { $$ = make_shared<ast::BinOp>(dynamic_pointer_cast<ast::Exp>($1),
      dynamic_pointer_cast<ast::Exp>($3), ast::BinOpType::DIV); } |
     ID { $$ = dynamic_pointer_cast<ast::ID>($1); } |
     Call { $$ = dynamic_pointer_cast<ast::Call>($1); } |
     NUM { $$ = dynamic_pointer_cast<ast::Num>($1); } |
     NUMB { $$ = dynamic_pointer_cast<ast::NumB>($1); } |
     STRING { $$ = dynamic_pointer_cast<ast::String>($1); } |
     TRUE { $$ = make_shared<ast::Bool>(true); } |
     FALSE { $$ = make_shared<ast::Bool>(false); } |
     NOT Exp { $$ = make_shared<ast::Not>(dynamic_pointer_cast<ast::Exp>($2)); } |
     Exp AND Exp 
     { $$ = make_shared<ast::And>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3)); } |
     Exp OR Exp 
     { $$ = make_shared<ast::Or>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3)); } |	
     Exp EQ_OP Exp  
     { $$ = std::make_shared<ast::RelOp>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3), ast::RelOpType::EQ); } |
     Exp NE_OP Exp  
     { $$ = std::make_shared<ast::RelOp>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3), ast::RelOpType::NE); } |
     Exp LT_OP Exp  
     { $$ = std::make_shared<ast::RelOp>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3), ast::RelOpType::LT); } |
     Exp GT_OP Exp  
     { $$ = std::make_shared<ast::RelOp>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3), ast::RelOpType::GT); } |
     Exp LE_OP Exp  
     { $$ = std::make_shared<ast::RelOp>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3), ast::RelOpType::LE); } |
     Exp GE_OP Exp  
     { $$ = std::make_shared<ast::RelOp>(dynamic_pointer_cast<ast::Exp>($1), dynamic_pointer_cast<ast::Exp>($3), ast::RelOpType::GE); } |
     LPAREN Type RPAREN Exp
     { $$ = make_shared<ast::Cast>(dynamic_pointer_cast<ast::Exp>($4), dynamic_pointer_cast<ast::Type>($2)); };







%%

// TODO: Place any additional code here
void yyerror(const char*){
      output::errorSyn(yylineno);
      exit(1);
}