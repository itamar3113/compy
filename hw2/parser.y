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
%token VOID INT BYTE BOOL
%token TRUE FALSE
%token RETURN IF ELSE WHILE BREAK CONTINUE
%token SC COMMA LBRACE RBRACE LPAREN RPAREN
%token ID NUM NUMB STRING
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

//Rule types
%type <ast::Node> Program
%type <ast::Funcs> Funcs
%type <ast::FuncDecl> FuncDecl
%type <ast::Type> RetType Type
%type <ast::Formals> Formals FormalsList
%type <ast::Formal> FormalDecl
%type <ast::Statements> Statements
%type <ast::Statement> Statement
%type <ast::Call> Call
%type <ast::ExpList> ExpList
%type <ast::Exp> Exp
%%

// While reducing the start variable, set the root of the AST
Program:  Funcs { program = $1; }
;

Funcs:  { $$ = std::make_shared<ast::Funcs>(); } |
	FuncDecl Funcs 
	{ auto funcs = $2; funcs->push_front($1); $$ = funcs; };

FuncDec: RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE
	 { $$ = make_shared<ast::FuncDecl>($2, $1, $4, $7); };

RetType: {$$ = $1} |
	 {$$ = make_shared<ast::Type(ast::BuiltInType::VOID);};

Formals: { $$ = std::make_shared<ast::Formals>(); } | 
	 FormalsList 
	 { $$ = $1; };

FormalsList: FormalDecl
	     { $$ = make_shared<ast::Formals>($1); } |
	     FormalDecl COMMA FormalsList
	     { auto formals = $3; formals->push_front($1); $$ = formals };

FormalDecl: Type ID
	    { $$ = make_shared<ast::Formal>($1, $2); };

Statements: Statement
	    { $$ = make_shared<ast::Statements>($1); } |
	    Statements Statement
	    { auto = sts = $1; sts->push_back($2); $$ = sts; };
	
Statement: LBRACE Statement RBRACE
	   { $$ = $2; }
	   Type ID SC
	   { $$ = make_shared<ast::varDecl>($2, $1); } |
	   Type ID ASSIGN Exp SC
	   { $$ = make_shared<ast::varDecl>($2, $1, $4); } |
	   ID ASSIGN Exp SC
	   { $$ = make_shared<ast::varDecl>($1, $3); } |
	   Call SC
	   { $$ = $1; } | 
	   RETURN SC
	   { $$ = make_shared<ast::Return>(); } |
	   RETURN Exp SC                 
	   { $$ = std::make_shared<ast::Return>($2); } |
	   IF LPAREN Exp RPAREN Statement
	   { $$ = std::make_shared<ast::If>($3, $5); } |
	   IF LPAREN Exp RPAREN Statement ELSE Statement  
	   { $$ = std::make_shared<ast::If>($3, $5, $7); } |
	   WHILE LPAREN Exp RPAREN Statement 
	   { $$ = std::make_shared<ast::While>($3, $5); } |
	   BREAK SC
	   {$$ = std::make_shared<ast::Break>(); } |
	   CONTINUE SC
	   { $$ = std::make_shared<ast::Continue>(); };
	
Call: ID LPAREN ExpList RPAREN
      { $$ = make_shared<ast::Call>($1, $3); } |
      ID LPAREN ExpList RPAREN
      { $$ = make_shared::Call>($1); };

ExpList: Exp
	 { $$ = make_shared<ast::ExpList>($1); } |
	 Exp COMMA ExpList
	 { auto exps = $3; exps->push_front($2); $$ = exps; };

Type: INT { $$ = make_shared<ast::Type>(ast::BuiltInType::INT); } |
      BYTE { $$ = make_shared<ast::Type>(ast::BuiltInType::BYTE); } |
      BOOL { $$ = make_shared<ast::Type>(ast::BuiltInType::BOOL); };

Exp: LPAREN Exp RPAREN
     { $$ = $2; } |
     Exp PLUS Exp
     { $$ = make_shared<ast::BinOp>($1, $3, ast::BinOpType::ADD); } |
     Exp MINUS Exp
     { $$ = make_shared<ast::BinOp>($1, $3, ast::BinOpType::SUB); } |
     Exp TIMES Exp
     { $$ = make_shared<ast::BinOp>($1, $3, ast::BinOpType::MUL); } |
     Exp DIVIDE Exp
     { $$ = make_shared<ast::BinOp>($1, $3, ast::BinOpType::DIV); } |
     ID { $$ = $1; } |
     Call { $$ = $1; } |
     NUM { $$ = $1; } |
     NUMB { $$ = $1; } |
     STRING { $$ = $1; } |
     TRUE { $$ = make_shared<ast::BOOL>(true); } |
     FALSE { $$ = make_shared<ast::BOOL>(false); } |
     NOT Exp { $$ = make_shared<ast::Not>($2); } |
     Exp AND Exp 
     { $$ = make_shared<ast::And>($1, $3); } |
     Exp OR Exp 
     { $$ = make_shared<ast::Or>($1, $3); } |	
     Exp EQ_OP Exp  
     { $$ = std::make_shared<ast::RelOp>($1, $3, ast::RelOpType::EQ); } |
     Exp NE_OP Exp  
     { $$ = std::make_shared<ast::RelOp>($1, $3, ast::RelOpType::NE); } |
     Exp LT_OP Exp  
     { $$ = std::make_shared<ast::RelOp>($1, $3, ast::RelOpType::LT); } |
     Exp GT_OP Exp  
     { $$ = std::make_shared<ast::RelOp>($1, $3, ast::RelOpType::GT); } |
     Exp LE_OP Exp  
     { $$ = std::make_shared<ast::RelOp>($1, $3, ast::RelOpType::LE); } |
     Exp GE_OP Exp  
     { $$ = std::make_shared<ast::RelOp>($1, $3, ast::RelOpType::GE); } |
     LPAREN Type RPAREN Exp
     { $$ = make_shared<ast::Cast>($4, $2); };







%%

// TODO: Place any additional code here