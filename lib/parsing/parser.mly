%{
  open Syntax
  open Typed_ast
  open Prim

  type bindings =
  | Binding of binding
  | RecBindings of binding list

  let expr_of_let_bindings (b : bindings) (e2 : e) =
  match b with
  | Binding b -> Let (b, e2)
  | RecBindings l -> Letrec (l, e2)
%}

%token ARROW
%token TBOOL TINT TARG
%token <string> ID
%token <int> INT
%token TRUE FALSE
%token NOT
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token AMPERSANDAMPERSAND BARBAR
%token DAT
%token JMP
%token SPL
%token NOP
%token MOV
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token JMZ
%token JMN
%token DJN
%token SEQ
%token SNE
%token SLT
%token STP
%token LDP
%token AND
%token IF THEN ELSE
%token FUN
%token LPAREN RPAREN
%token LET REC IN
%token COMMA
%token DOT
%token COLON
%token COLONCOLON
%token EOF

%nonassoc IN
%nonassoc ELSE

%nonassoc below_COMMA
%left COMMA
%right BARBAR
%right AMPERSANDAMPERSAND
%left EQUAL LESS
%left ARROW
%left PLUS MINUS
%left TIMES

%nonassoc NOT

%start file
%type <e option> file
%type <e> expr
%type <e> simple_expr
%type <e> app_expr

%%

file:
  | EOF
    { None }
  | e = top EOF
    { Some e }

ty:
  | t = simple_ty { t }
  | t1 = ty ARROW t2 = simple_ty { Type.Arrow (t1, t2) }
  | types = ty_product_list { Type.Product (types) }

simple_ty:
  | TARG { Type.Arg }
  | TINT { Type.Int }
  | TBOOL { Type.Bool }
  | LPAREN t = ty RPAREN { t }

%inline ty_product_list:
  | types = separated_nontrivial_llist (TIMES, simple_ty) { types }

top:
  | e = expr
    { e }
  | e = top COLONCOLON t = ty
    { Ascribe (e, t)}

expr:
  | e = app_expr
    { e }
  | op = op1 e = expr
    { Prim1 (op, e) }
  | e1 = expr op = op2 e2 = expr	
    { Prim2 (op, e1, e2) }
  | op = op2 e1 = expr e2 = expr	
    { Prim2 (op, e1, e2) }
  | op = op3 e1 = expr e2 = expr e3 = expr
    { Prim3 (op, e1, e2, e3) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Prim3 (If, e1, e2, e3) }
  | FUN x = ID ARROW e = expr
    { Lam (x, Type.Any, e) }
  | FUN x = ID COLON t = ty ARROW e = expr
    { Lam (x, t, e) }
  | lbs = let_bindings e = expr { expr_of_let_bindings lbs e }
  | exprs = expr_comma_list %prec below_COMMA { Tuple (exprs) }

%inline op1:
  | MINUS {Neg}
  | NOT {Not}

%inline op2:
  | PLUS {Plus}
  | MINUS {Minus}
  | TIMES {Times}
  | EQUAL {Eq}
  | LESS {Less}
  | AMPERSANDAMPERSAND {And}
  | BARBAR {Or}

%inline op2:
  | DAT {Dat}
  | JMP {Jmp}
  | SPL {Spl}
  | NOP {Nop}
  | MOV {Mov}
  | ADD {Add}
  | SUB {Sub}
  | MUL {Mul}
  | DIV {Div}
  | MOD {Mod}
  | JMZ {Jmz}
  | JMN {Jmn}
  | DJN {Djn}
  | SEQ {Seq}
  | SNE {Sne}
  | SLT {Slt}
  | STP {Stp}
  | LDP {Ldp}

app_expr:
  | e = simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { App (e1, e2) }

simple_expr:
  | x = ID
    { Value (Var x) }
  | TRUE    
    { Value (Bool true) }
  | FALSE
    { Value (Bool false) }
  | n = INT
    { Value (Int n) }
  | e = simple_expr DOT LPAREN n = INT RPAREN
    { Get (e, n) }
  | LPAREN e = expr RPAREN	
    { e }

%inline let_bindings:
  | LET b = binding IN { Binding b }
  | LET REC bs = and_bindings IN { RecBindings bs }

%inline binding:
  | id = ID EQUAL e = expr { { name = id; e; ty = Type.Any } }
  | id = ID COLON t = ty EQUAL e = expr { { name = id; e; ty = t } }

and_bindings:
  | b = binding { [b] }
  | b = binding AND bs = and_bindings { b :: bs }

(* copied from the OCaml parser *)
%inline expr_comma_list:
  es = separated_nontrivial_llist(COMMA, expr) { es }

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }
