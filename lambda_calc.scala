sealed trait Expr
case class Lambda(arg: Var, body: Expr) extends Expr
case class Var(name: String) extends Expr
case class Apply(fun: Expr, arg: Expr) extends Expr

def free_vars(e: Expr): Set[Var] = 
  e match
  {
    case Var(v) => Set(Var(v))
    case Apply(e0, e1) => free_vars(e0) ++ free_vars(e1)
    case Lambda(v,e) => free_vars(e) - v
  }

//Used for generating an alpha equivalent lambda term with variable p
def do_lambda_sub(p: Var, l: Lambda): Lambda = 
   Lambda(p, do_sub(l.arg,l.body,p))

//Sub e1 for p in expression e0
def do_sub(p: Var, e0: Expr, e1: Expr): Expr =
  e0 match
  {
    case Var(v) => if (p == e0) e1 else e0
    case Apply(e00,e11) => Apply(do_sub(p,e00,e1), do_sub(p,e11,e1))
    case Lambda(v,e) => if (v == p) 
							e
						else if (free_vars(e1) contains p) {
							val new_var = Var(v.name + "'");
							do_sub(p, Lambda(new_var, do_sub(v,e,new_var)), e1) }
						else 
							Lambda(v,do_sub(p,e,e1))
  }

def eval_apply(e0: Expr, e1:Expr): Expr =
  e0 match
  {
    case Lambda(v, e) => do_sub(v, e, e1)
    case _ => Apply(e0, e1)
  }

def eval(e: Expr): Expr =
  e match
  {
    case Var(_) => e
    case Apply(e0, e1) => eval_apply(eval(e0),eval(e1))
    case Lambda(arg,e) => Lambda(arg,eval(e))
  }

println(eval(Apply(Lambda(Var("z"),Var("x")),Var("y"))));
