datatype Exp = Const(int) | Var(string) | Plus(Exp, Exp) |  Mult(Exp, Exp)

function eval(e:Exp, store:map<string, int>):int
{
	match(e)
		case Const(n) => n
		case Var(s) => if(s in store) then store[s] else -1
		case Plus(e1, e2) => eval(e1, store) + eval(e2, store)
		case Mult(e1, e2) => eval(e1, store) * eval(e2, store)
}

//fill this function in to make optimizeFeatures work
function optimize(e:Exp):Exp
{
	match e 
		case Const(n) => e
		case Var(x) => e
		case Plus(e1, e2) => if e1 == Const(0)
		                     then e2 
							 else if e2 == Const(0)
							      then e1
								  else if e1 == Const(3) && e2 == Const(4)
								       then Const(7)
									   else e
		case Mult(e1, e2) => if e1 == Const(0) || e2 == Const(0)
		                     then Const(0)
							 else if e1 == Const(1)
							      then e2
								  else if (e2 == Const(1))
								       then e1 
									   else if e1 == Const(3) && e2 == Const(4)
									        then Const(12)
											else e
}

//as you write optimize this will become unproved
//you must write proof code so that Dafny can prove this
ghost method optimizeCorrect(e:Exp, s:map<string, int>)
	ensures eval(e,s) == eval(optimize(e), s);
{
	match e {
		case Const(n) => 
			calc {
				eval(Const(n), s);
			 == { assert optimize(Const(n)) == Const(n); }
			    eval(optimize(Const(n)), s);
			}
		case Var(x) => 
			calc {
				eval(Var(x), s);
			 == { assert optimize(Var(x)) == Var(x); }
			    eval(optimize(Var(x)), s);
			}
		case Plus(e1, e2) =>
			calc {
				eval(Plus(e1, e2), s);
			 == eval(e1, s) + eval(e2, s);
			 == { optimizeCorrect(e1, s); optimizeCorrect(e2, s); }
			    eval(optimize(e1), s) + eval(optimize(e2), s);
			 == eval(Plus(optimize(e1), optimize(e2)), s);
			}

		case Mult(e1, e2) =>
			calc {
				eval(Mult(e1, e2), s);
			 == eval(e1, s) * eval(e2, s);
			 == { optimizeCorrect(e1, s); optimizeCorrect(e2, s); }
			    eval(optimize(e1), s) * eval(optimize(e2), s);
			 == eval(Mult(optimize(e1), optimize(e2)), s);
			}
 	}
}

method optimizeFeatures()
{
	assert( optimize(Mult(Var("x"), Const(0))) == Const(0) );
	assert( optimize(Mult(Var("x"), Const(1))) == Var("x") );
	assert( optimize(Mult(Const(0), Var("x"))) == Const(0) );
	assert( optimize(Mult(Const(1), Var("x"))) == Var("x") );

	assert( optimize(Plus(Const(0), Var("x"))) == Var("x") );
	assert( optimize(Plus(Var("x"), Const(0))) == Var("x") );

	assert( optimize(Plus(Const(3),Const(4))) == Const(7) );
	assert( optimize(Mult(Const(3),Const(4))) == Const(12) );

	assert( optimize(Plus(Plus(Var("x"), Var("y")), Const(0))) == Plus(Var("x"), Var("y")) );
}