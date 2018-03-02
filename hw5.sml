fun zip (nil,nil) = nil (* takes two lists and creates a tupled list, extra values are left unpaired*)
| zip (_,nil) = nil
| zip (nil,_) = nil
| zip (x::xs, y::ys) = 
	let
		val cur = (x,y)::zip (xs,ys)
	in
		cur
	end;

fun reduce f (nil) = nil (*was told to use this, but I haven't had a need/way to*)
| reduce f (a::b) = foldl f a b;

fun vectorAdd (nil,nil) = nil (*adds two row vectors of even length*)
| vectorAdd (_,nil) = nil
| vectorAdd (nil,_) = nil
| vectorAdd (x::xs,y::ys) =
	let
		val cur = (x+y)::vectorAdd (xs,ys)
	in
		cur
	end;

fun svProduct (x,nil) = nil (*multiplies row vector by x*)
| svProduct (x,[y]) = [x*y]
| svProduct (x,y::ys) =
	map (fn (z) => x*z) (y::ys);

fun vmProduct (nil,nil) = nil (*takes a row vector and multiplies with a matrix to produce a row vector of column length*)
| vmProduct (_,nil) = nil
| vmProduct (nil,_) = nil
| vmProduct ((x::xs),(y::ys)) =
	let
		val cur = svProduct(x,y)::vmProduct(xs,ys)
	in
		cur
	end;

fun matrixProduct (nil,nil) = nil (*takes two matrices and multiplies them, working aside from vm's problems*)
| matrixProduct (_,nil) = nil
| matrixProduct (nil,_) = nil
| matrixProduct (x::xs,y::ys) = 
	let
		val cur = (vmProduct(x,y::ys))::matrixProduct(xs,y::ys)
	in
		cur (*fix vmProduct to fix this *)
	end;

fun helper (nil) = nil (*helper of vm, independent for testing purposes*)
		| helper ([x]: int list list) = (x: int list)
		| helper ((x::xn::nil): int list list) = (vectorAdd(x,xn): int list)
		| helper ((x::xn::xs): int list list) = helper(vectorAdd(x,xn)::xs);