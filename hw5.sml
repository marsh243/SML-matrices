fun zip (nil,nil) = nil
| zip (_,nil) = nil
| zip (nil,_) = nil
| zip (x::xs, y::ys) = 
	let
		val cur = (x,y)::zip (xs,ys)
	in
		cur
	end;

fun reduce f (nil) = nil
| reduce f (a::b) = foldl f a b;

fun vectorAdd (nil,nil) = nil
| vectorAdd (_,nil) = nil
| vectorAdd (nil,_) = nil
| vectorAdd (x::xs,y::ys) =
	let
		val cur = (x+y)::vectorAdd (xs,ys)
	in
		cur
	end;

fun svProduct (x,nil) = nil
| svProduct (x,[y]) = [x*y]
| svProduct (x,y::ys) =
	map (fn (z) => x*z) (y::ys);

fun vmProduct (nil,nil) = (nil: int list)
| vmProduct (_,nil) = (nil: int list)
| vmProduct (nil,_) = (nil: int list)
| vmProduct ([x],[[y]]) = svProduct(x,[y])
| vmProduct (((x::xs): int list),((y::yn::ys): int list list)) =
	let
		val cur = svProduct(x,y)::vmProduct(xs,yn::ys) (*operator operand type mismatch, vmProduct is not registering as a int list*)
		fun helper (nil) = nil
		| helper ([x]: int list list) = (x: int list)
		| helper ((x::xn::nil): int list list) = (vectorAdd(x,xn): int list)
		| helper ((x::xn::xs): int list list) = helper(vectorAdd(x,xn)::xs)
	in
		(helper(cur): int list) (* helper works independently, but recursive calls of vm are broken*)
	end;

fun matrixProduct (nil,nil) = nil
| matrixProduct (_,nil) = nil
| matrixProduct (nil,_) = nil
| matrixProduct (x::xs,y::ys) = 
	let
		val cur = (vmProduct(x,y::ys))::matrixProduct(xs,y::ys)
	in
		cur (*fix vmProduct to fix this *)
	end;