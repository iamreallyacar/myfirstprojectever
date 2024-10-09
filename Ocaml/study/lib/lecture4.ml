let rec append xs ys =
    match xs with
    | [] -> ys
    | x :: xs -> x :: (append xs ys)

(** Appends two lists. *)

let rec small x ys =
  match ys with
  | [] -> []
  | y :: ys ->
    if y < x then y :: small x ys
    else small x ys

(** Iterates through a list, ys. Returns the list with only elements which are smaller than x. *)

let rec large x ys =
  match ys with
  | [] -> []
  | y :: ys ->
    if x <= y then y :: large x ys
    else large x ys

(** Iterates through a list, ys. Returns the list with only elements which are larger or equal to x. *)

let rec qsort xs =
  match xs with
  | [] -> []
  | x :: xs ->
    let s = small x xs in
    let l = large x xs in
    append s (x :: l)

(** Quicksort doesn't seem to function. *)

let rec filter p xs =
    match xs with
    | [] -> []
    | x :: xs ->
      if p x then x :: filter p xs
      else filter p xs

(** if x matches a condition, p, then x is part of the resulting list*)

let small x xs =
    let ltx y = y < x
    in filter ltx xs

let large x xs =
    let gex y = x <= y
    in filter gex xs

let rec map f xs =
match xs with
| [] -> []
| x :: xs -> f x :: map f xs;;

(** Apply a function to every element in xs*)

let multByConst x p =
    if x = 0.0 then []
    else let mulx y = x *. y
         in map mulx p

let multByConst x p = 
	let mulx y = x *. y
		in map mulx p

let multByConst x p =
	map (fun y -> x *. y) p

let rec sum xs =
    match xs with
    | [] -> 0
    | x :: xs -> x + sum xs

let rec prod xs =
    match xs with
    | [] -> 1
    | x :: xs -> x * prod xs

let sumT xs =
    let rec helper xs acc =
      match xs with
      | [] -> acc
      | x :: xs -> helper xs (acc + x)
    in helper xs 0

let prodT xs =
    let rec helper xs acc =
      match xs with
      | [] -> acc
      | x :: xs -> helper xs (acc * x)
    in helper xs 1

let rec foldRight v f xs =
    match xs with
    | [] -> v
    | x :: xs -> f x (foldRight v f xs)

let rec foldLeft v f xs =
    match xs with
    | [] -> v
    | x :: xs -> foldLeft (f v x) f x

let sum xs = foldRight 0 (+) xs

let prod xs = foldRight 1 ( * ) xs

let sumT xs = foldLeft 0 (+) xs

let prodT xs = foldLeft 1 ( * ) xs

let length xs =
    let incr _ n = 1 + n
    in foldRight 0 incr xs

let lenT xs =
    let incr n _ = 1 + n
    in foldLeft 0 incr xs

let length xs = foldRight 0 (fun _ n -> 1 + n) xs

let small x ys =
    filter ((>) x) ys

let large x ys =
    filter ((<=) x) ys

let rec append xs ys =
    match xs with
    | [] -> ys
    | x :: xs -> x :: (append xs ys)

let append xs ys =
	foldRight ys (fun a b -> a :: b) xs

let rev xs =
	foldLeft [] (fun a b -> b :: a) xs

let map f xs =
	foldRight [] (fun a b -> f a :: b) xs

let rec foldRight v f xs =
    match xs with
    | [] -> v
    | x :: xs -> f x (foldRight v f xs)

let rec foldLeft v f xs =
    match xs with
    | [] -> v
    | x :: xs -> foldLeft (f v x) f xs

let rec filter p xs =
    match xs with
    | [] -> []
    | x :: xs ->
      if p x
	  then x :: filter p xs
      else filter p xs

let filter p xs =
	foldRight []
	(
		fun a b ->
		if p a then a :: b
		else b
	)
	xs

