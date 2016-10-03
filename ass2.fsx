//Purpose/Description: F# Functions
// Name: Adam Tahoun
//Author’s Panther ID: <4078653>
//Certification:
//I hereby certify that this work is my own and none of it is the work of
//any other person.


// Problem 1
let rec cartesian (l1,l2) = 
    match (l1,l2) with
    | ([],[]) -> []
    | (_,[]) -> []
    | ([],_) -> []
    | (x::xs,l2) -> (List.map (fun y -> x,y) l2) @ cartesian (xs,l2);;

let problem1 () =
    let l1=["a";"b";"c"]
    let l2 = [1;2]
    let crossproduct = cartesian (l1,l2)
    let crossproduct2 = cartesian (l1,[])
    printfn "------------------Problem 1---------------"
    printfn "The cartesian product of %A and %A is: %A." l1 l2 crossproduct
    printfn "The cartesian product of %A and %A is: %A." l1 [] crossproduct2;;

problem1();;




// Problem 2
let rec powerset = function 
    | [] -> [[]]
    | x::xs -> List.collect (fun y -> [y; x::y]) (powerset xs);;

let problem2 ()=
    let l1 = [1;2;3]
    let pow =powerset l1
    let pow1 = powerset []
    printfn "---------Problem 2-------"
    printfn "The power set of %A is %A" l1 pow
    printfn "the power set of the empty is: %A " pow1;;

problem2();;


// Problem 3
let rec transpose matrix = 
    match matrix with
    | row::rows ->
        match row with 
        | col::cols ->
            let first = List.map List.head matrix
            let rest = transpose (List.map List.tail matrix)
            first :: rest 
        | _ -> []
    | _ -> [];;
    
    
let problem3 () =
    let l1 = [1;2;3]
    let l2 = [4;5;6]
    let trans = transpose [l1;l2] 
    printfn "---------Problem 3--------------"
    printfn "The transpose of %A and %A is: %A" l1 l2 trans;;


// Problem 4 (see answers it the comments below the function)
let rec sort = function
  | [] -> []
  | [x] -> [x]
  | x1::x2::xs -> if x1 <= x2 then x1 :: sort (x2::xs)
                              else x2 :: sort (x1::xs)
//Checklist for Programming with Recursion
//Basis Case must return the correct result: True, if the list is empty then we return an empty list (which is sorted)
//         If the cardinality of the list == 1 then we return the list b.c a list of size one is already sorted.
//Non-basis case must do so too, assuming Rec. hypothesis: False, the non-basis case does not return the correct result, but returns a slightly varied version  of the list
//         We have to keep swapping elemets until we can parse through all elements without having to swap once.
// Recursion must be on a "smaller" input: True, each time we make the recursice call we are doing a list one element smaller then the prev list.


// Problem 5
let rec merge = function
| ([], ys) -> ys
| (xs, []) -> xs
| (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                            else y :: merge (x::xs, ys)
let rec split = function
| [] -> ([], [])
| [a] -> ([a], [])
| a::b::cs -> let (M,N) = split cs 
              (a::M, b::N)

let rec mergesort = function
| []  -> []
| [a] -> [a]
| L   -> let (M, N) = split L
         merge (mergesort M, mergesort N)
//Checklist for Programming with Recursion
//Basis Case must return the correct result: True
//Non-basis case must do so too, assuming Rec. hypothesis: False
// Recursion must be on a "smaller" input: True

let problem5 () =
    let l1 = [108;15;50;4;8;42;23;16]
    let ms = mergesort l1
    printfn "---------------Problem 4----------------"
    printfn "After fixing the problem with functions split and mergesort the mergesort of l1 is %A" ms;;

problem5();;

// Problem 6 


// uncurry :: ('a -> 'b -> 'c) -> 'a * 'b -> 'c
let uncurry f (a,b) = f a b;;
// multiply :: int -> int -> int
let problem6 () = 
    let addition (x1,x2) = x1 + x2
    let addCurry = curry addition
    let add2 = addCurry 2
    let value = add2 5
    printfn "The curried function returns %d" value
    printfn "The type of function curry is  curry :: ('a * 'b -> 'c) -> 'a -> 'b -> 'c"
    let addUncurry = uncurry addCurry
    let value2  = addUncurry (5,2)
    printfn "The uncurried function returns %d" value2;;
    printfn "The type of function uncurry is: ('a -> 'b -> 'c) -> 'a * 'b -> 'c ";;

problem6();;





