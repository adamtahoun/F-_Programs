//Purpose/Description: F# Functions
// Name: Adam Tahoun
//Author’s Panther ID: <4078653>
//Certification:
//I hereby certify that this work is my own and none of it is the work of
//any other person.

// Function for Greated Common Divisor
let rec gcd (x,y) =
    if y=0 then x
    else gcd(y,x%y);;

let example1 () =
    let x = (8,12)
    let y = gcd(8,12)
    printfn " The gcd of %A is %A" x y;;


example1 ();;


// Function to add two fractions
let (.+)(a, b)(c, d) = 
   let x = (a*d)+(b*c)
   let y = (b*d)
   let z = gcd(x,y)
   (x/z,y/z);;

let example2 () = 
    let x = (1,2)
    let y = (1,3)
    let (a,b) = x .+ y
    printfn "The sum of %A and %A is (%A,%A)" x y a b;;

example2 ();;
    

// Function to multiply two fractions
let (.*)(a, b)(c, d) = 
    let x = (a*c)
    let y = (b*d)
    let z = gcd(x,y)
    (x/z,y/z);;

let example3 () = 
    let x = (1,2)
    let y = (1,3)
    let (a,b) = x .* y
    printfn "The multiplication of %A and %A is (%A,%A)" x y a b;;

example3 ();;

let example4 () =
    let x = (1,2)
    let y = (2,3) 
    let z = (3,7)
    let (a,b) = x .+ y.*z 
    printfn "%A + %A * %A = (%A,%A)" x y z a b ;;

example4 ();;

// Function to reverse a list of lists
let revlists xs = List.map (fun x -> List.rev x) xs;;

let example5 () =
    let list = [[0;1;1];[3;2];[];[5]]
    let x =revlists [[0;1;1];[3;2];[];[5]]
    printfn "The reverse of %A is %A" list x;;

example5 ();;


// Function to interleave two lists
let rec interleave xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::xs, y::ys -> x::y::interleave xs ys

let example6 () = 
    let l1 = [1..10]
    let l2 = [3..5]
    let l3 =interleave l1 l2
    printfn "The interleave of %A and %A is %A" l1 l2 l3;;

example6();;

// Function to cut a list where the first list is size n.
let gencut (n, list) = 
    let rec aux = function
        | 0, xs, ys -> (List.rev xs,ys) // must return xs reversed since you are adding the head of ys to the beginning of the list each time.
        | n, xs, [] -> (xs, [])
        | n, xs, ys -> aux(n-1, List.head ys :: xs, List.tail ys)
    aux (n, [], list);;

let example7 () =
    let l1 = [1..10]
    let n = 5
    let l2 =gencut(n, l1)
    printfn "A general cut of %A with the list list being size %A is %A" l1 n l2;;

example7();;
    

// Function to cut a list of even length into two equal lists.
let cut list = 
    let n = (List.length list)/2
    gencut(n,list);;

let example8() = 
    let list = [1..6] 
    let clist = cut list
    printfn "A cut of %A is %A" list clist;;

example8();;

// Function to shuffle a list perfectly.
let shuffle list =
    let (x,y) = cut list
    interleave x y

let example9 () = 
    let list = [1..6]
    let slist = shuffle [1;2;3;4;5;6]
    printfn " The perfect shuffle of list %A is %A" list slist;;

example9();;

// Helper function for countshuffles
let countaux (deck, target) = 
    let rec aux = function 
        | (n, deck, target) when deck = target -> n // check if the two lists are equal
        | (n, deck, target) when deck <> target -> aux( n+1, shuffle deck, target) // if not equal then reshuffle and increment n 
    aux (1, deck, target);; // must start n at one since you pass a shuffled deck already

// Counts how many times a deck of size n needs to be shuffled to get back to original deck.
let countshuffles n = 
    let deck = [1..n]
    let target = deck
    countaux(shuffle deck,target);;

let example10 () = 
    let m = 52
    let n = countshuffles 52
    printfn "It takes %A shuffle(s) to get back a list size %A" m n;;

example10 ();;

