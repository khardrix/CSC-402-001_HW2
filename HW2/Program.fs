// Learn more about F# at http://fsharp.org
module HW2

open System

// Problem #1
let min3 (x, y, z) =
    if(x < y && x < z) then x
    else if(y < x && y < z) then y
    else z;;
    
// Problem #2
type Dinosaur = {name: string; weight: float; height: float;}
let tyranno = {name = "Tyrannosaurus"; weight = 7.0; height = 3.66;}
let brachio = {name = "Brachiosaurus"; weight = 35.0; height = 9.4;}
let nameOfHeavier (x: Dinosaur) (y: Dinosaur) =
    if(x.weight > y.weight) then x.name
    else y.name;;
    
// Problem #3
let threeDDist (x1: float, y1, z1) (x2, y2, z2) =
    let xDist = (x1 - x2) * (x1 - x2)       // (x1 - x2) ** 2.0
    let yDist = (y1 - y2) * (y1 - y2)       // (y1 - y2) ** 2.0
    let zDist = (z1 - z2) * (z1 - z2)       // (z1 - z2) ** 2.0
    sqrt(xDist + yDist + zDist);;

// Problem #4
type Shape =
    | Circle of float
    | Square of float
    | Triangle of float * float * float
    | Rectangle of float * float;;
let isShape = function
    | Circle r -> r > 0.0
    | Square a -> a > 0.0
    | Triangle(a, b, c) ->
        a > 0.0 && b > 0.0 && c > 0.0
        && a < b + c && b < c + a && c < a + b
    | Rectangle(l, w) -> l > 0.0 && w > 0.0;;
let area x =
    if not (isShape x)
    then failwith "not a legal shape"
    else match x with
            | Circle r -> Math.PI * r * r         // added extra indentations due to RIDER suggestions
            | Square a -> a * a
            | Triangle(a, b, c) ->
                let s = (a + b + c) / 2.0
                sqrt(s * (s - a) * (s - b) * (s - c))
            | Rectangle(l, w) -> l * w;;
let perimeter x =
    if not (isShape x)
        then failwith "not a legal shape"
    else match x with
            | Circle r -> 2.0 * Math.PI * r      // added extra indentations due to RIDER suggestions
            | Square a -> 4.0 * a
            | Triangle(a, b, c) -> a + b + c
            | Rectangle(l, w) -> 2.0 * (l + w);;
let circ = Circle 2.1;;
let sq = Square 3.6;;
let tri = Triangle(5.0, 12.0, 13.0);;
let rect = Rectangle(3.0, 4.0);;


// Problem #5
let makeShape (s : string) (a : float) (b : float option) (c : float option) =
    if (s = "Circle" && isShape (Circle a) && b = None && c = None) then
        Some (Circle a)
    else if (s = "Square" && isShape (Square a) && b = None && c = None) then
         Some (Square a)
    else if (s = "Rectangle" && isShape (Rectangle(a, b)) && c = None) then
        Some (Rectangle(a, b))
    else if (s = "Triangle" && isShape (Triangle (a, b, c))) then
        Some (Triangle(a, b, c))
    else
        None;;
        
        
(*
let makeShape (s : string) (a : float) (b : float option) (c : float option) =
    if s = "Circle" && isShape (Circle a) && b = None && c = None then
        Some (Circle a)
    else if s = "Rectangle" && isShape (Rectangle(a, b)) && ...
*)
    
(*
if(b = None && c = None) then
    if(s.Equals("Circle") || s.Equals("Square")) then
        if not (isShape s a) then
            failwith "not a legal shape"
        else
            Some (s a)
    else if(c = None) then
        if(s.Equals("Rectangle")) then
            if not (isShape s a) then
                failwith "not a legal shape"
            else
                Some (s (a, b))
    else
        if(s.Equals("Triangle")) then
            if not (isShape s a) then
                failwith "not a legal shape"
            else
                Some (s (a, b, c));;
*)                

(*
let makeShape (s : string) (a : float) (Some (b : float)) (Some (c : float)) =
    if not (isShape a)
        then failwith "not a legal shape"
    else
        if(b = None && c = None && s = "Circle" && isShape a)
            then x
*)
 
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
