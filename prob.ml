
module ListMonad = struct
  let return x = [x]
  let bind xs f = List.flatten (List.map f xs)
  let map f xs = List.map f xs
  let (>>=) xs f = bind xs f
  let (>>|) xs f = map f xs
  let join xss = List.flatten xss
  let (let*) xs f = bind xs f
  (* let (let+) xs f = map f xs *)
  let show f xs =
    "[" ^ (String.concat "; " (List.map f xs)) ^ "]"
end

let test =
  let open ListMonad in
  let xs = [1; 2; 3] in
  let ys = [4; 5] in
  (let* x = xs in
  let* y = ys in
  let result = x + y in
  return result)
  |> show string_of_int
  |> print_endline

module ProbMonad = struct
  type prob = float
  type 'a dist = Dist of ('a*prob) list

  let unpack (Dist xs) = xs
  let dedup (Dist xs) =
    let rec aux acc = function
      | [] -> List.rev acc
      | (x, p) :: (y, q) :: rest when x = y ->
          aux acc ((x, p +. q) :: rest)
      | (x, p) :: rest -> aux ((x, p) :: acc) rest
    in
    Dist (aux [] (List.sort compare xs))
  let sum = List.fold_left (+.) 0.0
  let total (Dist xs) = xs |> List.map snd |> sum
  let normalize (Dist xs) =
    let total_prob = total (Dist xs) in
    if total_prob = 0.0 then Dist [] else
      Dist (List.map (fun (x, p) -> (x, p /. total_prob)) xs)
  let show f (Dist xs) =
    let Dist xs = dedup (Dist xs) in
    "{" ^ (String.concat "; " (List.map (fun (x, p) -> Printf.sprintf "%s: %.1f%%" (f x) (100.*.p)) xs)) ^ "}"

  type 'a event = 'a -> bool
  let prob (e: 'a event) (Dist xs) = 
    xs |> List.filter (fun (x, _) -> e x)
    |> List.map snd
    |> sum
  
  let uniform xs = normalize (Dist (List.map (fun x -> (x, 1.0)) xs))
  let die n = uniform (List.init n (fun i -> i + 1))
  let coin p head tail = 
    assert (p >= 0.0 && p <= 1.0);
    Dist [(head, p); (tail, 1.0 -. p)]

  let fmap f (Dist xs) =
    Dist (List.map (fun (x, p) -> (f x, p)) xs)
  let return x = Dist [(x, 1.0)]
  let (<*>) (Dist fs) (Dist xs) =
    Dist (let open ListMonad in
    let* (x,px) = xs in
    let* (f,pf) = fs in
    return (f x, px *. pf))
  let (>>=) (Dist xs) f =
    Dist (
      let open ListMonad in
      let* (x, p) = xs in
      let Dist ys = f x in
      let* (y, q) = ys in
      return (y, p *. q)
    ) 
    (* |> dedup *)
  let (let*) xs f = xs >>= f
  let foldl1 f xs =
    match xs with
    | [] -> failwith "foldl1: empty list"
    | x :: xs' ->
      List.fold_left (fun acc x -> f acc x) x xs'
  let binom n p =
    (* binom n p = foldl1 (\x y -> squishD (liftA2 (+) x y)) $ replicate n (coin p 1 0) *)
    let experiments = List.init n (fun _ -> coin p 1 0) in
    foldl1 (fun (Dist acc: int dist) (Dist xs: int dist) ->
      (* squishD (liftA2 (+) acc x) *)
      Dist (let open ListMonad in
      let* (x,p) = acc in
      let* (y,q) = xs in
      return (x + y, p *. q))
    ) experiments
  let condition f (Dist xs) =
    normalize (Dist (List.filter (fun (x, _) -> f x) xs))
  let rec dice num n =
    if n == 0 then return []
    else
      let* x = die num in
      let* xs = dice num (n - 1) in
      return (x :: xs)
  let select n xs = 
    (* helper function to select elements from a list uniformly *)
    let selectOne xs =
      uniform (
        let rec aux acc = function
          | [] -> []
          | x :: xs' -> 
              (x, List.rev acc @ xs') :: aux (x :: acc) xs'
        in
        aux [] xs
      )
    in
    let rec selectMany n xs =
      if n <= 0 then return ([], xs)
      else
        let* (y, rest) = selectOne xs in 
        let* (ys, other) = selectMany (n - 1) rest in
        return (y :: ys, other)
    in
    fmap (fun (ys, _) -> List.rev ys) (selectMany n xs)


end

let test_prob =
  let open ProbMonad in
  let d6 = die 6 in
  show string_of_int d6 |> Printf.printf "D6: %s\n";
  let result = 
    let open ProbMonad in
    (* d1 >>= fun x -> d2 >>= fun y -> return (x + y) *)
    (* or equivalently using let* syntax *)
    let* x = d6 in
    let* y = d6 in
    return (x + y)
  in
  show string_of_int result |> Printf.printf "D6 + D6: %s\n";
  let d4 = die 4 in
  let result = 
    let open ProbMonad in
    let* x = d6 in
    let* y = d6 in
    let* z = d4 in
    return (x + y > z)
  in
  show string_of_bool result |> Printf.printf "2 D6 > D4: %s\n";

  let result = dice 6 2 in
  show (ListMonad.show string_of_int) result |> Printf.printf "2 D6: %s\n";
  let result = result >>= fun xs -> return (List.sort compare xs) in
  show (ListMonad.show string_of_int) result |> Printf.printf "2 D6: %s\n";
  let result = dice 6 4 >>= fun xs ->
    return ((xs |> List.filter (fun x -> x = 6) |> List.length)>=2) in
  show string_of_bool result |> Printf.printf "4 D6: at least 2 sixes: %s\n";

  let result = select 3 ["R"; "R"; "G"; "G"; "B"] >>= (fun xs -> return (xs = ["R";"G";"B"])) in
  (* let result = select 3 ["R"; "R"; "G"; "G"; "B"] >>= (fun xs -> return (xs = ["R";"B";"B"])) in *)
  show string_of_bool result |> Printf.printf "Select 3 from [R; R; G; G; B]: %s\n";
  (* let result = select 3 ["R"; "R"; "G"; "G"; "B"] in *)
  (* show (ListMonad.show Fun.id) result |> Printf.printf "Select 3 from [R; R; G; G; B]: %s\n"; *)

  let coin_flip = coin 0.5 "Heads" "Tails" in
  show Fun.id coin_flip |> Printf.printf "Coin Flip: %s\n";
  let conditioned = condition (fun x -> x = "Heads") coin_flip in
  show Fun.id conditioned |> Printf.printf "Conditioned Coin Flip: %s\n";

  let binomial_result = binom 10 0.5 in
  show string_of_int binomial_result |> Printf.printf "Binomial Distribution (n=10, p=0.5): %s\n";
  
  Printf.printf "%!";

  ()


let [@warning "-8"] () =
  let open ProbMonad in
  let d6 = die 6 in
  (* suppress warning pattern match *)
  let losses = 
    let* a1 = d6 in
    let* a2 = d6 in
    let* a3 = d6 in
    let* d1 = d6 in
    let* d2 = d6 in
    let* d3 = d6 in
    let [a3'; a2'; a1'] = List.sort compare [a1; a2; a3] in
    let [d2'; d1'] = List.sort compare [d1; d2] in
    (* old risk -- the defender gets three dice *)
    (* let [d3'; d2'; d1'] = List.sort compare [d1; d2; d3] in  *)
    let score =
      (if a1' > d1' then 1 else -1) + (* compare best dice *)
      (if a2' > d2' then 1 else -1)   (* compare second best dice *)
    in 
    return score
  in
  show string_of_int losses |> Printf.printf "Risk losses: %s\n";
  ()


let time f x = 
  let start = Sys.time () in
  let result = f x in
  let duration = Sys.time () -. start in
  Printf.printf "Time taken: %.3f seconds\n" duration;
  result

let () =
  let open ProbMonad in
  (* dice 6 7 will be much slower -- eager dedup does not help here *)
  time (fun () ->
  let rec diceSix num n =
    if n == 0 then return []
    else
      let* x = die num in
      let* xs = dice num (n - 1) in
      if x = 6 then
        return (6 :: xs)
      else
        return (xs)
  in
  let result = diceSix 6 7 >>= fun xs ->
    return ((xs |> List.filter (fun x -> x = 6) |> List.length)>=2) in
  show string_of_bool result |> Printf.printf "n D6: at least 2 sixes: %s\n"
  ) ();
  ()


module ProbNotation = struct
  include ProbMonad
  open ProbMonad
  let lift op x y = 
    let* a = x in
    let* b = y in
    return (op a b)
  let (+) = lift ( + )
  let (-) = lift ( - )
  let ( * ) = lift ( * )
  let (/) = lift ( / )
  let (>) = lift ( > )
  let (>=) = lift ( >= )
  let (<) = lift ( < )
  let (<=) = lift ( <= )
end

let () =
  let open ProbNotation in
  let d6 = die 6 in
  let d4 = die 4 in
  show string_of_bool (d4+d4 > d6) |> Printf.printf "D6: %s\n";
  ()