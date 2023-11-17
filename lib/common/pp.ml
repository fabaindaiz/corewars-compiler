let rec pp_list sep pp_elem fmt = function
  | [] -> ()
  | [ x ] -> Format.fprintf fmt "%a" pp_elem x
  | x :: xs ->
      Format.fprintf fmt "%a%s%a" pp_elem x sep (pp_list sep pp_elem) xs
