let rec exists f l = match l with
	  [] -> false (* the list is empty, return false *)
	| h::t -> if (f = h) then true (* the list has a head and a (possibly empty) tail. Check the return value of the predicate 'f' when applied to the head *)
	  else exists f t (* the predicate is false, recursively call the 'exists' function on the tail *);;

let rec subset a b = match a with
	  [] -> true (* the list a is empty and thus is a subset of b *)
	| h::t -> if exists h b then subset t b (* the list has a head and a (possibly empty) tail. Check if each element of a exists in b. If any one element doesn't exist in b, return false *)
	  else false;;

let equal_sets a b = (subset a b) && (subset b a) (* sets are equal iff they are subsets of each other *);;

let equal_rule_sets (f_rules, a) (rules, b) = equal_sets a b;;

let proper_subset a b = (not (equal_sets a b)) && (subset a b) (* set a is a proper set of b iff a is a subset of b, but a is not equal to b *);;

let rec computed_fixed_point eq f x = 
	if (eq (f x) x) then x (* If the fixed point has been reached, return the fixed point value *)
	else (computed_fixed_point eq f (f x)) ;; (* Otherwise, compute the value of the function with regards to x and try again to find the fixed point. *)

type ('nonterm, 'term) symbol = N of 'nonterm | T of 'term;;

let rec check_nterm_list s l = match l with
	  [] -> false (* If the list is empty, then s is not in the list *)
	| h::t -> if (s = h) then true (* Found the symbol so return true *)
	  else check_nterm_list s t (* Haven't found the symbol so continue searching *);;

let is_term s l = match s with 
	T s -> true (* Check if the symbol is a terminal *)
	| N s -> check_nterm_list s l (* Check if the nonterminal terminates via a rule that ends in a terminal by checking a list of symbols that do so *);;

let rec check_rule rh l = match rh with
	[] -> true (* The right hand symbols are terminals or terminate or the rule doesn't have a right hand side (which isn't allowed so we'll ignore that) *)
	| h::t -> if is_term h l then check_rule t l (* If symbol is terminal, check the rest of the right hand side *)
		else false (* Found a nonterminal that doesn't terminate *);;

let rec find_good_rules rules ntlist = match rules with
	[] -> ntlist
	| (lh,rh)::t -> if (check_rule rh ntlist) then begin
		if not (check_nterm_list lh ntlist) then
			find_good_rules t (lh::ntlist) (*add new good nonterm to list *)
		else	find_good_rules t ntlist (* already a good nonterm *)
		end (* The list has a head and possibly empty tail. If the rule is not a blind alley rule, add it to the return value and add the nonterminal to the nonterminal list and continue with the list *)
	 else find_good_rules t ntlist (* Ignore the first element and check the rest of the list if it is not terminating *);;

let filter_rules (rules, ntlist) = 
	(rules, (find_good_rules rules ntlist)) ;;

let rec ordered_rules orig_rules ntlist = match orig_rules with
	[] -> []
	| (lh,rh)::t -> if (check_rule rh ntlist) then (lh,rh)::(ordered_rules t ntlist) (* Check each rule to see if it is a good rule. If so, add it to the return list.*)
	else ordered_rules t ntlist (* If it is not a good rule, ignore it*);;
	
let filter_blind_alleys (start_sym, rules) = 
	(start_sym, ordered_rules rules (snd(computed_fixed_point (equal_rule_sets) (filter_rules) (rules,[]))))
	(*filter_blind_alleys is a wrapper function for a function that makes 
	sure the rules are in order*) ;;
