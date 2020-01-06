let subset a b =
	let subset = ref true in
	if ((List.length a) > (List.length b)) then subset := false
	else
		let found = ref false in
		for i = 0 to ((List.length a)-1) do
			found := false;
			for j = 0 to ((List.length b)-1) do
				if ((List.nth a i) == (List.nth b j)) then found := true;
			done;
			if (!found != true) then subset := false;
		done;
	;
	!subset;;
let result = subset [1;2;5] [2;1];;
print_endline (string_of_bool result);;
