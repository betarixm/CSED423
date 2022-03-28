class Main inherits IO {
    c : String <- "Correct\n";
    w : String <- "Wrong\n";
    main():SELF_TYPE {
      {
	out_string("Bool1 Eq: ");
	if (true = true.copy())
	then out_string(c)
	else out_string(w)
	fi;
	out_string("Bool2 Eq: ");
	if (false = false.copy())
	then out_string(c)
	else out_string(w)
	fi;
	out_string("Bool3 Eq: ");
	if (false = true.copy())
	then out_string(w)
	else out_string(c)
	fi;
	out_string("Bool4 Eq: ");
	if (true = false.copy())
	then out_string(w)
	else out_string(c)
	fi;
	out_string("Bool Neg: ");
	if not (true = true.copy())
	then out_string(w)
	else out_string(c)
	fi;
	out_string("Int1 Eq: ");
	if (5 = 5.copy())
	then out_string(c)
	else out_string(w)
	fi;
	out_string("Int2 Eq: ");
	if (7 = 3.copy())
	then out_string(w)
	else out_string(c)
	fi;
	out_string("Neg Int1: ");
	if (~3 < ~5.copy())
	then out_string(w)
	else out_string(c)
	fi;
	out_string("Neg Int2: ");
	if (~6 <= ~6.copy())
	then out_string(c)
	else out_string(w)
	fi;
	out_string("Pos Int1: ");
	if  (3 < 5.copy())
	then out_string(c)
	else out_string(w)
	fi;
	out_string("Pos Int2: ");
	if (7 <= 7.copy())
	then out_string(c)
	else out_string(w)
	fi;
	out_string("String1 Eq: ");
	if ("true" = "true")
	then out_string(c)
	else out_string(w)
	fi;
	out_string("String2 Eq: ");
	if ("true" = "true".copy())
	then out_string(c)
	else out_string(w)
	fi;
	out_string("String3 Eq: ");
	if ("true_" = "true".copy())
	then out_string(w)
	else out_string(c)
	fi;
	out_string("String4 Eq: ");
	if ("" = "".copy())
	then out_string(c)
	else out_string(w)
	fi;
      }
    };
};
