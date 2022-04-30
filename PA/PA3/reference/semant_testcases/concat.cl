Class Main inherits IO {
	sum : String <- "";
	s : String <- "_";
	main() : Object {
	 {
	 while not (s = "") loop
	  {
	    out_string("Type in a line: ");
	    s <- in_string();
	    sum <- sum.concat(s);
	  }
	 pool;
	 out_string("The sum is:\n").out_string(sum).out_string("\n");
	 } 
	};
};
