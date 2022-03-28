Class List { 
	isNil() : Object { abort() };

	cons(hd : Int) : Cons {
	  let new_cell : Cons <- new Cons in
		new_cell.init(hd,self)
	};

	car() : Int { abort() };

	cdr() : List { abort() };
};

Class Cons inherits List {
	car : Int;
	cdr : List;

	isNil() : Bool { false };

	init(hd : Int, tl : List) : Cons {
	  {
	    car <- hd;
	    cdr <- tl;
	    self;
	  }
	};
	  
	car() : Int { car };

	cdr() : List { cdr };
};

Class Nil inherits List {
	isNil() : Bool { true };
};





