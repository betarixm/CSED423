Class C1 inherits SELF_TYPE { 
  f(x : Int, y : SELF_TYPE) : Int { 1 };

};

Class SELF_TYPE {
  b : SELF_TYPE <- new SELF_TYPE;

  g(x : Int) : SELF_TYPE {
	let y : SELF_TYPE in
	  case y of
	    a : SELF_TYPE => new SELF_TYPE;
	    a : Int => (new SELF_TYPE)@SELF_TYPE.g(a);
	  esac
  };
};


