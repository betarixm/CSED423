class MyClass {
  do(arg:Object) : Void {
	case arg of
	  o1 : Object1 => 1;
	  o2 : Object2 => o2.special();
	esac 
  };
} ;

