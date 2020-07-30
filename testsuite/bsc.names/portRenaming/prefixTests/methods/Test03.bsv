interface IFC#(type anyType);
  (* prefix = "Variable" *)
 method Action start(anyType a, anyType b);
  (* prefix = "Variable" *)
 method anyType result(anyType c);
  (* prefix = "Variable" *)
 method ActionValue#(anyType) check(anyType d);
endinterface

typedef Bit#(7) Type;

(* synthesize *) 
module mkDesign_03 (IFC#(Type));

  Reg#(Type) val <- mkReg(0);
  Reg#(Type) res <- mkReg(0);


  method Action start(a,b);
    val <= a;
    res <= b;
  endmethod	

  method Type result(c);
     return res+c;
  endmethod	
  	 
  method ActionValue#(Type) check(d);
    val <= val + 1;
    res <= res + 2;
	return res+d;
  endmethod	
	
endmodule
