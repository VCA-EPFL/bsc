typedef Bit#(5) Type;

interface IFC#(type mType);
  (* ready = "stready" *)
 method Action start(mType a, mType b);
  (* ready = "resready" *)
 method mType result(mType c);
  (* ready = "chready" *)
 method ActionValue#(mType) check(mType d);
endinterface

(* synthesize *)
module mkDesign_01 (IFC#(Type));

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
