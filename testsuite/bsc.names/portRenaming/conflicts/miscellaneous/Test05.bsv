typedef Bit#(5) Type;

interface S1#(type mType);
 (*prefix = "start" *)
 method mType result((* port = "a" *)mType c);
 method ActionValue#(mType) check(mType d);
endinterface

interface IFC#(type aType);
 method Action start(aType a, aType b);
 (* prefix = "" *)
 interface S1#(aType) subIFC;
endinterface

(* synthesize *)
module mkDesign_05 (IFC#(Type));

  Reg#(Type) val <- mkReg(0);
  Reg#(Type) res <- mkReg(0);


  method Action start(a,b);
    val <= a;
    res <= b;
  endmethod

  interface S1 subIFC;
    method Type result(c);
      return res+c;
    endmethod

    method ActionValue#(Type) check(d);
      val <= val + 1;
      res <= res + 2;
	  return res+d;
    endmethod
  endinterface
endmodule
