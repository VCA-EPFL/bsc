import Vector::*;
import Sidemodule::*;


module mkBigInterface#(MyInterface yo)(BigFoo);
    let x <- mkMyInterface;
    Reg#(Bool) to <- mkReg(True);

    let foo = False; 
    foo = True; 

    rule yoyo if (to);
        to <= foo;
        yo.foo();
    endrule

    interface MyInterface sub = x;

    method Bool lol(TestType a); 
        Bool bar = True;
        bar = (x.bar(?) == a);
        return bar;
    endmethod
endmodule

module mkBig2Interface#(MyInterface yo)(BigFoo);
    let x <- mkMyInterface;
    Reg#(Bool) to <- mkReg(True);

    let foo = False; 
    foo = True; 

    rule yoyo if (to);
        to <= foo;
        yo.foo();
    endrule

    interface MyInterface sub = x;

    method Bool lol(TestType a); 
        Bool bar = True;
        bar = (x.bar(?) == a);
        return bar;
    endmethod
endmodule