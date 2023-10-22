import Vector::*;

typedef Bit#(32) TestType;

typedef struct { Bit#(32) foo; } Bar deriving (Eq);

typedef struct { a yo; } Maybe#(type a);

interface MyInterface;
    method Action foo();
    method Bit#(32) bar(TestType baz);
endinterface

module mkMyInterface(MyInterface);
    Vector#(2, Reg#(Bit#(32))) d <- replicateM(mkReg(0));

    method Action foo();
        noAction;
    endmethod

    method Bit#(32) bar(TestType baz);
        return 0;
    endmethod
endmodule

interface BigFoo;
    interface MyInterface sub;
    method Bool lol(TestType a);
endinterface

