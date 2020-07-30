import List::*;  // List::select;

interface SFIFO #(type t);
    method Action enq(t val);
    method t first();
    method Action deq();
    method Action clear();
    method Bool isFull();
    method Bool isEmpty();
endinterface

typedef Bit#(3) Ptr;

module mkSFIFO(SFIFO#(t)) provisos (Bits#(t, ts));
  Integer size = 8;

  Ptr maxptr = fromInteger(size - 1);
  Ptr minptr = 0;

  List#(Reg#(t)) rs = replicate(size, ?);
  Integer i;
  for (i=0; i<size; i=i+1)
    begin
      Reg#(t) r();
      mkRegU the_r(r);
      rs[i] = asReg(r);
    end

  Reg#(Ptr) head();
  mkReg#(0) the_head(head);

  Reg#(Ptr) tail();
  mkReg#(0) the_tail(tail);

  function Ptr incr(Ptr ptr);
      incr = (ptr == maxptr ? minptr : ptr + 1);
  endfunction
  function Ptr decr(Ptr ptr);
      decr = (ptr == minptr ? maxptr : ptr - 1);
  endfunction

  Bool notEmpty = head != tail;
  Bool notFull  = incr(tail) != head;

  function t get(Ptr ptr);
      get = (select(rs, ptr))._read;
  endfunction

  function Action put(Ptr ptr, t v);
    action
      (select(rs, ptr)) <= v;
    endaction
  endfunction
  
  method enq(x) if (notFull) ;
    action
      put(tail, x);
      tail <= incr(tail);
    endaction
  endmethod: enq

  method first() if (notEmpty) ;
      return (get(head));
  endmethod: first

  method deq() if (notEmpty) ;
    action
      head <= incr(head);
    endaction
  endmethod: deq

  method clear() ;
    action
      head <= minptr;
      tail <= minptr;
    endaction
  endmethod: clear

  method isFull();
      return (!notFull);
  endmethod

  method isEmpty();
      return (!notEmpty);
  endmethod

endmodule

