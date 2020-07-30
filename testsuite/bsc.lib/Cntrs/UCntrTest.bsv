import Cntrs::*;

import StmtFSM::*;


(*synthesize*)
module sysUCntrTest();

   UCount  udut <- mkUCount(1,15);
   UCount  idut <- mkUCount(1,7);
   UCount  bdut <- mkUCount(1,(2**32)-1);

   Reg#(UInt#(5))   i <- mkReg(0);

   function Action incrAll (Integer v)  = (
      action
         udut.incr ( (v));
         idut.incr ( (v));
         bdut.incr ( (v));
      endaction);
   function Action decrAll (Integer v)  = (
      action
         udut.decr ( (v));
         idut.decr ( (v));
         bdut.decr ( (v));
      endaction);
   function Action updateAll (Integer v)  = (
      action
         udut.update ( (v));
         idut.update ( (v));
         bdut.update ( (v));
      endaction);
   
   function Action writeAll (Integer v)  = (
      action
         udut <= ( (v));
         idut <= ( (v));
         bdut <= ( (v));
      endaction);


   
   rule showState (True);
      $display ("bdut = %d, idut = %d, udut = %d", bdut.isEqual(1), idut.isEqual(1), udut.isEqual(1));
   endrule
   
   let testSeq =
   (seq
       i <= 20;
       $display ("Incrementing by 1...........................");
       while (i != 0)
          action
             i <= i -1;
             incrAll(1);
          endaction
       i <= 20;
       $display ("Decrementing by 1...........................");
       writeAll(0);
       while (i != 0)
          action
             i <= i -1;
            decrAll(1);
          endaction
       $display ("Incr by 3 Decr by 1...........................");
       writeAll(0);
       i <= 20;
       while (i != 0)
          action
             i <= i -1;
            incrAll(3);
             decrAll(1);
          endaction
       $display ("Incr by 3 Decr by 1 update to 7 ...........................");
       writeAll(0);
       i <= 20;
       while (i != 0)
          action
             i <= i -1;
             incrAll(3);
             decrAll(1);
             updateAll(7);
          endaction
       $display ("Incr by 3 Decr by 1 update to 7 write to 4 ...........................");
       writeAll(0);
       i <= 20;
       while (i != 0)
          action
             i <= i -1;
             incrAll(3);
             decrAll(1);
             writeAll(4);
          endaction
    endseq);
   
   mkAutoFSM(testSeq);

endmodule
