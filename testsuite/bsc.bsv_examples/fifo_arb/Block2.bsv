import FIFO::*;
import FIFOF::*;
import Includes::*;

// Second version of FIFO arbiter - does naive round-robin selection

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
(* synthesize *)
module mkBlock2( LabIFC );
   Integer fifo_depth = 16;
  
   // modify data (by adding AA to certain digits)
   // to show it went through the arbiter
   function DataT generate_new_data(DataT val);
      return val + zeroExtend(24'hAA000);
   endfunction

   FIFOF#(DataT) inbound1();
   mkSizedFIFOF#(fifo_depth) the_inbound1(inbound1);
   
   FIFOF#(DataT) inbound2();
   mkSizedFIFOF#(fifo_depth) the_inbound2(inbound2);

   FIFOF#(DataT) outbound1();
   mkSizedFIFOF#(fifo_depth) the_outbound1(outbound1);

   // do a simple round robin selection 
   // checking each input FIFO on alternate cycles
   Reg#(Bool) roundRobin <- mkReg(True);

   Bool rule1_can_fire = (roundRobin==True);
   Bool rule2_can_fire = (roundRobin==False);

   rule flip (True);
      roundRobin <= !roundRobin;
   endrule

   // rule for enqueue of outbound from inbound1
   rule enq1 (rule1_can_fire);
      DataT in_data = inbound1.first;
      DataT out_data = generate_new_data(in_data);
      outbound1.enq(out_data); 
      inbound1.deq;
   endrule: enq1

   // rule for enqueue of outbound from inbound2
   rule enq2 (rule2_can_fire);
      DataT in_data = inbound2.first;
      DataT out_data = generate_new_data(in_data);
      outbound1.enq(out_data); 
      inbound2.deq;
   endrule: enq2

   method Action push1 (DataT a); 
      action
         inbound1.enq(a);
      endaction
   endmethod

   method Action push2 (DataT a);
      action
         inbound2.enq(a);
      endaction
   endmethod
   
   method get(); 
      actionvalue
         outbound1.deq();
         return outbound1.first();
      endactionvalue
   endmethod

endmodule

