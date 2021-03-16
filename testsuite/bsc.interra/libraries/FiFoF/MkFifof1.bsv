package MkFifof1;

import FIFOF::*;

(* synthesize *)
module mkDesign_MkFifof1 (FIFOF #(Bit #(8)));
  FIFOF#(Bit#(8)) datafifo <- mkFIFOF1() ;
  return (datafifo);
endmodule

module mkTestbench_MkFifof1 ();

  Reg#(Bit#(8)) sizeoflist <- mkReg(0);
  FIFOF#(Bit#(8)) datafifo <- mkDesign_MkFifof1() ;
  Reg#(Bit#(8)) counter <- mkReg(0);
  Reg#(Bool) fail <- mkReg(False);

  rule always_fire (True);
	 counter <= counter + 1;
  endrule

  rule data_write ((datafifo.notFull) && (counter < 20));
     datafifo.enq(8'h55);
	 $display("Cycle Number =%d, Value written = 55, Full=%d, Empty =%d", counter, !datafifo.notFull, !datafifo.notEmpty);
  endrule

  rule read_value ((datafifo.notEmpty) && ((counter > 20) && (counter < 40)));
	 $display("Cycle Number =%d, Value read = %h, Full =%d, Empty =%d",counter,datafifo.first, !datafifo.notFull, !datafifo.notEmpty);
	 if (datafifo.first != 8'h55)
	   fail <= True;
	 datafifo.deq;
  endrule

  rule data_write1 ((datafifo.notFull) &&  ((counter > 40) && (counter < 60)));
     datafifo.enq(counter+55);
	 $display("Cycle Number =%d, Value Written = %h, Full =%d, Empty =%d",counter,counter+55, !datafifo.notFull, !datafifo.notEmpty);
  endrule

  rule clear_fifo (counter == 60);
	 datafifo.clear;
	 $display("Cycle Number =%d, Clearing FIFO",counter);
  endrule

  rule data_write2 ((datafifo.notFull) &&  ((counter > 61) && (counter < 80)));
     datafifo.enq(8'haa);
	 $display("Cycle Number =%d, Value Written = %h, Full =%d, Empty =%d",counter,8'haa, !datafifo.notFull, !datafifo.notEmpty);
  endrule

  rule read_value2 ((datafifo.notEmpty) && ((counter > 80) && (counter < 100)));
	 $display("Cycle Number =%d, Value read = %h, Full =%d, Empty =%d",counter,datafifo.first, !datafifo.notFull, !datafifo.notEmpty);
	 if (datafifo.first != 8'haa)
	   fail <= True;
	 datafifo.deq;
  endrule

  rule endofsim (counter == 100);
	if (fail)
	  $display("Simulation Fails");
	else
	  $display("Simulation Passes");
	$finish(2'b00);
  endrule

endmodule: mkTestbench_MkFifof1
endpackage: MkFifof1
