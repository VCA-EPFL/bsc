package Tee;

import RPush :: *;
import FIFO :: *;

function Bit #(16) square (Bit #(8) in_a);
    return zeroExtend (in_a) * zeroExtend (in_a);
endfunction : square


interface Design_IFC;
   method Action push (Bit #(8) in_a);
   method Action clr ();
   method ActionValue #(Bit #(16)) result();
endinterface :Design_IFC

module mkDesign_Tee (Design_IFC);

    function Action fdisplay (Bit #(8) in_a);
        $display ("Pushing Value =%d \n", in_a);
    endfunction : fdisplay

    FIFO #(Bit #(16)) fifo1();
    mkSizedFIFO #(16) the_fifo1(fifo1);

    RPush #(Bit #(16)) input_a = fifoToRPush (fifo1);
    RPush #(Bit #(8)) output_a = tee(fdisplay, apply (square, input_a));

    method push(in_a);
       action
          output_a.push(in_a);
       endaction
   endmethod: push

   method clr ();
       action
          output_a.clear();
       endaction
   endmethod : clr

    method result();
       actionvalue
           fifo1.deq;
           return fifo1.first;
       endactionvalue
    endmethod: result

endmodule : mkDesign_Tee


module mkTestbench_Tee ();
    Design_IFC dut();
    mkDesign_Tee the_dut (dut);

    Reg #(Bit #(8)) counter();
    mkReg #(0) the_counter (counter);

    Reg #(Bool) fail();
    mkReg #(False) the_fail (fail);

    rule always_fire (True);
        counter <= counter + 1;
    endrule

    rule push_values (counter < 20);   //Push 20 Values
        if (counter == 5 || counter == 10 || counter == 15)
        begin
           dut.clr();
           $display ("Cycle Number = %d\nClearing Buffer\n", counter);
        end
        else
        begin
           $display ("Cycle Number = %d", counter);
           dut.push (counter);
        end
    endrule


    rule pop_values (counter !=5 && counter !=6 && counter !=8 );   // 5 values lost 4,5,9,10,15
        Bit #(16) result <- dut.result;
        $display ("Cycle Number = %d\nPopped Out Value = %d\n", counter, result);
        if (counter <=8 && result != square(counter-1))
            fail <= True;
        else if (counter > 8 && counter <= 10 && result != square(counter -2))
            fail <= True;
        else if (counter> 10 && result != square(counter -1))
            fail <= True;
    endrule

    rule endsim (counter == 8'd25);
        if (fail )
           $display("Simulation Fails");
        else
           $display("Simulation Passes");
        $finish(2'b00);
    endrule
endmodule : mkTestbench_Tee

endpackage : Tee
