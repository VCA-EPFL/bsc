module Tb();

 wire x,y;
 wire [5 - 1:0] result;
 wire [5 - 1:0] check;
 assign x = 1'b1;

 mkDesign_01 m1(.CLK(x),
		        .RST_N(x),
		        .start_a( {5{x}}),
		        .start_b( {5{x}}),
		        .EN_start(x),
		        .RDY_start(y),
		   	    .resresult(result),
		        .RDY_result(y),
		        .EN_check(x),
		        .chresult(check),
		        .RDY_check(y)
				);

endmodule				
