package Head;

import Vector :: *;

function Action displayabc (a abc) provisos (Bits #(a, sa));
    action
      $display ("%d", abc);
    endaction
endfunction



function Action display_list (Vector #(n,a) my_list) provisos (Bits # (a,sa));
     action
       joinActions ( map (displayabc, my_list));
     endaction
endfunction



module mkTestbench_Head();
   Vector #(5,Int #(32)) my_list1 = cons (0, cons (1, cons (2, cons (3, cons (4, nil)))));


   rule fire_once (True);
      $display ("Head = %d", head(my_list1));
      if (head(my_list1) !=0) 
        $display ("Simulation Fails");
      else
        $display ("Simulation Passes");
	  $finish(2'b00);
   endrule 
      
endmodule : mkTestbench_Head
endpackage : Head
