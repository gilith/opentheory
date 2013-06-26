module twoToTwo(a,b,s,c);
   input [31:0] a;
   input [31:0] b;
   output [31:0] s;
   output [31:0] c;

   assign s = a ^ b;
   assign c = a & b;

endmodule // twoToTwo

module main;
   reg [31:0] inp1;
   reg [31:0] inp2;
   wire [31:0] out1;
   wire [31:0] out2;

   twoToTwo root(.a(inp1), .b(inp2), .s(out1), .c(out2));

   initial
     begin
        #100 inp1 = 13;
        inp2 = 8;
     end

   always
     begin
        #200 $display("twoToTwo(%d,%d) --> (%d,%d)", inp1, inp2, out1, out2);
        $finish;
     end

endmodule /* main */
