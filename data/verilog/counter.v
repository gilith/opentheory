module twoToTwo(a,b,s,c);
   input [0:31] a;
   input [0:31] b;
   output [0:31] s;
   output [0:31] c;

   s = a ^ b;
   c = a & b;

endmodule // twoToTwo

module counter(clk,ld,nb,dn);

   input clk;
   input ld;
   input [0:32] nb;
   
   output       dn;

   reg [0:31] sr;
   reg [0:32] cr;
   reg        dr;

   wire [0:31] sq;
   wire [0:32] cq;
   wire        dq;

   twoToTwo
   always @(posedge clk)
     if (ld)
       begin
          dn <= 1'b0;
          sr <= 
     dn <= dr | cr[32];
   

           endmodule; // counter


module main;


   reg clk, reset;

   reg [31:0] value;

   wire [15:0] result;

   wire        rdy;


   sqrt32 root(.clk(clk), .rdy(rdy), .reset(reset), .x(value), .y(result));


   always #5 clk = ~clk;


      always @(posedge rdy) begin
         $display("sqrt(%d) --> %d", value, result);

         $finish;

      end


      initial begin
         clk = 0;

         reset = 1;

         $monitor($time,,"%m.acc = %b", root.acc);

         #100 value = 63;

         reset = 0;

      end // initial begin
endmodule /* main */
