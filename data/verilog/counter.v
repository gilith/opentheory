module compressor2(a,b,s,c);
   parameter width = 0;

   input [width-1:0] a;
   input [width-1:0] b;

   output [width-1:0] s;
   output [width-1:0] c;

   assign s = a ^ b;
   assign c = a & b;

endmodule // compressor2

module counter(clk,ld,nb,dn);
   parameter width = 0;

   input clk;
   input ld;
   input [width-1:0] nb;

   output dn;

   reg [width-2:0] sp;
   reg [width-1:0] cp;
   reg dp;

   wire [width-2:0] sq;
   wire [width-1:0] cq;
   wire dq;
   wire [width-2:0] sr;
   wire [width-1:0] cr;

   compressor2
     #(.width (width-1))
     advance
     (.a (sp),
      .b (cp[width-2:0]),
      .s (sq),
      .c (cq[width-1:1]));

   assign cq[0] = ~cp[0];
   assign dq = dp | cp[width-1];

   assign sr = ld ? nb[width-1:1] : sq;
   assign cr[0] = ld ? nb[0] : cq[0];
   assign cr[width-1:1] = ld ? {(width-1) {1'b0}} : cq[width-1:1];
   assign dn = ld ? 1'b0 : dq;

   always @(posedge clk)
     begin
        sp <= sr;
        cp <= cr;
        dp <= dn;
     end

endmodule // counter

module main;
   parameter delay = 10;
   parameter width = 5;

   reg clk;
   reg inp;
   reg [width-1:0] nb;

   wire out;

   counter
     #(.width (width))
     root
     (.clk (clk),
      .ld (inp),
      .nb (nb),
      .dn (out));

   initial
     begin
        $display("+------------------------+");
        $display("| Test bench for counter |");
        $display("+------------------------+");
        $monitor("ld = %b, sr = %d, cr = %d, dn = %b",
                  root.ld, root.sr, root.cr, root.dn);
        clk = 0;
        repeat(10) @(posedge clk);
        nb =  {width {1'b0}} - (delay + 1 - width);
        inp = 1;
        @(posedge clk);
        inp = 0;
        repeat(delay-1) @(posedge clk);
        if (out)
          begin
             $display("ERROR: counter finished too soon");
          end
        @(posedge clk);
        if (!out)
          begin
             $display("ERROR: counter did not finish on time");
          end
        repeat(5) @(posedge clk);
        $monitoroff;
        $display("Test complete at time %0t.", $time);
        $finish;
     end

   always
     #5 clk = !clk;

endmodule // main
