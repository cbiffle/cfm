`default_nettype none

// `define GO_SLOW

module top(
  input clk_12mhz,
  output TX,
  input RX,
  input dtr_n,
  output dcd_n,
  output cts_n,
  output [4:0] led);

wire clk_core;
wire pll_locked;

wire dtr = ~dtr_n;

`ifndef GO_SLOW
        SB_PLL40_CORE #(
                .FEEDBACK_PATH("SIMPLE"),
                .DELAY_ADJUSTMENT_MODE_FEEDBACK("FIXED"),
                .DELAY_ADJUSTMENT_MODE_RELATIVE("FIXED"),
                .PLLOUT_SELECT("GENCLK"),
                .FDA_FEEDBACK(4'b1111),
                .FDA_RELATIVE(4'b1111),
                .DIVR(0),
                .DIVF(63),
                .DIVQ(4),
                .FILTER_RANGE(1),
        ) pll (
                .REFERENCECLK (clk_12mhz),
                .PLLOUTGLOBAL (clk_core  ),
                .LOCK         (pll_locked),
                .BYPASS       (1'b0      ),
                .RESETB       (dtr       )
        );
`else
        assign clk_core = clk_12mhz;
        assign pll_locked = 1;
`endif
        reg [3:0] pll_lock_window = 0;
	reg pll_stable = 0;

	always @(posedge clk_core or negedge dtr) begin
          if (~dtr) begin
              pll_stable <= 0;
              pll_lock_window <= 0;
          end else begin
		pll_stable <= &pll_lock_window;
		pll_lock_window <= {pll_lock_window, pll_locked};
          end
	end

        reg [7:0] reset_delay = 0;
        reg core_reset_n = 0;

        always @(posedge clk_core or negedge dtr)
          if (~dtr) begin
            core_reset_n <= 0;
            reset_delay <= 0;
          end else if (pll_stable) begin
            if (reset_delay == 128) core_reset_n <= 1;
            reset_delay <= reset_delay + 1;
          end

        wire [15:0] out1;
        wire [15:0] out2;
        wire [15:0] in;

        cfm_demo_top _inst(
          .clk_core(clk_core),
          .reset(~core_reset_n),
          .out1(out1),
          .out2(out2),
          .inport(in),
        );

        assign led = {dtr, out2[3:0]};
        assign {cts_n, TX} = out1[1:0];
        assign in = {15'b0, RX};
        assign dcd_n = ~core_reset_n;
endmodule
