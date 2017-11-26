`default_nettype none

// `define GO_SLOW

module top(
  input clk_12mhz,
  output TX,
  output [4:0] led);

wire clk_core;
wire pll_locked;

`ifndef GO_SLOW
        SB_PLL40_CORE #(
                .FEEDBACK_PATH("SIMPLE"),
                .DELAY_ADJUSTMENT_MODE_FEEDBACK("FIXED"),
                .DELAY_ADJUSTMENT_MODE_RELATIVE("FIXED"),
                .PLLOUT_SELECT("GENCLK"),
                .FDA_FEEDBACK(4'b1111),
                .FDA_RELATIVE(4'b1111),
                .DIVR(0),
                .DIVF(52),
                .DIVQ(4),
                .FILTER_RANGE(1),
        ) pll (
                .REFERENCECLK (clk_12mhz),
                .PLLOUTGLOBAL (clk_core  ),
                .LOCK         (pll_locked),
                .BYPASS       (1'b0      ),
                .RESETB       (1'b1      )
        );
`else
        assign clk_core = clk_12mhz;
        assign pll_locked = 1;
`endif
        reg [3:0] pll_lock_window = 0;
	reg pll_stable = 0;

	always @(posedge clk_core) begin
		pll_stable <= &pll_lock_window;
		pll_lock_window <= {pll_lock_window, pll_locked};
	end

        reg [7:0] reset_delay = 0;
        reg resetn = 0;

        always @(posedge clk_core)
          if (pll_stable) begin
            if (reset_delay == 128) resetn <= 1;
            reset_delay <= reset_delay + 1;
          end

        wire [15:0] out;

        CFMTop_topEntity _inst(
          .c(clk_core),
          .r(~resetn),
          .result(out),
        );

        assign led = out[15:11];
        assign TX = out[0];

endmodule
