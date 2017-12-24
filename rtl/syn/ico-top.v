`default_nettype none

module top(
  input clk_100mhz,
  output TX,
  input RX,
  input S1,
  output cts_n,
  output [3:0] led,
  output pmod3_7, // hsync
  output pmod3_1, // vsync
  output pmod3_8, // R4
  output pmod3_9, // B4
  output pmod3_10,  // G3
  output pmod3_2, // G4
  output pmod3_3, // R3
  output pmod3_4);  // B3

wire clk_core;
wire pll_locked;
wire reset_n = ~S1;

        SB_PLL40_CORE #(
                .FEEDBACK_PATH("SIMPLE"),
                .DELAY_ADJUSTMENT_MODE_FEEDBACK("FIXED"),
                .DELAY_ADJUSTMENT_MODE_RELATIVE("FIXED"),
                .PLLOUT_SELECT("GENCLK"),
                .FDA_FEEDBACK(4'b1111),
                .FDA_RELATIVE(4'b1111),
                .DIVR(4),
                .DIVF(31),
                .DIVQ(4),
                .FILTER_RANGE(2),
        ) pll (
                .REFERENCECLK (clk_100mhz),
                .PLLOUTGLOBAL (clk_core  ),
                .LOCK         (pll_locked),
                .BYPASS       (1'b0      ),
                .RESETB       (reset_n   )
        );
        reg [3:0] pll_lock_window = 0;
	reg pll_stable = 0;

	always @(posedge clk_core or negedge reset_n) begin
          if (~reset_n) begin
              pll_stable <= 0;
              pll_lock_window <= 0;
          end else begin
		pll_stable <= &pll_lock_window;
		pll_lock_window <= {pll_lock_window, pll_locked};
          end
	end

        reg [7:0] reset_delay = 0;
        reg core_reset_n = 0;

        always @(posedge clk_core or negedge reset_n)
          if (~reset_n) begin
            core_reset_n <= 0;
            reset_delay <= 0;
          end else if (pll_stable) begin
            if (reset_delay == 128) core_reset_n <= 1;
            reset_delay <= reset_delay + 1;
          end

        wire [15:0] out1;
        wire [15:0] in;
        wire [5:0] vid;

        ico_soc _inst(
          .clk_core(clk_core),
          .reset(~core_reset_n),
          .out1(out1),
          .inport(in),
          .hsync(pmod3_7),
          .vsync(pmod3_1),
          .vid(vid),
        );

        assign led = out1[7:4];
        assign {cts_n, TX} = out1[1:0];
        assign in = {15'b0, RX};

        assign {pmod3_8, pmod3_2, pmod3_9, pmod3_3, pmod3_10, pmod3_4} = vid;
endmodule
