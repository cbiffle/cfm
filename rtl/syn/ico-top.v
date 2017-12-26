`default_nettype none

module top(
  input clk_100mhz,
  output TX,
  input RX,
  input S1,
  output cts_n,
  output [3:0] led,
  output vga_hsync,
  output vga_vsync,
  output [4:3] vga_r,
  output [4:3] vga_b,
  output [4:3] vga_g);

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
          .hsync(vga_hsync),
          .vsync(vga_vsync),
          .vid(vid),
        );

        assign led = out1[7:4];
        assign {cts_n, TX} = out1[1:0];
        assign in = {15'b0, RX};

        assign {vga_r[4], vga_g[4], vga_b[4], vga_r[3], vga_g[3], vga_b[3]}
          = vid;
endmodule
