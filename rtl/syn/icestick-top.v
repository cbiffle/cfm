`default_nettype none

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
    .PLLOUTGLOBAL (clk_core),
    .LOCK         (pll_locked),
    .BYPASS       (1'b0),
    .RESETB       (1'b1)
  );

  reg [3:0] pll_lock_window = 0;
  reg pll_stable = 0;
  // The terminal can reset us by "going away," thus the polarity here is
  // right.
  wire ext_reset = dtr_n;

  always @(posedge clk_core) begin
    pll_stable <= &pll_lock_window;
    pll_lock_window <= {pll_lock_window, pll_locked};
  end

  reg [7:0] reset_delay = 0;
  reg core_reset_n = &reset_delay;

  always @(posedge clk_core)
    if (~pll_stable || ext_reset) begin
      reset_delay <= 0;
    end else if (~&reset_delay) begin
      reset_delay <= reset_delay + 1;
    end

  reg RX_ = 1;
  always @(posedge clk_core)
    if (~core_reset_n) begin
      RX_ <= 1;
    end else begin
      RX_ <= RX;
    end

  wire [15:0] out1;
  wire [15:0] in;

  icestick_soc _inst(
    .clk_core(clk_core),
    .reset(~core_reset_n),
    .out1(out1),
    .inport(in),
    .uart_tx(TX),
    .uart_rx(RX_),
  );

  assign led = {core_reset_n, out1[4:1]};
  assign cts_n = out1[0];
  assign in = 16'b0;
  assign dcd_n = ~core_reset_n; // indicate reset status back to terminal
endmodule
