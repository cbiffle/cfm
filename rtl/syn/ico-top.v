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
  output [4:3] vga_g,
  output sd_cs_n,
  output sd_mosi,
  input sd_miso,
  output sd_sck,
  input sd_cd,
  output [18:0] sram_a,
  inout [15:0] sram_d,
  output sram_ce_n,
  output sram_we_n,
  output sram_oe_n,
  output sram_lb_n,
  output sram_ub_n,

  output pmod4_1,
  );

wire clk_core, clk_core90;
wire pll_locked;
wire reset_n = ~S1;

        SB_PLL40_2F_CORE #(
                .FEEDBACK_PATH("PHASE_AND_DELAY"),
                .DELAY_ADJUSTMENT_MODE_FEEDBACK("FIXED"),
                .DELAY_ADJUSTMENT_MODE_RELATIVE("FIXED"),
                .PLLOUT_SELECT_PORTA("SHIFTREG_0deg"),
                .PLLOUT_SELECT_PORTB("SHIFTREG_90deg"),
                .SHIFTREG_DIV_MODE(0),
                .FDA_FEEDBACK(4'b1111),
                .FDA_RELATIVE(4'b1111),
                .DIVR(4),
                .DIVF(1),
                .DIVQ(4),
                .FILTER_RANGE(2),
        ) pll (
                .REFERENCECLK (clk_100mhz),
                .PLLOUTGLOBALA(clk_core  ),
                .PLLOUTGLOBALB(clk_core90),
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

        wire sram_wr;
        wire [15:0] host_to_sram;
        wire [15:0] sram_to_host;
        SB_IO #(
          .PIN_TYPE(6'b1010_01),
          .PULLUP(0)
        ) sram_io [15:0] (
          .PACKAGE_PIN(sram_d),
          .OUTPUT_ENABLE(sram_wr && clk_core90),
          .D_OUT_0(host_to_sram),
          .D_IN_0(sram_to_host),
        );

        assign sram_we_n = !(sram_wr && clk_core90);
        assign sram_oe_n = sram_wr;
        assign sram_ce_n = 1;  // TODO
        assign sram_lb_n = sram_we_n;
        assign sram_ub_n = sram_we_n;

        wire [15:0] out1;
        wire [15:0] in;
        wire [5:0] vid;

        ico_soc _inst(
          .clk_core(clk_core),
          .reset(~core_reset_n),
          .out1(out1),
          .inport(in),
          .sram_to_host(sram_to_host),
          .hsync(vga_hsync),
          .vsync(vga_vsync),
          .vid(vid),
          .sram_a(sram_a[12:0]),
          .sram_wr(sram_wr),
          .host_to_sram(host_to_sram),

          .uart_tx(pmod4_1),
        );

        assign sram_a[18:13] = 0;
        assign led = out1[8:5];
        assign {cts_n, TX} = out1[1:0];
        assign {sd_cs_n, sd_mosi, sd_sck} = out1[4:2];
        assign in = {sd_cd, sd_miso, RX};

        assign {vga_r[4], vga_g[4], vga_b[4], vga_r[3], vga_g[3], vga_b[3]}
          = vid;
endmodule
