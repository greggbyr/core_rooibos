module tlul_mon_wrap import tlul_pkg::*; #(
  parameter logic [31:0] SRAM_BASE  = 32'h1000_0000,
  parameter logic [31:0] SRAM_MASK  = 32'hFFF0_0000,
  parameter logic [31:0] FLASH_BASE = 32'h2000_0000,
  parameter logic [31:0] FLASH_MASK = 32'hFFF0_0000,
  parameter logic [31:0] ROM_BASE  = 32'h0000_0000,
  parameter logic [31:0] ROM_MASK  = 32'hFFFF_0000
) (
  input  logic                 clk_i,
  input  logic                 rst_ni,

  // TL-UL struct ports to tap
  input  tl_h2d_t      sram_tl_h2d_i, // host -> device (A-channel)
  input  tl_d2h_t      sram_tl_d2h_i, // device -> host (A-ready, D-channel)
  input  tl_h2d_t      flash_tl_h2d_i,
  input  tl_d2h_t      flash_tl_d2h_i,

  // Retire pulse (1 cycle per retired instruction); see §3 for options
  input  logic                 retire_pulse_i,

  // (Optional) I-fetch TL-UL tap (read-only)
  input  tl_h2d_t      ifetch_tl_h2d_i,
  input  tl_d2h_t      ifetch_tl_d2h_i,

  // Expose counters for Verilator (optional; can be left unconnected)
  output logic [63:0]          minstret_o,
  output logic [63:0]          load_cnt_sram_o,
  output logic [63:0]          store_cnt_sram_o,
  output logic [63:0]          load_cnt_flash_o,
  output logic [63:0]          store_cnt_flash_o,
  output logic [63:0]          store_bytes_total_o,
  output logic [63:0]          dead_store_bytes_o
);
  import top_earlgrey_pkg::*;

  // Map TL-UL fields into scalar signals for our existing monitor
  logic         a_valid;
  logic [2:0]   a_opcode;
  logic [31:0]  a_address;
  logic [2:0]   a_size;
  logic [3:0]   a_mask;
  logic         a_ready;

  // Dead bytes
  logic [63:0] sram_dead_bytes, flash_dead_bytes;

  // Instruction fetch counters
  logic [63:0] ifetch_rom_loads, ifetch_flash_loads;
  logic [63:0] ifetch_rom_bytes, ifetch_flash_bytes;

  // Bytes we attribute to *alias collisions* (separate from true dead bytes)
  logic [63:0] sram_alias_dead_bytes, flash_alias_dead_bytes;
  logic [63:0] alias_dead_bytes_total; // convenience sum for printing

  // print and kill run manually as opentitantool kills on PASS
  integer tlmon_stop_after_instr = 0;
  integer tlmon_stop_at_cycle    = 0;

  // minstret check must be a plus args to work the display
  integer minstret_check         = 0;

  // Direct-mapped table vars for tracking dead bytes
  // Fixed maximum (compile-time)
  parameter int MAX_LGM = 12; // 4K lines max

  // Runtime effective size (set by plusarg), 1..MAX_LGM
  int unsigned lgm_eff;   // number of index bits actually used

  assign a_valid   = sram_tl_h2d_i.a_valid;
  assign a_opcode  = sram_tl_h2d_i.a_opcode;
  assign a_address = sram_tl_h2d_i.a_address;
  assign a_size    = sram_tl_h2d_i.a_size;
  assign a_mask    = sram_tl_h2d_i.a_mask;
  assign a_ready   = sram_tl_d2h_i.a_ready; // ready originates from device back to host

  // For access detection
  wire a_hs_sram  = sram_tl_h2d_i.a_valid  && sram_tl_d2h_i.a_ready;
  wire a_hs_flash = flash_tl_h2d_i.a_valid && flash_tl_d2h_i.a_ready;
  wire if_a_hs = ifetch_tl_h2d_i.a_valid && ifetch_tl_d2h_i.a_ready;

  // Byte-enable width
  localparam int DBW = top_pkg::TL_DBW;

  // Per-target counters
  logic [63:0] sram_loads, sram_stores, sram_bytes_wr, flash_loads, flash_stores, flash_bytes_wr;


  // --------------------- INITIAL BLOCKS ----------------------------

  initial begin
    int unsigned tmp;
    lgm_eff = MAX_LGM; // default
    if ($value$plusargs("TLMON_LGM=%d", tmp)) begin
      if (tmp == 0)        lgm_eff = 1;
      else if (tmp > MAX_LGM) lgm_eff = MAX_LGM;
      else                 lgm_eff = tmp;
    end

    // init message to verify monitor is actually coming up
    $display("TLMon wrap elaborated at %m");

    // Read optional plusargs
    void'($value$plusargs("TLMON_STOP_AFTER=%d", tlmon_stop_after_instr));
    void'($value$plusargs("TLMON_STOP_AT_CYC=%d", tlmon_stop_at_cycle));
    void'($value$plusargs("MINSTRET_CHECK=%d", minstret_check));
  end


  // ---------------------- FUNCTIONS ---------------------------------
  // Treat MASK as a region mask: 1-bits select the top bits to compare.
  function automatic bit addr_match(input logic [31:0] a,
                                    input logic [31:0] base,
                                    input logic [31:0] mask);
    return ((a & mask) == (base & mask));
  endfunction

  function automatic bit addr_in_sram(input logic [31:0] a);
    return addr_match(a, SRAM_BASE,  SRAM_MASK);
  endfunction

  function automatic bit addr_in_flash_data(input logic [31:0] a);
    // IMPORTANT: This is the executable/data window (e.g., 0x2000_0000…).
    // It deliberately excludes the flash controller MMIO window.
    return addr_match(a, FLASH_BASE, FLASH_MASK);
  endfunction

  function automatic bit in_region(input logic [31:0] addr,
                                  input logic [31:0] base,
                                  input logic [31:0] mask);
    return ((addr & mask) == (base & mask));
  endfunction

  function automatic bit addr_is_ifetch_mem(input logic [31:0] a);
    // Count only ROM or FLASH code fetches; exclude MMIO
    return in_region(a, ROM_BASE,  ROM_MASK) ||
          in_region(a, FLASH_BASE, FLASH_MASK);
  endfunction

  // Generic accessor detectors
  function automatic bit is_get  (tl_h2d_t a); return (a.a_opcode == Get); endfunction
  function automatic bit is_put  (tl_h2d_t a); return (a.a_opcode inside {PutFullData, PutPartialData}); endfunction
  function automatic bit is_grant(tl_d2h_t d); return d.d_valid; endfunction

  // Byte enable masking for loads
  function automatic logic [3:0] get_be(input logic [1:0] addr_lo, input logic [2:0] size);
    // size = 0->1B, 1->2B, 2->4B (clamp above 2 to 4B)
    logic [2:0] s = (size > 2) ? 3'(2) : size;
    logic [3:0] base = (4'(1) << (1 << s)) - 1; // 1,3,15
    get_be = base << addr_lo;
  endfunction


  // ----------------------------- TASKS -----------------------------
  task automatic tlmon_print();
    // ---- Declarations must come first in the block ----
    real A_data, A_ifetch, A_total, Bbits, alias_ratio;
    real ifetch_rom_bytes_pi, ifetch_flash_bytes_pi, ifetch_tot_bytes_pi, alias_rate;
    longint unsigned data_accesses;
    longint unsigned ifetch_accesses;
    longint unsigned ifetch_bytes;
    longint unsigned total_accesses;
    longint unsigned total_dead_bytes;

    // ---- Compute aggregates ----
    
    data_accesses           = sram_loads + sram_stores + flash_loads + flash_stores;
    ifetch_accesses         = ifetch_rom_loads + ifetch_flash_loads;
    ifetch_bytes            = ifetch_rom_bytes + ifetch_flash_bytes;
    total_accesses          = data_accesses + ifetch_accesses; // ok if 0 when ifetch port unconnected
    total_dead_bytes        = sram_dead_bytes + flash_dead_bytes;
    alias_dead_bytes_total  = sram_alias_dead_bytes + flash_alias_dead_bytes;

    if (minstret != 0) begin
      A_data      = real'(data_accesses) / real'(minstret);
      A_ifetch    = real'(ifetch_accesses) / real'(minstret);
      A_total     = real'(total_accesses) / real'(minstret);
      Bbits = 8.0 * real'(total_dead_bytes) / real'(minstret);
    end else begin
      A_data = 0.0; 
      A_ifetch=0.0; 
      A_total= 0.0; 
      Bbits = 0.0;
    end

    alias_ratio = (total_dead_bytes != 0)
                  ? (real'(alias_dead_bytes_total) / real'(total_dead_bytes))
                  : 0.0;

    // ---- Prints ----
    $display("--- TLMon summary ---");
    $display("minstret=%0d", minstret);
    $display("SRAM: loads=%0d stores=%0d bytes_wr=%0d dead_bytes=%0d",
            sram_loads, sram_stores, sram_bytes_wr, sram_dead_bytes);
    $display("FLASH: loads=%0d stores=%0d bytes_wr=%0d dead_bytes=%0d",
            flash_loads, flash_stores, flash_bytes_wr, flash_dead_bytes);

    // IFetch-specific ROM vs Flash breakdown
    ifetch_rom_bytes_pi = (minstret != 0) ? real'(ifetch_rom_bytes)/real'(minstret) : 0.0;
    $display("IFETCH (ROM): loads=%0d, bytes=%0d, bytes/inst=%0.6f", ifetch_rom_loads, ifetch_rom_bytes, ifetch_rom_bytes_pi);

    ifetch_flash_bytes_pi = (minstret != 0) ? real'(ifetch_flash_bytes)/real'(minstret) : 0.0;
    $display("IFETCH (Flash): loads=%0d, bytes=%0d, bytes/inst=%0.6f", ifetch_flash_loads, ifetch_flash_bytes, ifetch_flash_bytes_pi);
    
    ifetch_tot_bytes_pi = (minstret != 0) ? (real'(ifetch_accesses)+real'(ifetch_accesses))/real'(minstret) : 0.0;
    $display("IFETCH (Total): loads=%0d, bytes=%0d, bytes/inst=%0.6f", ifetch_accesses, ifetch_bytes, ifetch_tot_bytes_pi);

    $display("Implied instrs per fetch ≈ %0.3f",
            (ifetch_accesses != 0) ? real'(minstret)/real'(ifetch_accesses) : 0.0);
    
    // Aliasing data
    $display("ALIAS: sram_aliasB=%0d flash_aliasB=%0d total_aliasB=%0d",
            sram_alias_dead_bytes, flash_alias_dead_bytes, alias_dead_bytes_total);

    $display("A_data (data accesses/inst) = %0.6f", A_data);
    $display("A_ifetch (i-fetches/inst)   = %0.6f", A_ifetch);
    $display("A_total                      = %0.6f", A_total);
    $display("B (bits erased/inst, approx) = %0.6f", Bbits);
    $display("Alias fraction of dead bytes (est.) = %0.6f", alias_ratio);
    alias_rate  = ((sram_stores + flash_stores) != 0)
                ? real'(alias_dead_bytes_total) / real'(sram_stores + flash_stores) : 0.0;
    $display("Alias rate (Tot. alias dead bytes/ Tot. stores) = %0.6f", alias_rate);


    // CSV (keep fields stable for parsing)
    $display("TLMON_CSV,minstret=%0d,sram_ld=%0d,sram_st=%0d,sram_wrB=%0d,sram_deadB=%0d,flash_ld=%0d,flash_st=%0d,flash_wrB=%0d,flash_deadB=%0d,ifetch_rom_ld=%0d,ifetch_rom_byte=%0d,ifetch_rom_bpi=%0.6f,ifetch_flash_ld=%0d,ifetch_flash_byte=%0d,ifetch_flash_bpi=%0.6f,ifetch_tot_ld=%0d,ifetch_tot_byte=%0d,ifetch_tot_bpi=%0.6f,A_data=%0.6f,A_ifetch=%0.6f,A_total=%0.6f,Bbits=%0.6f,aliasB=%0d,aliasRatio=%0.6f,aliasRate=%0.6f",
            minstret, sram_loads, sram_stores, sram_bytes_wr, sram_dead_bytes,
            flash_loads, flash_stores, flash_bytes_wr, flash_dead_bytes,
            ifetch_rom_loads, ifetch_rom_bytes, ifetch_rom_bytes_pi, 
            ifetch_flash_loads, ifetch_flash_bytes, ifetch_flash_bytes_pi, 
            ifetch_accesses, ifetch_bytes, ifetch_tot_bytes_pi, 
            A_data, A_ifetch, A_total, Bbits, alias_dead_bytes_total, alias_ratio, alias_rate);

    $fflush();
  endtask

  // ----------------------- MONITOR COUNTERS -------------------------------

  // Cycle Counter for help with printing status
  longint unsigned cycle_count;
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) cycle_count <= 0;
    else         cycle_count <= cycle_count + 1;
  end

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      sram_loads <= '0; sram_stores <= '0; sram_bytes_wr <= '0;
      flash_loads <= '0; flash_stores <= '0; flash_bytes_wr <= '0;
      ifetch_rom_loads <= '0; ifetch_rom_bytes <= '0;
      ifetch_flash_loads <= '0; ifetch_flash_bytes <= '0;
    end else begin
      // ---- SRAM window only ----
      if (a_hs_sram && addr_in_sram(sram_tl_h2d_i.a_address)) begin
        if (is_get(sram_tl_h2d_i)) begin
          sram_loads <= sram_loads + 1;
        end
        if (is_put(sram_tl_h2d_i)) begin
          sram_stores   <= sram_stores + 1;
          sram_bytes_wr <= sram_bytes_wr + $countones(sram_tl_h2d_i.a_mask);
        end
      end

      // ---- FLASH *data* window only (exclude controller MMIO) ----
      if (a_hs_flash && addr_in_flash_data(flash_tl_h2d_i.a_address)) begin
        if (is_get(flash_tl_h2d_i)) begin
          flash_loads <= flash_loads + 1;
        end
        if (is_put(flash_tl_h2d_i)) begin
          flash_stores   <= flash_stores + 1;
          flash_bytes_wr <= flash_bytes_wr + $countones(flash_tl_h2d_i.a_mask);
        end
      end

      // ---- IFETCH Window Only ----
      if (if_a_hs && is_get(ifetch_tl_h2d_i) &&
          addr_is_ifetch_mem(ifetch_tl_h2d_i.a_address)) begin
        // breakdown by rom and flash
        // TL-UL size encodes #bytes = 1 << a_size
        if (in_region(ifetch_tl_h2d_i.a_address, ROM_BASE, ROM_MASK)) begin
          ifetch_rom_loads  <= ifetch_rom_loads + 1;
          ifetch_rom_bytes  <= ifetch_rom_bytes + (64'(1) << ifetch_tl_h2d_i.a_size);
        end else if (in_region(ifetch_tl_h2d_i.a_address, ROM_BASE, ROM_MASK)) begin
          ifetch_flash_loads  <= ifetch_flash_loads + 1;
          ifetch_flash_bytes  <= ifetch_flash_bytes + (64'(1) << ifetch_tl_h2d_i.a_size);
        end
      end
    end
  end

  // Count the number of retired instructions:
  logic [63:0] minstret;

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin 
      minstret <= '0;
    end else if (retire_pulse_i) begin 
      minstret <= minstret + 1;
      
      // Only print out the minstret value on initial run to determine the total count to finish at. 
      // Then re-run with +TLMON_STOP_AFTER=<count>
      // Subsequent users, this should be available for in separate project README or as a sourced command.
      if (minstret_check) begin
        $display("minstret=%0d", minstret);
      end
    end

    // Choose a condition (first one that is set wins)
    if (tlmon_stop_after_instr > 0 && minstret >= tlmon_stop_after_instr) begin
      #1 $finish;
    end else if (tlmon_stop_at_cycle > 0 && cycle_count >= tlmon_stop_at_cycle) begin
      #1 $finish;
    end
  end

  // Index helpers
  function automatic logic [MAX_LGM-1:0] raw_idx(input logic [31:0] a);
    // word address then take MAX_LGM bits
    return a[ (MAX_LGM+1) : 2 ];
  endfunction

  function automatic logic [MAX_LGM-1:0] eff_idx(input logic [31:0] a, input int unsigned eff);
    logic [MAX_LGM-1:0] i = raw_idx(a);
    logic [MAX_LGM-1:0] mask;
    // Build mask = (1<<eff)-1 safely
    if (eff >= MAX_LGM) mask = {MAX_LGM{1'b1}};
    else                mask = (1 << eff) - 1;
    return i & mask;
  endfunction

  function automatic logic [31:0] line_base(input logic [31:0] a, input int unsigned eff);
    logic [31:0] low_mask;
    int sh = eff + 2; // +2 for byte-in-word
    if (sh >= 31) low_mask = 32'hFFFF_FFFC; // conservatively zero LSBs
    else          low_mask = (32'h1 << sh) - 1;
    return a & ~low_mask;
  endfunction

  typedef struct packed {
    logic       valid;
    logic [31:0] base;   // line base address with low (eff+2) bits zeroed
  } meta_t;

  meta_t sram_meta [2**MAX_LGM];
  meta_t flash_meta[2**MAX_LGM];

  // Reset
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      sram_dead_bytes <= '0; flash_dead_bytes <= '0;
      sram_alias_dead_bytes <= '0; flash_alias_dead_bytes <= '0;
      for (int i=0;i<2**MAX_LGM;i++) begin
        sram_meta[i].valid  = 1'b0; sram_meta[i].base  = '0;
        flash_meta[i].valid = 1'b0; flash_meta[i].base = '0;
      end
    end else begin
      alias_dead_bytes_total = sram_alias_dead_bytes + flash_alias_dead_bytes;

      // --- SRAM READ: observation clears "new" ---
      if (is_get(sram_tl_h2d_i) && sram_tl_d2h_i.a_ready) begin
        logic [31:0] ra  = {sram_tl_h2d_i.a_address[31:2], 2'b00};
        sram_meta[ eff_idx(ra, lgm_eff) ].valid = 1'b0;
      end

      // --- SRAM WRITE: dead vs alias ---
      if (is_put(sram_tl_h2d_i) && sram_tl_d2h_i.a_ready) begin
        logic [31:0] wa   = {sram_tl_h2d_i.a_address[31:2], 2'b00};
        automatic int wB  = $countones(sram_tl_h2d_i.a_mask);
        logic [MAX_LGM-1:0] i  = eff_idx(wa, lgm_eff);
        logic [31:0]        b  = line_base(wa, lgm_eff);

        if (sram_meta[i].valid) begin
          if (sram_meta[i].base == b) sram_dead_bytes        <= sram_dead_bytes + wB;
          else                        sram_alias_dead_bytes  <= sram_alias_dead_bytes + wB;
        end
        sram_meta[i].valid <= 1'b1;
        sram_meta[i].base  <= b;

        sram_stores   <= sram_stores + 1;
        sram_bytes_wr <= sram_bytes_wr + wB;
      end

      // --- FLASH READ ---
      if (is_get(flash_tl_h2d_i) && flash_tl_d2h_i.a_ready) begin
        logic [31:0] ra  = {flash_tl_h2d_i.a_address[31:2], 2'b00};
        flash_meta[ eff_idx(ra, lgm_eff) ].valid = 1'b0;
      end

      // --- FLASH WRITE (if present) ---
      if (is_put(flash_tl_h2d_i) && flash_tl_d2h_i.a_ready) begin
        logic [31:0] wa   = {flash_tl_h2d_i.a_address[31:2], 2'b00};
        automatic int wB  = $countones(flash_tl_h2d_i.a_mask);
        logic [MAX_LGM-1:0] i  = eff_idx(wa, lgm_eff);
        logic [31:0]        b  = line_base(wa, lgm_eff);

        if (flash_meta[i].valid) begin
          if (flash_meta[i].base == b) flash_dead_bytes       <= flash_dead_bytes + wB;
          else                         flash_alias_dead_bytes <= flash_alias_dead_bytes + wB;
        end
        flash_meta[i].valid <= 1'b1;
        flash_meta[i].base  <= b;

        flash_stores   <= flash_stores + 1;
        flash_bytes_wr <= flash_bytes_wr + wB;
      end
    end
  end


  // ------------------------- OUTPUT PIPING -----------------------------------
  // Pipe counters out just in case
  assign minstret_o         = minstret;
  assign load_cnt_sram_o    = sram_loads;
  assign store_cnt_sram_o   = sram_stores;
  assign load_cnt_flash_o   = flash_loads;
  assign store_cnt_flash_o  = flash_stores;
  assign store_bytes_total_o= sram_bytes_wr + flash_bytes_wr;
  assign dead_store_bytes_o = sram_dead_bytes + flash_dead_bytes;


  // ----------------------------- FINAL BLOCKS --------------------------------
  // Fallback: still dump at natural end if something else finishes the sim
  final begin
    tlmon_print();
    $fflush();
  end
endmodule

