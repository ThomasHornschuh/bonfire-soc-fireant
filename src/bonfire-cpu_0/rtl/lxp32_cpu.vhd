---------------------------------------------------------------------
-- LXP32 CPU Core
--
-- Part of the LXP32 CPU
--
-- Copyright (c) 2016 by Alex I. Kuznetsov
---------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity lxp32_cpu is
    generic(
        DBUS_RMW: boolean;
        DIVIDER_EN: boolean;
        MUL_ARCH: string;
        START_ADDR: std_logic_vector(29 downto 0);
        USE_RISCV : boolean := false;
        REG_RAM_STYLE : string := "block";
        ENABLE_TIMER : boolean := true;
        TIMER_XLEN : natural := 32;
        BRANCH_PREDICTOR : boolean := false
    );
    port(
        clk_i: in std_logic;
        rst_i: in std_logic;

        lli_re_o: out std_logic;
        lli_adr_o: out std_logic_vector(29 downto 0);
        lli_dat_i: in std_logic_vector(31 downto 0);
        lli_busy_i: in std_logic;
        lli_cc_invalidate_o : out std_logic;

        dbus_cyc_o: out std_logic;
        dbus_stb_o: out std_logic;
        dbus_we_o: out std_logic;
        dbus_sel_o: out std_logic_vector(3 downto 0);
        dbus_ack_i: in std_logic;
        dbus_adr_o: out std_logic_vector(31 downto 2);
        dbus_dat_o: out std_logic_vector(31 downto 0);
        dbus_dat_i: in std_logic_vector(31 downto 0);


        irq_i: in std_logic_vector(7 downto 0)
    );
end entity;

architecture rtl of lxp32_cpu is

signal fetch_word: std_logic_vector(31 downto 0);
signal fetch_next_ip: std_logic_vector(29 downto 0);
signal fetch_valid: std_logic;
signal fetch_jump_ready: std_logic;
signal fetch_fence_i : std_logic;

signal fetch_jump_prediction : std_logic;

signal decode_ready: std_logic;
signal decode_valid: std_logic;

signal decode_cmd_loadop3: std_logic;
signal decode_cmd_signed: std_logic;
signal decode_cmd_dbus: std_logic;
signal decode_cmd_dbus_store: std_logic;
signal decode_cmd_dbus_byte: std_logic;
signal decode_cmd_dbus_hword: std_logic:='0';
signal decode_cmd_addsub: std_logic;
signal decode_cmd_mul: std_logic;
signal decode_cmd_div: std_logic;
signal decode_cmd_div_mod: std_logic;
signal decode_cmd_cmp: std_logic;
signal decode_cmd_jump: std_logic;
signal decode_cmd_negate_op2: std_logic;
signal decode_cmd_and: std_logic;
signal decode_cmd_xor: std_logic;
signal decode_cmd_shift: std_logic;
signal decode_cmd_shift_right: std_logic;
signal decode_cmd_mul_high : std_logic; -- TH
signal decode_cmd_signed_b: std_logic; -- TH
signal decode_cmd_slt : std_logic; -- TH
signal decode_cmd_csr : std_logic; -- TH
signal decode_cmd_trap :  STD_LOGIC; -- TH: Execute trap
signal decode_cmd_tret :  STD_LOGIC; -- TH: return from trap

signal decode_jump_type: std_logic_vector(3 downto 0);

signal decode_op1: std_logic_vector(31 downto 0);
signal decode_op2: std_logic_vector(31 downto 0);
signal decode_op3: std_logic_vector(31 downto 0);
signal decode_dst: std_logic_vector(7 downto 0);

signal decode_csr_x0_o :  STD_LOGIC; -- should be set when rs field is x0
signal decode_csr_op_o :  STD_LOGIC_VECTOR (1 downto 0); -- lower bits of funct3

signal execute_ready: std_logic;
signal execute_jump_valid: std_logic;
signal execute_jump_dst: std_logic_vector(29 downto 0);
signal execute_jump_prediction  : std_logic;


signal sp_raddr1: std_logic_vector(7 downto 0);
signal sp_rdata1: std_logic_vector(31 downto 0);
signal sp_raddr2: std_logic_vector(7 downto 0);
signal sp_rdata2: std_logic_vector(31 downto 0);
signal sp_waddr: std_logic_vector(7 downto 0);
signal sp_we: std_logic;
signal sp_wdata: std_logic_vector(31 downto 0);

signal displacement : std_logic_vector(11 downto 0):= (others=>'0');

signal interrupt_valid: std_logic;
signal interrupt_vector: std_logic_vector(2 downto 0);
signal interrupt_ready: std_logic;
signal interrupt_return: std_logic;


signal decode_trap_cause : STD_LOGIC_VECTOR(3 downto 0); -- TH: Trap/Interrupt cause
signal decode_interrupt :  STD_LOGIC; -- Trap is interrupt
signal decode_epc,ex_epc,ex_tvec : std_logic_vector(31 downto 2);

signal sstep : std_logic;




begin

g_fetch_simple: if  not  BRANCH_PREDICTOR  generate

fetch_inst: entity work.lxp32_fetch(rtl)
    generic map(
        START_ADDR=>START_ADDR,
        USE_RISCV=>USE_RISCV
    )
    port map(
        clk_i=>clk_i,
        rst_i=>rst_i,

        lli_re_o=>lli_re_o,
        lli_adr_o=>lli_adr_o,
        lli_dat_i=>lli_dat_i,
        lli_busy_i=>lli_busy_i,
        lli_cc_invalidate_o=>lli_cc_invalidate_o,

        word_o=>fetch_word,
        next_ip_o=>fetch_next_ip,
        valid_o=>fetch_valid,
        ready_i=>decode_ready,
        fence_i_i=>fetch_fence_i,

        jump_valid_i=>execute_jump_valid,
        jump_dst_i=>execute_jump_dst,
        jump_ready_o=>fetch_jump_ready

    );

    fetch_jump_prediction <= '0';

end generate;

g_fetch_bonfire: if  BRANCH_PREDICTOR generate

  assert USE_RISCV
    report "BRANCH_PREDICTOR only supported with USE_RISCV = true "
    severity failure;

  fetch_inst: entity work.bonfire_fetch(rtl)
      generic map(
          START_ADDR=>START_ADDR

      )
      port map(
          clk_i=>clk_i,
          rst_i=>rst_i,

          lli_re_o=>lli_re_o,
          lli_adr_o=>lli_adr_o,
          lli_dat_i=>lli_dat_i,
          lli_busy_i=>lli_busy_i,
          lli_cc_invalidate_o=>lli_cc_invalidate_o,

          word_o=>fetch_word,
          next_ip_o=>fetch_next_ip,
          valid_o=>fetch_valid,
          ready_i=>decode_ready,
          fence_i_i=>fetch_fence_i,

          jump_valid_i=>execute_jump_valid,
          jump_dst_i=>execute_jump_dst,
          jump_ready_o=>fetch_jump_ready,
          jump_prediction_o=>fetch_jump_prediction
      );



end generate;



lxp32decode: if not USE_RISCV generate
  decode_inst: entity work.lxp32_decode(rtl)

    port map(
        clk_i=>clk_i,
        rst_i=>rst_i,

        word_i=>fetch_word,
        next_ip_i=>fetch_next_ip,
        valid_i=>fetch_valid,
        jump_valid_i=>execute_jump_valid,
        ready_o=>decode_ready,


        interrupt_valid_i=>interrupt_valid,
        interrupt_vector_i=>interrupt_vector,
        interrupt_ready_o=>interrupt_ready,

        sp_raddr1_o=>sp_raddr1,
        sp_rdata1_i=>sp_rdata1,
        sp_raddr2_o=>sp_raddr2,
        sp_rdata2_i=>sp_rdata2,

        ready_i=>execute_ready,
        valid_o=>decode_valid,

        cmd_loadop3_o=>decode_cmd_loadop3,
        cmd_signed_o=>decode_cmd_signed,
        cmd_dbus_o=>decode_cmd_dbus,
        cmd_dbus_store_o=>decode_cmd_dbus_store,
        cmd_dbus_byte_o=>decode_cmd_dbus_byte,
        cmd_addsub_o=>decode_cmd_addsub,
        cmd_mul_o=>decode_cmd_mul,
        cmd_div_o=>decode_cmd_div,
        cmd_div_mod_o=>decode_cmd_div_mod,
        cmd_cmp_o=>decode_cmd_cmp,
        cmd_jump_o=>decode_cmd_jump,
        cmd_negate_op2_o=>decode_cmd_negate_op2,
        cmd_and_o=>decode_cmd_and,
        cmd_xor_o=>decode_cmd_xor,
        cmd_shift_o=>decode_cmd_shift,
        cmd_shift_right_o=>decode_cmd_shift_right,



        jump_type_o=>decode_jump_type,



        op1_o=>decode_op1,
        op2_o=>decode_op2,
        op3_o=>decode_op3,
        dst_o=>decode_dst
    );

   decode_cmd_mul_high<='0'; -- TH
   decode_cmd_signed_b<='0'; -- TH
   decode_cmd_slt <= '0'; -- TH
   decode_cmd_trap <= '0';
   decode_cmd_tret <= '0';
   decode_cmd_csr <= '0';
   fetch_fence_i <= '0';
end generate;

riscv_decode: if USE_RISCV generate
decode_inst: entity work.riscv_decode(rtl)
    generic map (
     BRANCH_PREDICTOR=>BRANCH_PREDICTOR
    )
    port map(
        clk_i=>clk_i,
        rst_i=>rst_i,

        word_i=>fetch_word,
        next_ip_i=>fetch_next_ip,
        valid_i=>fetch_valid,
        jump_valid_i=>execute_jump_valid,
        ready_o=>decode_ready,
        fencei_o=>fetch_fence_i,

        interrupt_valid_i=>interrupt_valid,
        interrupt_vector_i=>interrupt_vector,
        interrupt_ready_o=>interrupt_ready,

        sp_raddr1_o=>sp_raddr1,
        sp_rdata1_i=>sp_rdata1,
        sp_raddr2_o=>sp_raddr2,
        sp_rdata2_i=>sp_rdata2,

        displacement_o=>displacement,

        ready_i=>execute_ready,
        valid_o=>decode_valid,

        cmd_loadop3_o=>decode_cmd_loadop3,
        cmd_signed_o=>decode_cmd_signed,
        cmd_dbus_o=>decode_cmd_dbus,
        cmd_dbus_store_o=>decode_cmd_dbus_store,
        cmd_dbus_byte_o=>decode_cmd_dbus_byte,
        cmd_dbus_hword_o=>decode_cmd_dbus_hword, -- TH
        cmd_addsub_o=>decode_cmd_addsub,
        cmd_mul_o=>decode_cmd_mul,
        cmd_div_o=>decode_cmd_div,
        cmd_div_mod_o=>decode_cmd_div_mod,
        cmd_cmp_o=>decode_cmd_cmp,
        cmd_jump_o=>decode_cmd_jump,
        cmd_negate_op2_o=>decode_cmd_negate_op2,
        cmd_and_o=>decode_cmd_and,
        cmd_xor_o=>decode_cmd_xor,
        cmd_shift_o=>decode_cmd_shift,
        cmd_shift_right_o=>decode_cmd_shift_right,
        cmd_mul_high_o=>decode_cmd_mul_high, -- TH
        cmd_signed_b_o=>decode_cmd_signed_b,
        cmd_slt_o => decode_cmd_slt, --TH

        -- TH: CSR
        cmd_csr_o=>decode_cmd_csr,
        csr_x0_o=>decode_csr_x0_o,
        csr_op_o=>decode_csr_op_o,

        cmd_trap_o => decode_cmd_trap,
        cmd_tret_o => decode_cmd_tret,
        trap_cause_o => decode_trap_cause,
        interrupt_o => decode_interrupt,
        epc_o => decode_epc,

        epc_i => ex_epc,
        tvec_i => ex_tvec,
        sstep_i => sstep,

        jump_type_o=>decode_jump_type,
        jump_prediction_i=>fetch_jump_prediction,
        jump_prediction_o=>execute_jump_prediction,

        op1_o=>decode_op1,
        op2_o=>decode_op2,
        op3_o=>decode_op3,
        dst_o=>decode_dst
    );
end generate;

execute_inst: entity work.lxp32_execute(rtl)
    generic map(
        DBUS_RMW=>DBUS_RMW,
        DIVIDER_EN=>DIVIDER_EN,
        MUL_ARCH=>MUL_ARCH,
        USE_RISCV=>USE_RISCV,
        ENABLE_TIMER=>ENABLE_TIMER,
        TIMER_XLEN=>TIMER_XLEN,
        BRANCH_PREDICTOR=>BRANCH_PREDICTOR
    )
    port map(
        clk_i=>clk_i,
        rst_i=>rst_i,

        next_ip_i=> fetch_next_ip, -- TH
        cmd_loadop3_i=>decode_cmd_loadop3,
        cmd_signed_i=>decode_cmd_signed,
        cmd_dbus_i=>decode_cmd_dbus,
        cmd_dbus_store_i=>decode_cmd_dbus_store,
        cmd_dbus_byte_i=>decode_cmd_dbus_byte,
        cmd_dbus_hword_i=>decode_cmd_dbus_hword, -- TH
        cmd_addsub_i=>decode_cmd_addsub,
        cmd_mul_i=>decode_cmd_mul,
        cmd_div_i=>decode_cmd_div,
        cmd_div_mod_i=>decode_cmd_div_mod,
        cmd_cmp_i=>decode_cmd_cmp,
        cmd_jump_i=>decode_cmd_jump,
        cmd_negate_op2_i=>decode_cmd_negate_op2,
        cmd_and_i=>decode_cmd_and,
        cmd_xor_i=>decode_cmd_xor,
        cmd_shift_i=>decode_cmd_shift,
        cmd_shift_right_i=>decode_cmd_shift_right,
        cmd_mul_high_i=>decode_cmd_mul_high, --TH
        cmd_signed_b_i=>decode_cmd_signed_b, -- TH
        cmd_slt_i => decode_cmd_slt, -- TH

        cmd_csr_i=>decode_cmd_csr,
        csr_op_i=>decode_csr_op_o,
        csr_x0_i=>decode_csr_x0_o,

        cmd_trap_i=>decode_cmd_trap,
        cmd_tret_i=>decode_cmd_tret,
        trap_cause_i => decode_trap_cause,
        interrupt_i => decode_interrupt,
        epc_i => decode_epc,
        epc_o => ex_epc,
        tvec_o => ex_tvec,
        sstep_o => sstep,

        jump_type_i=>decode_jump_type,
        jump_prediction_i=>execute_jump_prediction,

        op1_i=>decode_op1,
        op2_i=>decode_op2,
        op3_i=>decode_op3,
        dst_i=>decode_dst,

        sp_waddr_o=>sp_waddr,
        sp_we_o=>sp_we,
        sp_wdata_o=>sp_wdata,
        displacement_i=>displacement,

        valid_i=>decode_valid,
        ready_o=>execute_ready,

        dbus_cyc_o=>dbus_cyc_o,
        dbus_stb_o=>dbus_stb_o,
        dbus_we_o=>dbus_we_o,
        dbus_sel_o=>dbus_sel_o,
        dbus_ack_i=>dbus_ack_i,
        dbus_adr_o=>dbus_adr_o,
        dbus_dat_o=>dbus_dat_o,
        dbus_dat_i=>dbus_dat_i,

        jump_valid_o=>execute_jump_valid,
        jump_dst_o=>execute_jump_dst,
        jump_ready_i=>fetch_jump_ready,

        interrupt_return_o=>interrupt_return,

        ext_irq_in=>irq_i, -- for RISC-V only...
        -- for RISC-V: Interrupt are handle by the control unit, the signal below will be asserted by the CU
        -- to the decode to execute an interrupt
        riscv_interrupt_exec_o => interrupt_valid
    );

lxp_regs: if not USE_RISCV  generate
scratchpad_inst: entity work.lxp32_scratchpad(rtl)
    port map(
        clk_i=>clk_i,

        raddr1_i=>sp_raddr1,
        rdata1_o=>sp_rdata1,
        raddr2_i=>sp_raddr2,
        rdata2_o=>sp_rdata2,

        waddr_i=>sp_waddr,
        we_i=>sp_we,
        wdata_i=>sp_wdata
    );

end generate;


riscv_regs: if  USE_RISCV  generate
scratchpad_inst: entity work.riscv_regfile(rtl)
    generic map( REG_RAM_STYLE=>REG_RAM_STYLE)
    port map(
        clk_i=>clk_i,

        raddr1_i=>sp_raddr1,
        rdata1_o=>sp_rdata1,
        raddr2_i=>sp_raddr2,
        rdata2_o=>sp_rdata2,

        waddr_i=>sp_waddr,
        we_i=>sp_we,
        wdata_i=>sp_wdata
    );

end generate;


lxp_imux : if not USE_RISCV generate
interrupt_mux_inst: entity work.lxp32_interrupt_mux(rtl)
    port map(
        clk_i=>clk_i,
        rst_i=>rst_i,

        irq_i=>irq_i,

        interrupt_valid_o=>interrupt_valid,
        interrupt_vector_o=>interrupt_vector,
        interrupt_ready_i=>interrupt_ready,
        interrupt_return_i=>interrupt_return,

        sp_waddr_i=>sp_waddr,
        sp_we_i=>sp_we,
        sp_wdata_i=>sp_wdata
    );
end generate;

end architecture;
