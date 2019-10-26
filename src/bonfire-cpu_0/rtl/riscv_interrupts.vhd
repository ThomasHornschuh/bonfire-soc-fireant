----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date:    16:04:26 03/10/2017
-- Design Name:
-- Module Name:    riscv_interrupts - rtl
-- Project Name:
-- Target Devices:
-- Tool versions:
-- Description:
--   Bonfire CPU
--   (c) 2016,2017 Thomas Hornschuh
--   See license.md for License

-- RISC-V local interrupt controller
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--synthesis translate_off
use work.txt_util.all;
--synthesis translate_on


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.csr_def.all;

entity riscv_interrupts is
    generic (
      NUM_LOCALINTERUPTS : natural :=16
    );
    Port (
      mie : in  STD_LOGIC; -- Global M-Mode Interrupt Enable
      ir_in  : in t_irq_enable;
      ir_out : out t_irq_pending;
      --ir_we_o : out std_logic; -- Interrupt register Write Enable
      interrupt_exec_o : out std_logic;  -- Signal Interrrupt to exec/decode unit
      interrupt_ack_i : in std_logic; --  Interrupt was taken
      mcause_o : out t_mcause;

      ext_irq_in : in std_logic;
      timer_irq_in : in std_logic;
      software_irq_in : in std_logic;
      l_irq_in : in std_logic_vector(NUM_LOCALINTERUPTS-1 downto 0);

      clk_i : in  STD_LOGIC;
      rst_i : in  STD_LOGIC
    );

end riscv_interrupts;

architecture rtl of riscv_interrupts is

signal interrupt_exec: std_logic:='0';

signal irq_pending : t_irq_pending := c_pending_init;

begin

  assert  NUM_LOCALINTERUPTS<=16
    report "NUM_LOCALINTERUPTS out of range 0..16"
    severity failure;

  ir_out<=irq_pending;

  -- register pending interrupts
  process(clk_i) begin

    if rising_edge(clk_i) then
      if rst_i='1' then
          irq_pending<= c_pending_init;
      else
        irq_pending.mtip<=timer_irq_in;
        irq_pending.msip<=software_irq_in;
        irq_pending.meip<=ext_irq_in;
        irq_pending.lip(l_irq_in'range) <= l_irq_in;
      end if;
    end if;
  end process;




  interrupt_exec_o<=interrupt_exec;

  -- Interrupt priority decoder
  process(clk_i)
  variable found : boolean;
  variable int_e : std_logic;
  variable cause : t_mcause;
  begin
    if rising_edge(clk_i) then
      if rst_i='1' then
          interrupt_exec<='0';
      else

          found:=false;
          int_e := '0';
          if mie='1' and interrupt_exec='0' then -- process interrupts when globally enabled
            for i in l_irq_in'range loop
              if ir_in.lie(i)='1' and irq_pending.lip(i)='1' then
                 int_e:='1';
                 cause:= irq_cause(i+16);
                 found:=true;
                 exit;
              end if;
            end loop;
            if not found then
               if ir_in.meie='1' and irq_pending.meip='1' then
                 int_e:='1';
                 cause:=irq_cause(IRQ_CODE_MEXTERNAL);
               elsif ir_in.msie='1' and irq_pending.msip='1' then
                 int_e:='1';
                 cause:= irq_cause(IRQ_CODE_MSOFTWARE);
               elsif ir_in.mtie='1' and irq_pending.mtip='1'  then
                 int_e:='1';
                 cause:=irq_cause(IRQ_CODE_MTIMER);
               end if;
            end if;
            interrupt_exec<=int_e;
            mcause_o<=cause;
            --synthesis translate_off
              if int_e='1' then
                report "Interrupt triggered: " & hstr(cause)
                severity note;
              end if;
            --synthesis translate_on
          elsif interrupt_exec='1' and interrupt_ack_i='1' then
            interrupt_exec<='0';
          end if;
      end if;
    end if;


  end process;


end rtl;
