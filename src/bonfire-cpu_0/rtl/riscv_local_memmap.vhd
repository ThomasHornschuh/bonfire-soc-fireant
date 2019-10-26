----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:13:30 03/09/2017 
-- Design Name: 
-- Module Name:    riscv_local_memmap - rtl 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--   Bonfire CPU 
--   (c) 2016,2017 Thomas Hornschuh
--   See license.md for License 

-- Bonfire CPU local memory mapped devices
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity riscv_local_memmap is

generic (

   TIMER_XLEN : natural := 32

);

port (
  -- bus interface
		clk_i: in std_logic;
		rst_i: in std_logic;
		
		wbs_cyc_i: in std_logic;
		wbs_stb_i: in std_logic;
		wbs_we_i: in std_logic;
		wbs_sel_i: in std_logic_vector(3 downto 0);
		wbs_ack_o: out std_logic;
		wbs_adr_i: in std_logic_vector(15 downto 2);
		wbs_dat_i: in std_logic_vector(31 downto 0);
		wbs_dat_o: out std_logic_vector(31 downto 0);
      
      timer_irq_o : out std_logic
);

end riscv_local_memmap;

architecture rtl of riscv_local_memmap is

signal mtime : unsigned(TIMER_XLEN-1 downto 0) := (others=>'0');
signal mtimecmp :  unsigned(TIMER_XLEN-1 downto 0) := (others=>'0');
signal cs : std_logic;
signal timer_irq : std_logic :='0';

begin

  timer_irq_o<=timer_irq;

  cs <= '1'  when wbs_adr_i(15 downto 4) = "000000000000" and wbs_cyc_i='1' and wbs_stb_i='1' else
        '0';

  wbs_ack_o <= cs;
  
  --TODO: Add Support for mtime and mtimecmp > 32 Bit
  with wbs_adr_i(3 downto 2) select
     wbs_dat_o <= std_logic_vector(mtime(31 downto 0))    when "00",
                  std_logic_vector(mtimecmp(31 downto 0)) when "10",
                  (others=>'X') when others;
  
  

  process(clk_i) begin
    if rising_edge(clk_i) then
      if rst_i='1' then
        mtime <= (others=>'0');
        mtimecmp <= (others=>'0');
        timer_irq<='0';
      else
        mtime <= mtime + 1;
        if mtimecmp /= 0 and mtime=mtimecmp then
          timer_irq <= '1';
        end if;
        if cs='1' and wbs_we_i='1'  then
          case wbs_adr_i(3 downto 2) is
             when "10" =>
               mtimecmp(31 downto 0) <= unsigned(wbs_dat_i);
               timer_irq <= '0';
             when "11" =>  
              if mtimecmp'high > 31 then
                mtimecmp(mtimecmp'high downto 32) <= unsigned(wbs_dat_i(mtimecmp'high-32 downto 0)); 
                timer_irq <= '0';
              end if;
             when others =>  
          end case;
        end if;           
      end if;
    end if;
  
  
  end process;


end rtl;

