----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    20:45:30 03/22/2017 
-- Design Name: 
-- Module Name:    counter_64Bit - rtl 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--  Bonfire CPU 
--   (c) 2016,2017 Thomas Hornschuh
--   See license.md for License 
--  64 Bit Read Only Counter

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

entity counter_64Bit is
    Port ( clk_i : in  STD_LOGIC;
           reset_i : in  STD_LOGIC;
           counter_value_o : out  STD_LOGIC_VECTOR (63 downto 0));
end counter_64Bit;

architecture rtl of counter_64Bit is

signal counter : unsigned(counter_value_o'RANGE) :=  (others=>'0');

begin

  counter_value_o <= std_logic_vector(counter);

  process(clk_i) begin
    
    if rising_edge(clk_i) then
      if reset_i='1' then
        counter <= (others=>'0');
      else
        counter <= counter + 1;
      end if;        
    end if;
  
  end process;


end rtl;

