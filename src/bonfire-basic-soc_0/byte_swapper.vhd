----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    00:28:56 12/10/2016 
-- Design Name: 
-- Module Name:    byte_swapper - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
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
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity byte_swapper is
    Port ( din : in  STD_LOGIC_VECTOR (31 downto 0);
           dout : out  STD_LOGIC_VECTOR (31 downto 0));
end byte_swapper;

architecture Behavioral of byte_swapper is

attribute keep_hierarchy : string;
attribute keep_hierarchy of Behavioral: architecture is "TRUE";

subtype tWord is std_logic_vector(31 downto 0);

function doSwapBytes(d : tWord) return tWord is
begin
  
    return d(7 downto 0)&d(15 downto 8)&d(23 downto 16)&d(31 downto 24);
  
end;

begin

   dout <= doSwapBytes(din);

end Behavioral;

