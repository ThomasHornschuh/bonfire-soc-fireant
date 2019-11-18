----------------------------------------------------------------------------------

-- Create Date:    23:34:41 12/06/2017

-- Module Name:    gpio_pad - Behavioral

-- Description:

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh
-- IO Buf for Simulation uses. Defined locally to allow easy simulation with ghdl
--
-- License: See LICENSE or LICENSE.txt File in git project root.
--
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;



entity gpio_pad is
    Port ( I : in  STD_LOGIC;
           O : out  STD_LOGIC;
           T : in  STD_LOGIC;
           IO : inout  STD_LOGIC);
end gpio_pad;

architecture Behavioral of gpio_pad is

begin

   O <= IO;

   process(I,T) begin
     if T='1' then
       IO <= 'Z';
     else
       IO <= I;
     end if;

   end process;


end Behavioral;

