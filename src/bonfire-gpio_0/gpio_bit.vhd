----------------------------------------------------------------------------------
-- Module Name:    gpio_bit - Behavioral

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh

-- Models a single bit of the GPIO Port. See chapter 17. of the SiFive FE310-G000 Manual

-- License: See LICENSE or LICENSE.txt File in git project root. 
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

entity gpio_bit is
    Port ( rst_i: in STD_LOGIC;
           clk_i : in STD_LOGIC;
           pin_value_o : out  STD_LOGIC;
           input_en_i : in  STD_LOGIC;
           output_en_i : in  STD_LOGIC;
           port_value_i : in  STD_LOGIC;
           out_xor_i : in  STD_LOGIC;

           rising_o : out STD_LOGIC;
           falling_o : out STD_LOGIC;

           -- IO Block control - connect to a IO Block
           iob_o : out  STD_LOGIC; -- Output
           iob_i : in STD_LOGIC;  -- Input
           iob_t : out STD_LOGIC   -- Tri-State out

           );
end gpio_bit;

architecture Behavioral of gpio_bit is

signal pv1, pv2, pin_value : std_logic; -- Input synchronizers

-- Xilinx specific attributes
-- Place pv1 FF in IO Block
attribute IOB: string;
attribute IOB of pv1: signal is "true";

--signal edge : std_logic; --  for Edge detector
signal rising, falling : std_logic;

begin

-- Input
 process(clk_i) begin
    if rising_edge(clk_i) then
      pv1 <= iob_i;
      --pv2 <= pv1;
      if input_en_i='1' then
        pv2 <= pv1;
      else
        pv2 <= '0';
      end if;
              
      pin_value <= pv2;
      
      -- Edge detector
      rising <= '0';
      falling <= '0';
      if pv2='1' and pin_value='0' then
        rising <= '1';
      end if;  
      if pv2='0' and pin_value='1' then
        falling <= '1';
      end if;
    end if;

 end process;

 pin_value_o <= pin_value;

 -- Output

 iob_o <=  port_value_i xor out_xor_i;
 iob_t <=  not output_en_i;

 rising_o <= rising;
 falling_o <= falling;

end Behavioral;

