----------------------------------------------------------------------------------

--
-- The Bonfire Processor Project, (c) 2016,2017,2018 Thomas Hornschuh


-- License: See LICENSE or LICENSE.txt File in git project root.
-- Description:
-- 2K*8 Bit  Dual Port RAM with "write first" strategy
-- Simulation version independant of Xilinx prmitives (e.g for easy use with ghdl)
-- Can be used as replacement for ram2048x8, but not at the same time

--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use IEEE.NUMERIC_STD.ALL;



entity ram2048x8 is
    Port ( DOutA : out  STD_LOGIC_VECTOR (7 downto 0);
           DInA : in  STD_LOGIC_VECTOR (7 downto 0);
           AdrA : in  STD_LOGIC_VECTOR (10 downto 0);
           ENA : in  STD_LOGIC;
           WRENA : in  STD_LOGIC;
           CLKA : in  STD_LOGIC;
           DoutB : out  STD_LOGIC_VECTOR (7 downto 0);
           DInB : in  STD_LOGIC_VECTOR (7 downto 0);
           AdrB : in  STD_LOGIC_VECTOR (10 downto 0);
           ENB : in  STD_LOGIC;
           WRENB : in  STD_LOGIC;
           CLKB : in  STD_LOGIC);
end ram2048x8;

architecture Behavioral of ram2048x8 is


type t_ram is array (0 to 2047) of std_logic_vector(7 downto 0);

shared variable ram : t_ram;


begin


  dummy: process begin
    report "Instantiation of ram2048x8sim" severity note;
    wait;
  end process;


  process(CLKA) begin
    if rising_edge(CLKA) then
       if ENA = '1' then
         if WRENA='1' then
           ram(to_integer(unsigned(AdrA))):=DInA;
         end if;
         DOutA <= ram(to_integer(unsigned(AdrA)));
       end if;
    end if;

  end process;

  process(CLKB) begin
    if rising_edge(CLKB) then
       if ENB = '1' then
         if WRENB='1' then
           ram(to_integer(unsigned(AdrB))):=DInB;
         end if;
         DOutB <= ram(to_integer(unsigned(AdrB)));
       end if;
    end if;

  end process;



end Behavioral;

