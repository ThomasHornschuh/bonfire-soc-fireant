----------------------------------------------------------------------------------

--
-- The Bonfire Processor Project, (c) 2016,2017,2018 Thomas Hornschuh


-- License: See LICENSE or LICENSE.txt File in git project root.
-- Description:
-- 512*8 Bit  Dual Port RAM with "write first" strategy



--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_textio.all;

use IEEE.NUMERIC_STD.ALL;

library STD;
use STD.textio.all;


entity ram8 is
    generic (
       ADR_SIZE : natural := 9;
       RAM_FILE : string :="";
       LANE_INDEX :natural := 0;
       MODE : string := "H"

    );
    Port ( DOutA : out  STD_LOGIC_VECTOR (7 downto 0);
           DInA : in  STD_LOGIC_VECTOR (7 downto 0);
           AdrA : in  STD_LOGIC_VECTOR (ADR_SIZE-1 downto 0);
           ENA : in  STD_LOGIC;
           WRENA : in  STD_LOGIC;
           CLKA : in  STD_LOGIC;
           DoutB : out  STD_LOGIC_VECTOR (7 downto 0);
           DInB : in  STD_LOGIC_VECTOR (7 downto 0);
           AdrB : in  STD_LOGIC_VECTOR (ADR_SIZE-1 downto 0);
           ENB : in  STD_LOGIC;
           WRENB : in  STD_LOGIC;
           CLKB : in  STD_LOGIC);
end ram8;

architecture Behavioral of ram8 is

constant size : natural := 2 ** ADR_SIZE;


type tRam is array (0 to size-1) of std_logic_vector(7 downto 0);
subtype tWord is std_logic_vector(31 downto 0);




impure function InitFromFile(lane: natural)  return tRam is
FILE RamFile : text; -- is in RamFileName;
variable RamFileLine : line;
variable word : tWord;
variable r : tRam;
variable is_open : boolean;

begin
  report "RamFileName is " & RAM_FILE;
  if RAM_FILE /= "" then
    file_open(RamFile,RAM_FILE,READ_MODE);
    is_open:=true;
  else
    is_open:=false;
  end if;
  for I in tRam'range loop
    if is_open and not endfile(RamFile) then
      readline (RamFile, RamFileLine);
      if mode="H" then
        hread (RamFileLine, word); -- alternative: HEX read
      else
        read(RamFileLine,word);  -- Binary read
      end if;
      r(I) := word( (lane+1)*8 -1 downto lane*8 ); -- Get selected byte lane
      -- if SwapBytes then
      --   r(I) :=  DoSwapBytes(word);
      -- else
      --   r(I) := word;
      -- end if;
    else
      r(I) := (others=>'0');
    end if;
  end loop;
  if is_open then
    file_close(RamFile);
  end if;
  return r;
end function;


shared variable ram : tRam := InitFromFile(LANE_INDEX);

begin


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
