---------------------------------------------------------------------------------

-- Module Name:    MainMemory - Behavioral


-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh


-- License: See LICENSE or LICENSE.txt File in git project root.

-- Main Memory consisting of 8 Bit Lanes
-- can be initalized with a file



----------------------------------------------------------------------------------





library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_textio.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

library STD;
use STD.textio.all;

entity MainMemory is
    generic (RamFileName : string := "meminit.ram";
             mode : string := "B";
             ADDR_WIDTH: integer;
             SIZE : integer;
             Swapbytes : boolean; -- SWAP Bytes in RAM word in low byte first order to use data2mem
             EnableSecondPort : boolean := true -- enable inference of the second port
            );
    Port ( DBOut : out  STD_LOGIC_VECTOR (31 downto 0);
           DBIn : in  STD_LOGIC_VECTOR (31 downto 0);
           AdrBus : in  STD_LOGIC_VECTOR (ADDR_WIDTH-1 downto 0);
           ENA : in  STD_LOGIC;
           WREN : in  STD_LOGIC_VECTOR (3 downto 0);
           CLK : in  STD_LOGIC;
           -- Second Port ( read only)
           CLKB : in STD_LOGIC;
           ENB : in STD_LOGIC;
           AdrBusB : in  STD_LOGIC_VECTOR (ADDR_WIDTH-1 downto 0);
           DBOutB : out  STD_LOGIC_VECTOR (31 downto 0)

              );
end MainMemory;


architecture Behavioral of MainMemory is

begin

  genmem : for i in 0 to 3 generate

        Inst_ram: entity work.ram8
        GENERIC MAP (
          ADR_SIZE => ADDR_WIDTH,
          RAM_FILE => RamFileName,
          LANE_INDEX => i
        )
        PORT MAP(
           DOutA => DbOut((i+1)*8 -1 downto i*8),
           DInA => DBIn((i+1)*8 -1 downto i*8),
           AdrA => AdrBus,
           ENA =>  ENA,
           WRENA =>WREN(i),
           CLKA => CLK,
           DoutB => DBOutB((i+1)*8 -1 downto i*8),
           DInB => (others=>'0'),
           AdrB => AdrBusB ,
           ENB =>  ENB,
           WRENB => '0' ,
           CLKB =>  CLKB
        );

     end generate;

end Behavioral;
