----------------------------------------------------------------------------------

-- Create Date:    18:13:15 10/03/2017
-- Module Name:    dcache_ram8K_spartan6 - Behavioral

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh

--  Spartan 6 specific RAM implementation with block RAM primitives


-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------
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

use work.log2;


entity dcache_ram8K_spartan6 is
generic(
  CACHE_SIZE : natural :=2048; -- Cache Size in MASTER_DATA_WIDTH Bit words
  MASTER_DATA_WIDTH : natural := 32;
  ATTR_KEEP_HIERARCHY : string := "FALSE"
);
    Port ( slave_db_i : in  STD_LOGIC_VECTOR (MASTER_DATA_WIDTH-1 downto 0);
           slave_db_o : out  STD_LOGIC_VECTOR (MASTER_DATA_WIDTH-1 downto 0);
           slave_wren_i : in  STD_LOGIC_VECTOR (MASTER_DATA_WIDTH / 8 - 1 downto 0);
           slave_en_i : in  STD_LOGIC;
           slave_adr_i : in  STD_LOGIC_VECTOR (log2.log2(CACHE_SIZE)-1 downto 0);
           master_db_i : in  STD_LOGIC_VECTOR (MASTER_DATA_WIDTH-1 downto 0);
           master_db_o : out  STD_LOGIC_VECTOR (MASTER_DATA_WIDTH-1 downto 0);
           master_we_i : in  STD_LOGIC;
           master_en_i : in  STD_LOGIC;
           master_adr_i : in  STD_LOGIC_VECTOR (log2.log2(CACHE_SIZE)-1 downto 0);
           clk_i : in  STD_LOGIC);
end dcache_ram8K_spartan6;

architecture Behavioral of dcache_ram8K_spartan6 is



--attribute keep_hierarchy : string;
--attribute keep_hierarchy of Behavioral: architecture is keep_h;



begin

  assert CACHE_SIZE = 2048 and  MASTER_DATA_WIDTH = 32
    report "dcache_ram8K_spartan6: This module only support 8K * 32Bits Cache configuration"
    severity failure;


   genmem : for i in 0 to 3 generate

      Inst_ram2048x8: entity work.ram2048x8 PORT MAP(
         DOutA => slave_db_o((i+1)*8 -1 downto i*8),
         DInA => slave_db_i((i+1)*8 -1 downto i*8),
         AdrA => slave_adr_i,
         ENA =>  slave_en_i,
         WRENA =>slave_wren_i(i),
         CLKA => clk_i,
         DoutB =>master_db_o((i+1)*8 -1 downto i*8),
         DInB => master_db_i((i+1)*8 -1 downto i*8),
         AdrB => master_adr_i ,
         ENB =>  master_en_i,
         WRENB => master_we_i ,
         CLKB =>  clk_i
      );

   end generate;

end Behavioral;

