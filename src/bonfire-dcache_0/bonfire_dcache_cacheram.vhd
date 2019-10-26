----------------------------------------------------------------------------------

-- Create Date:    16:38:14 10/03/2017
-- Design Name:
-- Module Name:    bonfire_dcache_cacheram - Behavioral

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh

-- Generic RAM implementation for inference


-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.log2;

entity bonfire_dcache_cacheram is
generic(
  CACHE_SIZE : natural :=2048; -- Cache Size in MASTER_DATA_WIDTH Bit words
  MASTER_DATA_WIDTH : natural := 128;
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
end bonfire_dcache_cacheram;

architecture Behavioral of bonfire_dcache_cacheram is


--attribute keep_hierarchy : string;
--attribute keep_hierarchy of Behavioral: architecture is ATTR_KEEP_HIERARCHY;



type t_cache_ram is array (0 to CACHE_SIZE-1) of std_logic_vector(MASTER_DATA_WIDTH-1 downto 0);

shared variable cache_ram : t_cache_ram;
attribute ram_style: string;
attribute ram_style of cache_ram: variable is "block";

begin

--  debug: process(clk_i)
--  begin
--     if rising_edge(clk_i) then
--       if cache_ram(0) = X"0000000c000000080000000400000800" then
--         report  "Crash" severity note;
--       end if;  
--     end if;
--  end process; 

  proc_cache_ram_slave: process(clk_i) begin
   if rising_edge(clk_i) then

    if slave_en_i='1' then
         -- -- write cycle
        for b in slave_wren_i'range loop -- byte selector
           if slave_wren_i(b)='1' then
             cache_ram(to_integer(unsigned(slave_adr_i)))((b+1)*8-1 downto b*8):=slave_db_i((b+1)*8-1 downto b*8);
           end if;
         end loop;
        --  read cycle
        slave_db_o <= cache_ram(to_integer(unsigned(slave_adr_i)));
     end if;

   end if;
 end process;


  cache_ram_master: process(clk_i) begin

     if rising_edge (clk_i) then

         if master_en_i='1' then
            if  master_we_i='1' then
              cache_ram(to_integer(unsigned(master_adr_i))):=master_db_i;
            end if;
            master_db_o <= cache_ram(to_integer(unsigned(master_adr_i)));
         end if;

     end if;
   end process;

end Behavioral;

