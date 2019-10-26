----------------------------------------------------------------------------------

-- Module Name:    wbs_memory_interface - Behavioral 

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh

-- License: See LICENSE or LICENSE.txt File in git project root. 
-- 
--   
--   Wishbone interface for Block RAMs with Wishbone Burst support
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

entity wbs_memory_interface is
generic (
     ram_adr_width : natural;
     ram_size : natural;
     wbs_adr_high : natural := 27;
	  RamFileName : string := "meminit.ram";
	  mode : string := "B";
     Swapbytes : boolean := true -- SWAP Bytes in RAM word in low byte first order to use data2mem  
--     UseBRAMPrimitives : boolean := TRUE
	 );
port(
		clk_i: in std_logic;
		rst_i: in std_logic;
		
		wbs_cyc_i: in std_logic;
		wbs_stb_i: in std_logic;
		wbs_we_i: in std_logic;
		wbs_sel_i: in std_logic_vector(3 downto 0);
		wbs_ack_o: out std_logic;
		wbs_adr_i: in std_logic_vector(wbs_adr_high downto 2);
		wbs_dat_i: in std_logic_vector(31 downto 0);
		wbs_dat_o: out std_logic_vector(31 downto 0);
        wbs_cti_i: in std_logic_vector(2 downto 0)
		
	);
end wbs_memory_interface;

architecture Behavioral of wbs_memory_interface is

constant slave_adr_high : natural := 29; 
-- Slaves
-- RAM
signal ram_adr,ram_adr_o : std_logic_vector(ram_adr_width-1 downto 0);
signal ram_a_we: std_logic_vector(3 downto 0);
signal is_read,ack_read, ack_write : std_logic;

signal adr_reg : std_logic_vector(ram_adr_width-1 downto 0); -- for Burst mode support

begin

   
    ram_adr <= wbs_adr_i(ram_adr_width+1  downto 2);
	 
    is_read <= wbs_cyc_i and wbs_stb_i and not wbs_we_i;
  
  -- Wishbone ACK 
  process (clk_i) is
  begin
	if rising_edge(clk_i) then  
      if ack_read='1' and wbs_cti_i/="010" then -- clear ack at end of  cycle 
        ack_read <= '0';
      else        
		  ack_read<= is_read;
      end if;  
	end if;
  end process;
  

   ack_write<=wbs_cyc_i and wbs_stb_i and wbs_we_i;
   wbs_ack_o<=ack_read or ack_write;


-- Burst Mode support

   process(clk_i) is
   begin
     if rising_edge(clk_i) then  
        if is_read='1' and wbs_cti_i="010" then  -- burst cycle ??
          if ack_read='0' then -- begin of new cycle
            adr_reg <= std_logic_vector(unsigned(ram_adr)+1);
          else
             adr_reg <= std_logic_vector(unsigned(adr_reg)+1); 
          end if;             
        end if;
	end if;
   
   end process;


   -- adr multiplexer
   process(ram_adr,adr_reg,is_read,ack_read,wbs_cti_i) is
   begin
     if is_read='1' and ack_read='1' and (wbs_cti_i="010" or wbs_cti_i="111") then
       ram_adr_o <=  adr_reg;
     else
        ram_adr_o <=  ram_adr;   
     end if;        
   end process;

   
     -- RAM WREN Signals   
   gen_ram_a_we: for i in 3 downto 0 generate
	    ram_a_we(i)<='1' when wbs_cyc_i='1' and wbs_stb_i='1' and wbs_we_i='1' and wbs_sel_i(i)='1' 
	                           else '0';
  end generate;	
   
   
 --  genericMainMemory: if not UseBRAMPrimitives generate
   
      ram: entity work.MainMemory 
        generic map (
           ADDR_WIDTH =>ram_adr_width,
           SIZE => ram_size,
           RamFileName => RamFileName,
           mode => mode,
           Swapbytes => Swapbytes,
           EnableSecondPort => false            
        )
           
      PORT MAP(
         DBOut =>wbs_dat_o,
         DBIn => wbs_dat_i,
         AdrBus => ram_adr_o,
         ENA => wbs_cyc_i,
         WREN => ram_a_we,
         CLK => clk_i,
         CLKB =>clk_i ,
         ENB =>'0' ,
         AdrBusB =>(others=>'0'),
         DBOutB => open
      );
 
  
  
end Behavioral;

