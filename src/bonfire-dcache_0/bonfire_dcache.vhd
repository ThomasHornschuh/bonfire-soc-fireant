----------------------------------------------------------------------------------

-- Create Date: 06/28/2017 04:40:39 PM
-- Design Name:
-- Module Name: bonfire_dcache - Behavioral

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh

-- Data Cache with Wishbone interfaces.
-- Supports wide (>32Bit) Master Wishbone interface for DDR RAMs

-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.log2;
use work.bonfire_dcache_help.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity bonfire_dcache is
generic(
  MASTER_DATA_WIDTH : natural := 128; -- 16 Bytes....
  LINE_SIZE : natural :=4; -- Line size in MASTER_DATA_WIDTH  words
  CACHE_SIZE : natural :=2048; -- Cache Size in MASTER_DATA_WIDTH Bit words
  ADDRESS_BITS : natural := 30;  -- Number of bits of chacheable address range
  DEVICE_FAMILY : string :="";
  NUM_SETS : natural := 1 -- Number of Cache Sets
);
Port (
   clk_i: in std_logic;
   rst_i: in std_logic;

   -- Slave Interface (from CPU to Cache) -- fixed 32 Bit

   wbs_cyc_i : in std_logic ;
   wbs_stb_i : in std_logic ;
   wbs_we_i : in std_logic ;
   wbs_sel_i : in std_logic_vector (3 downto 0);
   wbs_ack_o : out std_logic ;
   wbs_adr_i : in std_logic_vector (ADDRESS_BITS+1 downto 2);

   wbs_dat_o : out std_logic_vector (31 downto 0);
   wbs_dat_i : in std_logic_vector (31 downto 0);

   -- Master Interface (from Cache to memory)
   wbm_cyc_o: out std_logic;
   wbm_stb_o: out std_logic;
   wbm_we_o : out std_logic;
   wbm_cti_o: out std_logic_vector(2 downto 0);
   wbm_bte_o: out std_logic_vector(1 downto 0);
   wbm_sel_o : out std_logic_vector(MASTER_DATA_WIDTH/8-1 downto 0);
   wbm_ack_i: in std_logic;
   wbm_adr_o: out std_logic_vector(ADDRESS_BITS+1 downto log2.log2(MASTER_DATA_WIDTH/8));
   wbm_dat_i: in std_logic_vector(MASTER_DATA_WIDTH-1 downto 0);
   wbm_dat_o: out std_logic_vector(MASTER_DATA_WIDTH-1 downto 0)


 );
end bonfire_dcache;

architecture Behavioral of bonfire_dcache is

--attribute keep_hierarchy : string;
--attribute keep_hierarchy of Behavioral: architecture is "TRUE";

constant spartan6_name : string := "SPARTAN6";

constant WORD_SELECT_BITS : natural := log2.log2(MASTER_DATA_WIDTH/32);
constant CL_BITS : natural :=log2.log2(LINE_SIZE); -- Bits for adressing a word in a cache line
constant SET_ADR_BITS : natural := log2.log2(NUM_SETS);
constant DIRECT_MAPPED : boolean := NUM_SETS = 1;
constant CACHE_ADR_BITS : natural := log2.log2(CACHE_SIZE) - SET_ADR_BITS; -- total adress bits for cache

constant LINE_SELECT_ADR_BITS : natural := CACHE_ADR_BITS-CL_BITS; -- adr bits for selecting a cache line
constant TAG_RAM_SIZE : natural := log2.power2(LINE_SELECT_ADR_BITS); -- the Tag RAM size is defined by the size of line select address
constant TAG_RAM_BITS: natural := ADDRESS_BITS-LINE_SELECT_ADR_BITS-CL_BITS-WORD_SELECT_BITS;


constant LINE_MAX : std_logic_vector(CL_BITS-1 downto 0) := (others=>'1');

constant  MASTER_WIDTH_BYTES : natural := MASTER_DATA_WIDTH / 8 ; -- Number of Bytes for a memory bus word


-- Slave interface calculations
constant MUX_SIZE : natural  :=MASTER_DATA_WIDTH / 32; -- Multiplex factor from memory bus to CPU bus
constant CL_BITS_SLAVE : natural := log2.log2(LINE_SIZE*MUX_SIZE);

-- Cache address range in master address
constant CACHEADR_LOW : natural := wbm_adr_o'low;
constant CACHEADR_HI : natural  := CACHEADR_LOW+CACHE_ADR_BITS-1;


-- Generation options
constant gen_sp6_special : boolean := DEVICE_FAMILY = spartan6_name and CACHE_SIZE=2048 and MASTER_DATA_WIDTH=32;

-- Slave bus
signal slave_adr : std_logic_vector (wbs_adr_i'range);
signal wbs_enable : std_logic;
signal slave_rd_ack : std_logic :='0';
signal slave_write_enable : std_logic; -- combinatorial, slave write cycle enabled

-- Tag RAM Interface
signal tag_index : unsigned(LINE_SELECT_ADR_BITS-1 downto 0); -- Offset into TAG RAM
signal buffer_index : unsigned(LINE_SELECT_ADR_BITS-1 downto 0); -- index of last buffered tag value
signal hit,miss : std_logic;
signal write_back_enable : std_logic; -- combinatorial, actual tag line must be written back

-- Bus master signals

signal master_offset_counter : unsigned(CL_BITS-1 downto 0) :=to_unsigned(0,CL_BITS);
signal master_address : std_logic_vector(wbm_adr_o'range);
signal cache_offset_counter : unsigned(CL_BITS-1 downto 0);


signal wbm_enable : std_logic:='0';
signal cache_ram_out : std_logic_vector(wbm_dat_o'range);

type t_wbm_state is (wb_idle,wb_burst_read,wb_burst_write,wb_finish,wb_retire);

signal wbm_state : t_wbm_state:=wb_idle;

-- tag interface
signal tag_dirty, tag_valid, tag_we : std_logic;
signal tag_buffer_address : unsigned(TAG_RAM_BITS-1 downto 0);


-- Cache RAM Interface
subtype t_cacheadr is std_logic_vector(CACHE_ADR_BITS+SET_ADR_BITS-1 downto 0);

signal cache_AdrBus : t_cacheadr;
signal cache_DBOut,cache_DBIn : std_logic_vector (MASTER_DATA_WIDTH-1 downto 0);
signal cache_wren : std_logic_vector (MASTER_WIDTH_BYTES-1 downto 0);
signal slave_en_i, master_en_i, master_we_i : std_logic;
signal slave_cache_adr : t_cacheadr;

-- Set associative cache
subtype t_setselector is std_logic_vector(max(SET_ADR_BITS-1,0) downto 0);
signal selected_set : t_setselector;
signal slave_set : t_setselector;
signal master_set : t_setselector;


   function is_sel(adr: std_logic_vector (slave_adr'range);mux:natural) return boolean is
   variable res: boolean;
   begin
     if MUX_SIZE=1 then
       return true;
     else
       res:= mux=unsigned(adr(log2.log2(MUX_SIZE)-1+adr'low downto adr'low));
       return res;
     end if;

   end function;


begin

  assert (DEVICE_FAMILY=spartan6_name and gen_sp6_special) or
         (DEVICE_FAMILY /= spartan6_name )
  report "Module bonfire_dcache: On Spartan 6 generic synthesis of Cache RAM will most likely fail, use only CACHE_SIZE=2048 and MASTER_DATA_WIDTH=32 for a hard coded work around"
  severity warning;


  slave_adr <= wbs_adr_i; -- currently only an alias...
  wbs_enable <= wbs_cyc_i and wbs_stb_i;

  wbs_ack_o <= slave_rd_ack or slave_write_enable;

  slave_write_enable <= wbs_enable and wbs_we_i and hit;


  -- Wishbone ack signal for cache reads

  proc_slave_rd_ack: process(clk_i) begin

    if rising_edge(clk_i) then
      if slave_rd_ack='1' then
         slave_rd_ack <= '0';
      elsif hit='1' and wbs_enable='1' and wbs_we_i='0'  then
        slave_rd_ack<='1';
      end if;
    end if;

  end process;


  tag_we <= '1' when (wbm_ack_i='1' and wbm_state=wb_finish) or slave_write_enable = '1' else '0';
  tag_dirty <= slave_write_enable;
  tag_valid <= not write_back_enable;

  dm: if DIRECT_MAPPED generate

  inst_bonfire_dcache_set : entity work.bonfire_dcache_set
  generic map (
    CL_BITS            => CL_BITS,
    CACHE_ADR_BITS     => CACHE_ADR_BITS,
    TAG_RAM_BITS       => TAG_RAM_BITS,
    ADDRESS_BITS       => ADDRESS_BITS,
    MASTER_WIDTH_BYTES => MASTER_WIDTH_BYTES,
    DEVICE_FAMILY      => DEVICE_FAMILY
  )
  port map (
    clk_i   => clk_i,
    rst_i   => rst_i,
    adr_i   => slave_adr,
    en_i    => wbs_enable,
    we_i    => tag_we,
    dirty_i => tag_dirty,
    valid_i => tag_valid,
    tag_index_o => tag_index,
    hit_o   => hit,
    miss_o  => miss,
    dirty_miss_o => write_back_enable,
    tag_value_o => tag_buffer_address,
    buffer_index_o => buffer_index
  );


slave_cache_adr <= slave_adr(CACHEADR_HI  downto CACHEADR_LOW);

cache_AdrBus<= std_logic_vector(buffer_index) &
               std_logic_vector (cache_offset_counter) when write_back_enable='1'  else

               std_logic_vector(tag_index) & std_logic_vector(master_offset_counter);

end generate;


sa: if not DIRECT_MAPPED  generate
bonfire_dcache_multi_sets_i : entity work.bonfire_dcache_multi_sets
generic map (
  LOG2_SETS          => SET_ADR_BITS,
  CL_BITS            => CL_BITS,
  CACHE_ADR_BITS     => CACHE_ADR_BITS,
  TAG_RAM_BITS       => TAG_RAM_BITS,
  ADDRESS_BITS       => ADDRESS_BITS,
  MASTER_WIDTH_BYTES => MASTER_WIDTH_BYTES,
  DEVICE_FAMILY      => DEVICE_FAMILY
)
port map (
  clk_i   => clk_i,
  rst_i   => rst_i,
  adr_i   => slave_adr,
  en_i    => wbs_enable,
  we_i    => tag_we,
  dirty_i => tag_dirty,
  valid_i => tag_valid,
  tag_index_o => tag_index,
  hit_o   => hit,
  miss_o  => miss,
  dirty_miss_o => write_back_enable,
  tag_value_o => tag_buffer_address,
  buffer_index_o => buffer_index,
  selected_set_o => selected_set
);


slave_set <= selected_set;
master_set <= selected_set;

slave_cache_adr <= slave_set & slave_adr(CACHEADR_HI  downto CACHEADR_LOW);

cache_AdrBus<= master_set &
               std_logic_vector(buffer_index) &
               std_logic_vector (cache_offset_counter) when write_back_enable='1'  else

               master_set &  std_logic_vector(tag_index) & std_logic_vector(master_offset_counter);

end generate;



   cache_dbmux: for i in 0 to MUX_SIZE-1 generate
   begin
     -- For writing the Slave bus can just be demutiplexed n times
     -- Write Enable is done on byte lane level
     cache_DBIn((i+1)*32-1 downto i*32)<=wbs_dat_i;
   end generate;


 -- Interface to Cache RAM

   slave_en_i <= hit and wbs_enable;

   proc_cache_wren:process(wbs_sel_i,slave_adr,wbs_we_i) begin

    for i in 0 to MUX_SIZE-1 loop
      if is_sel(slave_adr,i) and wbs_we_i='1' then
        cache_wren((i+1)*4-1 downto i*4) <= wbs_sel_i;
      else
        cache_wren((i+1)*4-1 downto i*4) <= "0000";
      end if;
     end loop;

   end process;


   proc_cache_rdmux:process(cache_DBOut,slave_adr) begin
  -- Databus Multiplexer, select the 32 Bit word from the cache ram word.
     wbs_dat_o <= (others => 'X');
     for i in 0 to MUX_SIZE-1 loop
       if is_sel(slave_adr,i) then
         wbs_dat_o <= cache_DBOut((i+1)*32-1 downto i*32);
       end if;
     end loop;
   end process;




  master_en_i <= '1' when (wbm_ack_i='1' and wbm_enable='1') or
                         (write_back_enable='1' and wbm_state=wb_idle)
                     else '0';

  master_we_i <= wbm_ack_i and  not write_back_enable;


  cache_ram_generic: if not gen_sp6_special generate

  Inst_bonfire_dcache_cacheram: entity work.bonfire_dcache_cacheram
  GENERIC MAP (
    CACHE_SIZE => CACHE_SIZE,
    MASTER_DATA_WIDTH => MASTER_DATA_WIDTH,
    ATTR_KEEP_HIERARCHY => "FALSE"

  )
  PORT MAP(
      slave_db_i => cache_DBIn,
      slave_db_o => cache_DBOut,
      slave_wren_i => cache_wren,
      slave_en_i => slave_en_i ,
      slave_adr_i => slave_cache_adr,
      master_db_i => wbm_dat_i,
      master_db_o => cache_ram_out,
      master_we_i => master_we_i,
      master_en_i => master_en_i ,
      master_adr_i => cache_AdrBus,
      clk_i => clk_i
   );
end generate;


cache_ram_spartan6: if gen_sp6_special generate


  Inst_bonfire_dcache_cacheram: entity work.dcache_ram8K_spartan6
  GENERIC MAP (
    CACHE_SIZE => CACHE_SIZE,
    MASTER_DATA_WIDTH => MASTER_DATA_WIDTH,
    ATTR_KEEP_HIERARCHY => "FALSE"

  )
  PORT MAP(
      slave_db_i => cache_DBIn,
      slave_db_o => cache_DBOut,
      slave_wren_i => cache_wren,
      slave_en_i => slave_en_i ,
      slave_adr_i => slave_cache_adr,
      master_db_i => wbm_dat_i,
      master_db_o => cache_ram_out,
      master_we_i => master_we_i,
      master_en_i => master_en_i ,
      master_adr_i => cache_AdrBus,
      clk_i => clk_i
   );
end generate;



-- Master State engine

   wbm_cyc_o <=  wbm_enable;
   wbm_stb_o <=  wbm_enable;
   master_address <=  std_logic_vector(tag_buffer_address) &
                      std_logic_vector(buffer_index) &
                      std_logic_vector (master_offset_counter) when write_back_enable='1'
                      else slave_adr(master_address'high downto master_address'low+master_offset_counter'length) & std_logic_vector(master_offset_counter);
   wbm_adr_o <= master_address;

   wbm_dat_o <= cache_ram_out;



   master_rw: process(clk_i) -- Master cycle state engine
   variable n : unsigned(master_offset_counter'range);
   begin
     if rising_edge(clk_i) then
       if rst_i='1' then
            wbm_enable<='0';
            wbm_state<=wb_idle;
            master_offset_counter<=to_unsigned(0,master_offset_counter'length);
            cache_offset_counter<=to_unsigned(0,cache_offset_counter'length);
           -- tag_we <= '0';
        else
          case wbm_state is
            when wb_idle =>
              if miss='1' and hit='0' then
                wbm_enable<='1';

                wbm_sel_o <= (others=>'1');


                if write_back_enable='1'  then
                   cache_offset_counter<=master_offset_counter+1;
                   wbm_we_o<='1';
                   wbm_cti_o<="010";
                   wbm_state<=wb_burst_write;
                else
                   wbm_we_o<='0';
                   wbm_cti_o<="010";
                   wbm_state<=wb_burst_read;
                end if;
               end if;

             when  wb_burst_read|wb_burst_write =>
               n:=master_offset_counter+1;
               if  wbm_ack_i='1' then
                  if std_logic_vector(n)=LINE_MAX then
                     wbm_cti_o<="111";
                     wbm_state<=wb_finish;
                   end if;
                   master_offset_counter<=n;
                   cache_offset_counter<=n+1; -- used for address look ahead in write back mode
                end if;

             when wb_finish=>
               if  wbm_ack_i='1' then
                  wbm_enable<='0';
                  wbm_we_o<='0';
              --    tag_we<='1';
                  master_offset_counter<=to_unsigned(0,master_offset_counter'length);
                  cache_offset_counter<=to_unsigned(0,cache_offset_counter'length);
                  wbm_state<=wb_retire;
                end if;
            when wb_retire=>
            --  tag_we<='0';
              wbm_state<=wb_idle;
          end case;

        end if;
      end if;
   end process;


end Behavioral;
