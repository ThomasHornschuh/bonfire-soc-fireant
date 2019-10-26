----------------------------------------------------------------------------------

-- Create Date: 04/03/2019
-- Design Name:
-- Module Name: bonfire_dcache_Multi_sets - Behavioral

-- The Bonfire Processor Project, (c) 2016-2019 Thomas Hornschuh

--

-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.log2;
use work.bonfire_dcache_help.all;

entity bonfire_dcache_multi_sets is

generic(

  LOG2_SETS : natural :=0; -- 0= 1 set, 1= 2 sets, etc ...
  CL_BITS : natural; -- Bits for adressing a word in a cache line
  CACHE_ADR_BITS : natural; -- total adress bits for cache
  TAG_RAM_BITS : natural; -- number of address bits stored in tag ram

  ADDRESS_BITS : natural := 30;  -- Number of bits of chacheable address range
  MASTER_WIDTH_BYTES : natural := 4;
  DEVICE_FAMILY : string :=""
);
Port (
  clk_i: in std_logic;
  rst_i: in std_logic;

  adr_i : in std_logic_vector(ADDRESS_BITS+1 downto 2);
  en_i : in std_logic;

  we_i : in std_logic; -- Tag RAM write enable (update...)
  dirty_i : in std_logic;
  valid_i : in std_logic;

  tag_index_o : out unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);
  hit_o : out std_logic;
  miss_o : out std_logic;
  dirty_miss_o : out std_logic;
  tag_value_o : out unsigned(TAG_RAM_BITS-1 downto 0);
  buffer_index_o : out unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);

  selected_set_o: out std_logic_vector(max(LOG2_SETS-1,0) downto 0) -- Out: selected cache set to use

);

end entity;

architecture Behavioral of bonfire_dcache_multi_sets is

--attribute keep_hierarchy : string;
--attribute keep_hierarchy of Behavioral: architecture is "yes";


constant NUM_SETS : natural := 2 ** LOG2_SETS;
constant LINE_SELECT_ADR_BITS : natural := CACHE_ADR_BITS-CL_BITS; -- adr bits for selecting a cache line
constant TAG_RAM_SIZE : natural := 2 ** LINE_SELECT_ADR_BITS; -- the Tag RAM size is defined by the size of line select address

type t_set_interface is record

  tag_index_o :  unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);
  hit_o :  std_logic;
  miss_o :  std_logic;
  dirty_miss_o :  std_logic;
  tag_value_o :  unsigned(TAG_RAM_BITS-1 downto 0);
  tag_valid_o : std_logic;
  buffer_index_o :  unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0);

end record;

type  t_sets is array( 0 to NUM_SETS-1 ) of t_set_interface;
subtype t_setindex is natural range 0 to NUM_SETS-1;

subtype t_clockbits is std_logic_vector(NUM_SETS-1 downto 0);
type t_clockram is array (0 to TAG_RAM_SIZE-1 ) of t_clockbits;



signal sets : t_sets;
signal selected_set_index, hit_set : t_setindex;
--signal round_robin : unsigned(max(LOG2_SETS-1,0) downto 0) := ( others=> '0');
signal we : std_logic_vector( 0 to NUM_SETS-1 );
signal dirty_miss : std_logic;
signal hit : std_logic;
--signal miss : std_logic;
signal purge : std_logic; -- Valid cache line must be purged
signal buffer_index, tag_index : unsigned(LINE_SELECT_ADR_BITS-1 downto 0);

-- LRU algorihtm signals
signal clock_ram : t_clockram := (others =>(others=> '0'));
attribute ram_style: string; -- for Xilinx
attribute ram_style of clock_ram: signal is  "distributed"; -- "block";
signal clock : t_clockbits := (others=> '0');

signal next_clock : t_clockbits;
signal clock_hand : t_setindex := 0;
signal purge_set : t_setindex := 0;
signal purge_found : std_logic := '0';



begin

  assert NUM_SETS>1
    report "Using this module with NUM_SETS=1 is not supported"
    severity failure;

  buffer_index <= sets(0).buffer_index_o; -- all buffer indexes are identical...
  buffer_index_o <= buffer_index;
  tag_index <= sets(0).tag_index_o;
  tag_index_o <= tag_index;

  selected_set_index <= purge_set when purge_found='1' else hit_set;
  selected_set_o <= std_logic_vector(to_unsigned(selected_set_index,selected_set_o'length));
  miss_o <= purge_found;
  tag_value_o <= sets(selected_set_index).tag_value_o;
  dirty_miss <= sets(selected_set_index).dirty_miss_o;
  dirty_miss_o <= dirty_miss;

  hit_o <= hit;


  gensets: for i in sets'range generate


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
      adr_i   => adr_i,
      en_i    => en_i,
      we_i    => we(i),
      dirty_i => dirty_i,
      valid_i => valid_i,
      tag_index_o => sets(i).tag_index_o,
      hit_o   => sets(i).hit_o,
      miss_o  => sets(i).miss_o,
      dirty_miss_o => sets(i).dirty_miss_o,
      tag_value_o => sets(i).tag_value_o,
      tag_valid_o => sets(i).tag_valid_o,
      buffer_index_o =>sets(i).buffer_index_o
    );


  end generate;

  -- write enable multiplexer
  we_en_mux: process(selected_set_index,we_i)
  variable e : std_logic;
  begin

    for i in sets'range loop
      if  i = selected_set_index then
        e := we_i;
      else
        e := '0';
      end if;
     we(i) <= e;
    end loop;

  end process;


-- hit/mis logic and set selector
  find_set : process (en_i,sets)
  variable h : std_logic;
  variable found : boolean;

  begin
    h := '0';
    hit_set <= 0;
    purge <= '0';
    if en_i='1' then
      for i in sets'range loop
        if sets(i).hit_o = '1' then
          h := '1';
          hit_set <= i;
          exit;
        end if;
      end loop;
      if h = '0' and  sets(0).miss_o = '1' then
         purge <= '1';
      end if;
    end if;

    hit <= h;
  end process;


 -- Clock  algorihtm

  purge_found <= '1' when purge = '1' and clock(clock_hand) ='0'
                  else '0';

  purge_set <= clock_hand;
  p_next_clock_comb: process(en_i,hit,selected_set_index,purge,purge_found,clock_hand)

  begin

    next_clock <= clock;
     if en_i= '1' then
       if hit='1' then
          next_clock(selected_set_index) <= '1';
      elsif purge ='1' and purge_found = '0' and clock(clock_hand) = '1' then
          next_clock(clock_hand) <= '0';
      end if;
    end if;

  end process;


  p_clock_hand : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if purge ='1' and purge_found = '0' then
        clock_hand <= (clock_hand + 1 ) mod NUM_SETS;
      end if;
    end if;
  end process;


  p_clock_ram: process (clk_i)
  variable t : std_logic;
  begin

    if rising_edge(clk_i) then
      if hit = '1' or purge = '1' then
        clock_ram(to_integer(tag_index)) <= next_clock;
        clock <= next_clock;
      else
        clock <= clock_ram(to_integer(tag_index));
      end if;

    end if;

  end process;


end Behavioral;
