----------------------------------------------------------------------------------

-- Create Date: 04/03/2019
-- Design Name:
-- Module Name: bonfire_dcache_set - Behavioral

-- The Bonfire Processor Project, (c) 2016-2019 Thomas Hornschuh

--

-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.log2;

entity bonfire_dcache_set is

generic(

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
  tag_valid_o : out std_logic;
  buffer_index_o : out unsigned(CACHE_ADR_BITS-CL_BITS-1 downto 0)

);

end bonfire_dcache_set;


architecture Behavioral of bonfire_dcache_set is

constant LINE_SELECT_ADR_BITS : natural := CACHE_ADR_BITS-CL_BITS; -- adr bits for selecting a cache line
constant TAG_RAM_SIZE : natural := log2.power2(LINE_SELECT_ADR_BITS); -- the Tag RAM size is defined by the size of line select address
constant MUX_SIZE : natural  :=MASTER_WIDTH_BYTES / 4; -- Multiplex factor from memory bus to CPU bus
constant CL_BITS_SLAVE : natural := CL_BITS + log2.log2(MUX_SIZE);


subtype t_tag_value is unsigned(TAG_RAM_BITS-1 downto 0);
subtype t_dirty_bits is std_logic_vector(MASTER_WIDTH_BYTES-1 downto 0);

type t_tag_data is record
   valid : std_logic;
   dirty : std_logic;
   address : t_tag_value;
end record;

constant tag_rec_len:natural:= 1+t_tag_value'length+1;

subtype t_tag_bits is std_logic_vector(tag_rec_len-1 downto 0);

constant init_dirty_bits : t_dirty_bits := (others=>'0');
constant init_tag_data : t_tag_data := ('0','0',to_unsigned(0,t_tag_value'length));

type t_tag_ram is array (0 to TAG_RAM_SIZE-1) of t_tag_bits;
--type t_cache_ram is array (0 to CACHE_SIZE-1) of std_logic_vector(MASTER_DATA_WIDTH-1 downto 0);

signal tag_value : t_tag_value;
signal tag_index : unsigned(LINE_SELECT_ADR_BITS-1 downto 0); -- Offset into TAG RAM

signal tag_ram : t_tag_ram := (others =>(others=> '0')) ;
attribute ram_style: string; -- for Xilinx
attribute ram_style of tag_ram: signal is  "distributed"; -- "block";

signal tag_we : std_logic:='0'; -- Tag RAM Write Enable - updates Tag RAM



signal wbs_enable : std_logic;

signal tag_buffer : t_tag_data; -- last buffered tag value
signal buffer_index : unsigned(LINE_SELECT_ADR_BITS-1 downto 0); -- index of last buffered tag value

signal tag_out,tag_in : t_tag_bits; -- input and output of tag RAM

signal hit,miss : std_logic;

function to_tag_bits(t:t_tag_data) return t_tag_bits is
  variable r: t_tag_bits;
  begin
    r(r'high):=t.valid;
    r(r'high-1):=t.dirty;
    r(r'high-2 downto 0):=std_logic_vector(t.address);
    return r;
  end function;

  function to_tag_data(t:t_tag_bits) return t_tag_data is
  variable r:t_tag_data;
  begin
    r.valid:=t(t'high);
    r.dirty:=t(t'high-1);
    r.address:=unsigned(t(t'high-2 downto 0));
    return r;
  end function;

begin

-- Output assignments

  tag_index_o <= tag_index;
  hit_o <= hit;
  miss_o <= miss;
  tag_value_o <= tag_buffer.address;
  tag_valid_o <= tag_buffer.valid;
  buffer_index_o <= buffer_index;

-- Tag assignments

  tag_value <= unsigned(adr_i(adr_i'high downto adr_i'high-TAG_RAM_BITS+1));
  tag_index <= unsigned (adr_i(tag_index'length+CL_BITS_SLAVE+adr_i'low-1 downto CL_BITS_SLAVE+adr_i'low));

 -- mapping between tag bitstring and record, needed because of synthesis limiations
 tag_in <= to_tag_bits( (valid_i,dirty_i,tag_value));
 tag_buffer <= to_tag_data(tag_out);



 check_hitmiss : process( tag_value,tag_buffer,buffer_index,tag_index,en_i )
    variable index_match,tag_match : boolean;

    begin
      index_match:= buffer_index = tag_index;
      tag_match:=tag_buffer.valid='1' and tag_buffer.address=tag_value;


      if  index_match and tag_match and en_i='1' then
        hit <= '1';
      else
        hit <= '0';
      end if;

      -- A miss only occurs when the tag buffer contains data for the right index but
      -- the tag itself does not match
      if en_i='1' and index_match and not tag_match then
        miss <= '1';
        dirty_miss_o <= tag_buffer.dirty;
      else
        miss <= '0';
        dirty_miss_o <= '0';
      end if;
    end process;


    proc_tag_ram:process(clk_i)

    variable rd,wd : t_tag_data;
    begin
      if rising_edge(clk_i) then
        if rst_i='1' then
           tag_out <= to_tag_bits(init_tag_data);
        else
           if we_i='1' then
             tag_ram(to_integer(tag_index))<=tag_in;
             tag_out <= tag_in; -- write first RAM...
           else
             tag_out <= tag_ram(to_integer(tag_index));
           end if;
        end if;
        buffer_index <= tag_index;

      end if;

    end process;


end Behavioral;
