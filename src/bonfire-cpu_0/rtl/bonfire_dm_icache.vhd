----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date:    14:42:05 01/30/2017
-- Design Name:
-- Module Name:    bonfire_dm_icache - Behavioral
-- Project Name:
-- Target Devices:
-- Tool versions:
-- Description:
--   Bonfire CPU
--   (c) 2016,2017 Thomas Hornschuh
--   See license.md for License
--   Implements a direct-mapped Instruction Cache with variable size and line size
--   Designed as Plug-In Replacement for the orginal "ring-buffer" cache of lxp32
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
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

use work.log2;

entity bonfire_dm_icache is

generic (
  LINE_SIZE : natural :=8; -- Line size in 32 Bit words
  CACHE_SIZE : natural :=2048; -- Cache Size in 32 Bit words
  ADDRESS_BITS : natural := 30;  -- Number of bits of chacheable address range
  FIX_BUSY : boolean := false -- Fix lli_busy handling to be specification conform
);
port(
        clk_i: in std_logic;
        rst_i: in std_logic;

        -- Local Bus to Core
        lli_re_i: in std_logic;
        lli_adr_i: in std_logic_vector(29 downto 0);
        lli_dat_o: out std_logic_vector(31 downto 0);
        lli_busy_o: out std_logic;

        -- Wishbone Master to RAM
        wbm_cyc_o: out std_logic;
        wbm_stb_o: out std_logic;
        wbm_cti_o: out std_logic_vector(2 downto 0);
        wbm_bte_o: out std_logic_vector(1 downto 0);
        wbm_ack_i: in std_logic;
        wbm_adr_o: out std_logic_vector(29 downto 0);
        wbm_dat_i: in std_logic_vector(31 downto 0);

        -- Cache Control
        cc_invalidate_i : in std_logic;
        cc_invalidate_complete_o : out std_logic

       --dbus_cyc_snoop_i : std_logic -- TH
    );


end bonfire_dm_icache;

architecture Behavioral of bonfire_dm_icache is

constant CL_BITS : natural :=log2.log2(LINE_SIZE); -- Bits for cache line
constant CACHE_ADR_BITS : natural := log2.log2(CACHE_SIZE); -- total adress bits for cache
constant LINE_SELECT_ADR_BITS : natural := CACHE_ADR_BITS-CL_BITS; -- adr bits for selecting a cache line
constant TAG_RAM_SIZE : natural := log2.power2(LINE_SELECT_ADR_BITS); -- the Tag RAM size is defined by the size of line select address
constant TAG_RAM_BITS: natural := ADDRESS_BITS-CACHE_ADR_BITS; -- the Tag RAM needs to compare all remaining address bits
constant TAG_RAM_WIDTH : natural := TAG_RAM_BITS+1;  -- The Tag RAM will also contain a valid bit in is upper position

constant LINE_MAX : std_logic_vector(CL_BITS-1 downto 0) := (others=>'1');

subtype t_tag_value is unsigned(TAG_RAM_BITS-1 downto 0);

type t_tag_data is record
   valid : std_logic;
   address : t_tag_value;
end record;
type t_tag_ram is array (0 to TAG_RAM_SIZE-1) of std_logic_vector(TAG_RAM_WIDTH-1 downto 0);
type t_cache_ram is array (0 to CACHE_SIZE-1) of std_logic_vector(31 downto 0);

signal tag_value : t_tag_value;
signal tag_index,                   -- Offset into TAG RAM
       update_index,                -- Address for Cache invalidation run
       tag_adr : unsigned(LINE_SELECT_ADR_BITS-1 downto 0); -- Actual tag_adr, multiplexer output

signal tag_ram : t_tag_ram := (others => (others=> '0')) ;
attribute ram_style: string; -- for Xilinx
attribute ram_style of tag_ram: signal is  "distributed"; -- "block";

signal cache_ram : t_cache_ram;

signal adr :  std_logic_vector(29 downto 0);
signal adr_reg : std_logic_vector(29 downto 0);

signal tag_buffer : t_tag_data := ('0',others=>to_unsigned(0,t_tag_value'length)); -- last buffered tag value
signal buffer_index : unsigned(LINE_SELECT_ADR_BITS-1 downto 0); -- index of last buffered tag value

signal hit,miss : std_logic;

signal wb_enable : std_logic := '0';
signal re_reg : std_logic := '0' ;

signal read_offset_counter : unsigned(CL_BITS-1 downto 0);
signal read_address : std_logic_vector(wbm_adr_o'high downto 0);
signal read_cache_address_reg : std_logic_vector(CACHE_ADR_BITS-1 downto 0);

type t_wb_state is (wb_idle,wb_burst,wb_finish,run_invalidate); --wb_retire,

signal wb_state : t_wb_state;
signal running_invalidation : std_logic;

signal invalidate_requested : std_logic :='0'; -- register to store a active invalidate request
signal cc_invalidate_complete : std_logic := '0';

signal tag_we : std_logic; -- :='0'; -- tag RAM Write enable

signal busy : std_logic := '0';

begin


  wbm_cyc_o<=wb_enable;
  wbm_stb_o<=wb_enable;
  wbm_bte_o<="00";
  read_address<=adr(adr'high downto CL_BITS) & std_logic_vector(read_offset_counter);
  wbm_adr_o<=read_address;

  tag_value <= unsigned(adr(adr'high downto adr'high-TAG_RAM_BITS+1));
  tag_index <= unsigned(adr(LINE_SELECT_ADR_BITS+CL_BITS-1 downto CL_BITS));

  running_invalidation <= '1' when wb_state=run_invalidate else '0';

  tag_we <= '1' when  running_invalidation='1' or (wb_state=wb_finish and wbm_ack_i = '1') else '0';

  cc_invalidate_complete_o <= cc_invalidate_complete;


  check_hitmiss : process(tag_value,tag_buffer,buffer_index,tag_index,lli_re_i,re_reg,running_invalidation)
  variable index_match,tag_match : boolean;
  variable re : boolean;
  begin
    index_match:= buffer_index = tag_index;
    tag_match:=tag_buffer.valid='1' and tag_buffer.address=tag_value and not running_invalidation='1';
    re:= lli_re_i='1' or re_reg='1';

    if  index_match and tag_match and re then
      hit<='1';
    else
      hit<='0';
    end if;

    -- A miss only occurs when the tag buffer contains data for the right index but
    -- the tag itself does not match
    if re and index_match and not tag_match then
      miss<='1';
    else
      miss<='0';
    end if;
  end process;



fx_busy : if FIX_BUSY  generate

  lli_busy_o<=busy;
  adr <=  lli_adr_i when busy='0' else adr_reg;

  process(clk_i) begin
     if rising_edge(clk_i) then
       if rst_i='1' then
         re_reg <= '0';
         busy <= '0';
        else
          busy<= not hit and (lli_re_i or re_reg);
          if lli_re_i='1' and busy='0' then
            adr_reg<=lli_adr_i;
          end if;
          if lli_re_i='1' and hit='0' and re_reg='0' then
            re_reg<='1';

          elsif hit='1' then
            re_reg<='0';
          end if;
       end if;
     end if;
  end process;

end generate;

old_busy: if not FIX_BUSY generate

  lli_busy_o<= not hit and (lli_re_i or re_reg);
  adr <=  lli_adr_i;

  process(clk_i) begin
     if rising_edge(clk_i) then
        if lli_re_i='1' and hit='0' then
          re_reg<='1';
        elsif hit='1' then
          re_reg<='0';
        end if;
     end if;
  end process;

end generate;



  -- Tag address multiplexer
  tag_adr <= update_index when running_invalidation='1'
             else tag_index;

  proc_tag_ram:process(clk_i)

  variable rd,wd : std_logic_vector(TAG_RAM_WIDTH-1 downto 0);
  begin
    if rising_edge(clk_i) then
      if rst_i='1' then
         tag_buffer<= ('0',others=>to_unsigned(0,t_tag_value'length));
      else
         if tag_we='1' then
            if running_invalidation='1' then
              wd(wd'high):='0';
            else
              wd(wd'high):='1';
            end if;
            wd(TAG_RAM_BITS-1 downto 0):=std_logic_vector(tag_value);
            tag_ram(to_integer(tag_adr))<=wd;
            rd:=wd;
         else
            rd:=tag_ram(to_integer(tag_adr));
         end if;


        tag_buffer.valid<=rd(rd'high);
        tag_buffer.address<= unsigned(rd(TAG_RAM_BITS-1 downto 0));
        buffer_index<=tag_index;

      end if;
    end if;

  end process;



  proc_cache_ram: process(clk_i) begin

    if rising_edge(clk_i) then
      -- read cache
      if (lli_re_i='1' or re_reg='1' ) and (FIX_BUSY or hit='1') then -- hit='1' and
        lli_dat_o <= cache_ram(to_integer(unsigned(adr(CACHE_ADR_BITS-1 downto 0))));
      end if;
      -- read data from Wisbone bus into Cache RAM on ACK
      if wbm_ack_i='1' and wb_enable='1' then
        cache_ram(to_integer(unsigned(read_address(CACHE_ADR_BITS-1 downto 0))))<=wbm_dat_i;
      end if;
    end if;
  end process;


   register_invalidate: process(clk_i)
   begin

     if rising_edge(clk_i) then
       if rst_i='1' then
         invalidate_requested <= '1'; -- Reset will invalidate cache
       else
         if wb_state = run_invalidate then
           invalidate_requested <= '0';
         elsif cc_invalidate_i='1' then
           invalidate_requested <= '1';
         end if;
       end if;
     end if;

   end process;



  proc_wb_read: process(clk_i)
  variable n : unsigned(read_offset_counter'high downto 0);
  variable u_next : unsigned(update_index'range);
  begin
     if rising_edge(clk_i) then
       if rst_i='1' then
         wb_enable<='0';
         wb_state<=wb_idle;
         read_offset_counter<=to_unsigned(0,read_offset_counter'length);
        -- tag_we<='0';
         cc_invalidate_complete <= '0';
       else

         case wb_state is
           when wb_idle =>
             cc_invalidate_complete<='0';

             if cc_invalidate_i='1' or invalidate_requested='1' then
               wb_state <= run_invalidate;
               update_index <= to_unsigned(0,update_index'length);
               --tag_we<='1';

             elsif miss='1' and hit='0' then
               wb_enable<='1';

               read_offset_counter<=to_unsigned(0,read_offset_counter'length);
               -- Special case: Line Size 1...
               if LINE_SIZE=1 then
                 wb_state <= wb_finish;
                 wbm_cti_o<="000";
               else
                 wb_state <= wb_burst;
                 wbm_cti_o<="010";
               end if;
             end if;
           when wb_burst =>
             if  wbm_ack_i='1' then
                n:=read_offset_counter+1;
                if std_logic_vector(n)=LINE_MAX then
                  wbm_cti_o<="111";
                  wb_state<=wb_finish;
                 -- tag_we<='1';
                end if;
                read_offset_counter<=n;
             end if;
           when wb_finish=>
              if  wbm_ack_i='1' then
                wb_enable<='0';
                wb_state<=wb_idle;
               -- tag_we<='0';
              end if;
          -- when wb_retire=>
            --  wb_state<=wb_idle;
           when run_invalidate =>
              u_next:=update_index+1;
              update_index <= u_next;
              if u_next = 0 then
                 --tag_we<='0';
                 wb_state<=wb_idle;
                 cc_invalidate_complete<='1';
               end if;
         end case;
       end if;
     end if;
  end process;

end Behavioral;
