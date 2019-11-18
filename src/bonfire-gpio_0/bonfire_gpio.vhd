----------------------------------------------------------------------------------

-- Module Name:    bonfire-gpio - Behavioral

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh

--  GPIO Interface compatible with SiFive FE310-G000 GPIO
--  See chapter 17. of the SiFive FE310-G000 Manual


-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity bonfire_gpio is
GENERIC (

    maxIObit : natural := 7;  -- Adr bus  high
    minIObit : natural := 2;   -- Adr bus  low
    NUM_GPIO_BITS : natural := 32

);
PORT (
    -- Wishbone Bus
    wb_clk_i: in std_logic;
    wb_rst_i: in std_logic;
    wb_dat_o: out std_logic_vector(31 downto 0);
    wb_dat_i: in std_logic_vector(31 downto 0);
    wb_adr_i: in std_logic_vector(maxIObit downto minIObit);
    wb_we_i:  in std_logic;
    wb_cyc_i: in std_logic;
    wb_stb_i: in std_logic;
    wb_ack_o: out std_logic;

    -- irq outs
    rise_irq_o : out std_logic;
    fall_irq_o : out std_logic;
    high_irq_o : out std_logic;
    low_irq_o : out std_logic;


    -- GPIO
    gpio_o : out std_logic_vector(NUM_GPIO_BITS-1 downto 0);
    gpio_i : in  std_logic_vector(NUM_GPIO_BITS-1 downto 0);
    gpio_t : out std_logic_vector(NUM_GPIO_BITS-1 downto 0)
);

attribute opt_mode: string;
attribute opt_mode of bonfire_gpio : entity is "area";

end bonfire_gpio;

architecture Behavioral of bonfire_gpio is

attribute keep_hierarchy : string;
attribute keep_hierarchy of Behavioral: architecture is "TRUE";

-- Attribute Infos for Xilinx Vivado IP Integrator Block designs
-- Should not have negative influence on other platforms.

ATTRIBUTE X_INTERFACE_INFO : STRING;
ATTRIBUTE X_INTERFACE_INFO of  wb_clk_i : SIGNAL is "xilinx.com:signal:clock:1.0 wb_clk_i CLK";
--X_INTERFACE_INFO of  wb_rst_i : SIGNAL is "xilinx.com:signal:reset:1.0 wb_rst_i RESET";

ATTRIBUTE X_INTERFACE_PARAMETER : STRING;
ATTRIBUTE X_INTERFACE_PARAMETER of wb_clk_i : SIGNAL is "ASSOCIATED_BUSIF WB_SLAVE";
--ATTRIBUTE X_INTERFACE_PARAMETER of rst_i : SIGNAL is "ASSOCIATED_BUSIF WB_DB";

ATTRIBUTE X_INTERFACE_INFO OF wb_cyc_i: SIGNAL IS "bonfire.eu:wb:Wishbone_master:1.0 WB_SLAVE wb_dbus_cyc_o";
ATTRIBUTE X_INTERFACE_INFO OF wb_stb_i: SIGNAL IS "bonfire.eu:wb:Wishbone_master:1.0 WB_SLAVE wb_dbus_stb_o";
ATTRIBUTE X_INTERFACE_INFO OF wb_we_i: SIGNAL IS "bonfire.eu:wb:Wishbone_master:1.0  WB_SLAVE wb_dbus_we_o";
ATTRIBUTE X_INTERFACE_INFO OF wb_ack_o: SIGNAL IS "bonfire.eu:wb:Wishbone_master:1.0 WB_SLAVE wb_dbus_ack_i";
ATTRIBUTE X_INTERFACE_INFO OF wb_adr_i: SIGNAL IS "bonfire.eu:wb:Wishbone_master:1.0 WB_SLAVE wb_dbus_adr_o";
ATTRIBUTE X_INTERFACE_INFO OF wb_dat_i: SIGNAL IS "bonfire.eu:wb:Wishbone_master:1.0 WB_SLAVE wb_dbus_dat_o";
ATTRIBUTE X_INTERFACE_INFO OF wb_dat_o: SIGNAL IS "bonfire.eu:wb:Wishbone_master:1.0 WB_SLAVE wb_dbus_dat_i";

ATTRIBUTE  X_INTERFACE_INFO OF gpio_o: SIGNAL IS "xilinx.com:interface:gpio:1.0 GPIO TRI_O";
ATTRIBUTE  X_INTERFACE_INFO OF gpio_i: SIGNAL IS "xilinx.com:interface:gpio:1.0 GPIO TRI_I";
ATTRIBUTE  X_INTERFACE_INFO OF gpio_t: SIGNAL IS "xilinx.com:interface:gpio:1.0 GPIO TRI_T";


subtype t_gpio_bits is std_logic_vector(NUM_GPIO_BITS-1 downto 0);

subtype t_dbus  is std_logic_vector(wb_dat_o'high downto wb_dat_o'low);


-- Register addresses

type t_regnum is (tr_value,tr_input_en,tr_output_en,tr_port,tr_pue,
                  tr_ds,tr_rise_ie,
                  tr_rise_ip,tr_fall_ie,tr_fall_ip,tr_high_ie,tr_high_ip,
                  tr_low_ie,tr_low_ip,
                  tr_iof_en,tr_iof_sel,tr_out_xor);

function get_regnum(adr: std_logic_vector) return t_regnum is
variable r: t_regnum;
begin

  r:= t_regnum'VAL(to_integer(unsigned(adr(minIObit+5 downto minIObit))));
  return r;
end;


function request_interrupt(pending: t_gpio_bits;enabled : t_gpio_bits) return std_logic is
variable r: std_logic;
begin
   r:='0';
   for i in pending'range loop
     r := r or (pending(i) and enabled(i));
   end loop;
   return r;

end;


-- Registers
signal  input_en : t_gpio_bits:= (others=>'0');
signal  output_en : t_gpio_bits:= (others=>'0');
signal  port_reg : t_gpio_bits:= (others=>'0');
signal  rise_ie : t_gpio_bits:= (others=>'0');
signal  rise_ip : t_gpio_bits:= (others=>'0');
signal  fall_ie : t_gpio_bits:= (others=>'0');
signal  fall_ip : t_gpio_bits:= (others=>'0');
signal  high_ie :t_gpio_bits:= (others=>'0');
signal  high_ip : t_gpio_bits:= (others=>'0');
signal  low_ie : t_gpio_bits:= (others=>'0');
signal  low_ip : t_gpio_bits:= (others=>'0');
signal  out_xor : t_gpio_bits:= (others=>'0');


-- Inputs from port bit module
signal pin_values : t_gpio_bits;
signal rise_ip_i : t_gpio_bits;
signal fall_ip_i : t_gpio_bits;

COMPONENT gpio_bit
    PORT(
        rst_i : IN std_logic;
        clk_i : IN std_logic;
        input_en_i : IN std_logic;
        output_en_i : IN std_logic;
        port_value_i : IN std_logic;
        out_xor_i : IN std_logic;
        iob_i : IN std_logic;
        pin_value_o : OUT std_logic;
        rising_o : OUT std_logic;
        falling_o : OUT std_logic;
        iob_o : OUT std_logic;
        iob_t : OUT std_logic
        );
    END COMPONENT;

   function fill_bits(v: t_gpio_bits) return t_dbus is
   variable r : t_dbus;
   begin
     r(v'range):=v;
     r(r'high downto v'length) := (others=>'0');
     return r;
   end;


begin

portbits: for i in gpio_o'range generate

 i_gpio_bit: gpio_bit PORT MAP(
        rst_i => wb_rst_i,
        clk_i =>  wb_clk_i,
        pin_value_o => pin_values(i),
        input_en_i => input_en(i),
        output_en_i => output_en(i),
        port_value_i => port_reg(i),
        out_xor_i =>  out_xor(i),
        rising_o =>  rise_ip_i(i),
        falling_o => fall_ip_i(i),
        iob_o => gpio_o(i),
        iob_i => gpio_i(i),
        iob_t => gpio_t(i)
    );


end generate;


   wb_ack_o <= wb_cyc_i and wb_stb_i; -- zero wait state operation

   -- Read process

   process(wb_cyc_i,wb_stb_i,wb_we_i,wb_adr_i,
           pin_values,input_en,output_en,port_reg,
           rise_ie,rise_ip,fall_ie,fall_ip,high_ie,
           high_ip,low_ie,low_ip,out_xor )
   variable regnum : t_regnum;

   begin

     wb_dat_o <= (others=> 'X');
     if wb_cyc_i = '1' and wb_stb_i='1' and wb_we_i='0' then

       regnum := get_regnum(wb_adr_i);
       case regnum is
         when tr_value => wb_dat_o <= fill_bits(pin_values);
         when tr_input_en => wb_dat_o <= fill_bits(input_en);
         when tr_output_en => wb_dat_o <= fill_bits(output_en);
         when tr_port => wb_dat_o <= fill_bits(port_reg);
         when tr_rise_ie => wb_dat_o <= fill_bits(rise_ie);
         when tr_rise_ip => wb_dat_o <= fill_bits(rise_ip);
         when tr_fall_ie => wb_dat_o <= fill_bits(fall_ie);
         when tr_fall_ip => wb_dat_o <= fill_bits(fall_ip);
         when tr_high_ie => wb_dat_o <= fill_bits(high_ie);
         when tr_high_ip => wb_dat_o <= fill_bits(high_ip);
         when tr_low_ie => wb_dat_o <= fill_bits(low_ie);
         when tr_low_ip => wb_dat_o <= fill_bits(low_ip);
         when tr_out_xor => wb_dat_o <= fill_bits(out_xor);

         when others=>

       end case;

     end if;

   end process;


   -- write process
   process(wb_clk_i)
   variable regnum : t_regnum;

       procedure clear_pending(signal ip_reg: out t_gpio_bits;
                                dat : std_logic_vector(wb_dat_i'range)) is
       begin
         for i in ip_reg'range loop
           if dat(i)='1' then
             ip_reg(i) <= '0';
           end if;
         end loop;
       end;

       procedure set_pending(signal ip_reg : out t_gpio_bits;
                             port_ip:t_gpio_bits) is

       begin
         for i in ip_reg'range loop
           if port_ip(i)='1'  then
             ip_reg(i) <= '1';
           end if;
         end loop;
       end;

   begin
     if rising_edge(wb_clk_i) then

       -- Register pending bits from IO Port when enabled
       set_pending(rise_ip,rise_ip_i);
       set_pending(fall_ip,fall_ip_i);
       set_pending(high_ip,pin_values);
       set_pending(low_ip, not pin_values);


       if wb_cyc_i = '1' and wb_stb_i='1' and wb_we_i='1' then
          regnum := get_regnum(wb_adr_i);
          case regnum is
             -- when tr_value => -- read only
                when tr_input_en =>  input_en <= wb_dat_i(input_en'range);
                when tr_output_en => output_en <= wb_dat_i(output_en'range);
                when tr_port =>  port_reg <= wb_dat_i(port_reg'range);
                when tr_rise_ie => rise_ie <= wb_dat_i(rise_ie'range);
                when tr_rise_ip => clear_pending(rise_ip,wb_dat_i);
                when tr_fall_ie => fall_ie <= wb_dat_i(fall_ie'range);
                when tr_fall_ip => clear_pending(fall_ip,wb_dat_i);
                when tr_high_ie => high_ie <= wb_dat_i(high_ie'range);
                when tr_high_ip => clear_pending(high_ip,wb_dat_i);
                when tr_low_ie => low_ie <= wb_dat_i(low_ie'range);
                when tr_low_ip => clear_pending(low_ip,wb_dat_i);
                when tr_out_xor => out_xor <= wb_dat_i(out_xor'range);

                when others=>


          end case;


       end if;

     end if;

   end process;

-- IRQ Lines

  rise_irq_o <= request_interrupt(rise_ip,rise_ie);
  fall_irq_o <= request_interrupt(fall_ip,fall_ie);
  high_irq_o <= request_interrupt(high_ip,high_ie);
  low_irq_o  <= request_interrupt(low_ip,low_ie);


end Behavioral;
