----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date:    17:30:47 02/18/2017
-- Design Name:
-- Module Name:    wb_spi_interface - Behavioral
-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh
-- Simple SPI Interface


-- registers:
-- base+0   -- chip select control; bit 0 is slave_cs
-- base+4   -- status register; bit 0 indicates "transmitter busy"
-- base+8   -- transmitter: write a byte here, starts SPI bus transaction
-- base+0x0C   -- receiver: last byte received (updated on each transation)
-- base+0x10   -- clock divider: SPI CLK is clk_i/2*(1+n) ie for 128MHz clock, divisor 0 is 64MHz, 1 is 32MHz, 3 is 16MHz etc


-- License: See LICENSE or LICENSE.txt File in git project root. 
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

entity wb_spi_interface is
generic (
   CLK_FREQUENCY : natural := (96 * 1000000);
   WB_DATA_WIDTH : natural :=8;
   ADR_HIGH : natural :=4;
   ADR_LOW  : natural :=2
);
port (
      clk_i : in std_logic;
      reset_i : in std_logic;

      -- SPI Port:
      slave_cs_o         : out std_logic;
      slave_clk_o        : out std_logic;
      slave_mosi_o       : out std_logic;
      slave_miso_i       : in  std_logic;

      -- Interrupt signal:
      irq : out std_logic;

      -- Wishbone ports:
      wb_adr_in  : in  std_logic_vector(ADR_HIGH downto ADR_LOW);
      wb_dat_in  : in  std_logic_vector(WB_DATA_WIDTH-1 downto 0);
      wb_dat_out : out std_logic_vector(WB_DATA_WIDTH-1 downto 0);
      wb_we_in   : in  std_logic;
      wb_cyc_in  : in  std_logic;
      wb_stb_in  : in  std_logic;
      wb_ack_out : out std_logic
);
end wb_spi_interface;

architecture Behavioral of wb_spi_interface is

COMPONENT spimaster
    PORT(
        clk : IN std_logic;
        reset : IN std_logic;
        cpu_address : IN std_logic_vector(2 downto 0);
        data_in : IN std_logic_vector(7 downto 0);
        enable : IN std_logic;
        req_read : IN std_logic;
        req_write : IN std_logic;
        slave_miso : IN std_logic;
        cpu_wait : OUT std_logic;
        data_out : OUT std_logic_vector(7 downto 0);
        slave_cs : OUT std_logic;
        slave_clk : OUT std_logic;
        slave_mosi : OUT std_logic
        );
    END COMPONENT;


signal req_read,req_write,enable,cpu_wait : std_logic;


begin

  enable <= wb_cyc_in and wb_stb_in;
  req_read <= enable and not wb_we_in;
  req_write <= wb_we_in;

  wb_ack_out <= enable and not cpu_wait;

  i_spimaster: spimaster PORT MAP(
        clk => clk_i,
        reset => reset_i,
        cpu_address => wb_adr_in(ADR_LOW+2 downto ADR_LOW),
        cpu_wait => cpu_wait,
        data_in => wb_dat_in(7 downto 0),
        data_out => wb_dat_out(7 downto 0),
        enable => enable,
        req_read => req_read,
        req_write => req_write,
        slave_cs => slave_cs_o,
        slave_clk => slave_clk_o,
        slave_mosi => slave_mosi_o,
        slave_miso => slave_miso_i
    );


end Behavioral;

