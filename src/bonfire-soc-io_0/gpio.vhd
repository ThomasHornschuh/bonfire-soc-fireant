----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date:    00:05:00 09/05/2016
-- Design Name:
-- Module Name:    gpio - Behavioral
-- Project Name:
-- Target Devices:
-- Tool versions:
-- Description:
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
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity gpio is
generic(
    wbs_adr_high : natural := 27;
    NUM_GPIO_BITS : natural := 32
);
port(
      -- output

        leds: out std_logic_vector(NUM_GPIO_BITS-1 downto 0);

      -- bus interface
        clk_i: in std_logic;
        rst_i: in std_logic;

        wbs_cyc_i: in std_logic;
        wbs_stb_i: in std_logic;
        wbs_we_i: in std_logic;
        wbs_sel_i: in std_logic_vector(3 downto 0);
        wbs_ack_o: out std_logic;
        wbs_adr_i: in std_logic_vector(wbs_adr_high downto 2);
        wbs_dat_i: in std_logic_vector(31 downto 0);
        wbs_dat_o: out std_logic_vector(31 downto 0)
    );
    end gpio;

architecture Behavioral of gpio is



begin

   wbs_dat_o <= (others=>'0');

   process(clk_i) begin
     if rising_edge(clk_i) then
        if rst_i='1' then
           leds<=(others=>'0');
        end if;

        if wbs_cyc_i='1' and wbs_stb_i='1' then
            if wbs_we_i='1' and wbs_sel_i="1111" then
              leds <= wbs_dat_i(leds'range);
            end if;
          wbs_ack_o<='1';
        else
          wbs_ack_o<='0';
      end if;
    end if;
  end process;

end Behavioral;

