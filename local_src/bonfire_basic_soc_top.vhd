----------------------------------------------------------------------------------

-- Module Name:    bonfire_basic_soc - Behavioral

-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh

--
-- License: See LICENSE or LICENSE.txt File in git project root.
--
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

entity bonfire_basic_soc_top is
generic (
     RamFileName : string:="/home/thomas/development/bonfire/bonfire-software/test/mult.hex";
     BYPASS_CLKGEN : boolean := false

   );
   port(
        sysclk  : in  std_logic;
        o_resetn : out std_logic;
        i_locked : in std_logic;
        resetn : in std_logic;

        -- UART0 signals:
        uart0_txd : out std_logic;
        uart0_rxd : in  std_logic :='1';

        LED : out std_logic_vector(3 downto 0)


    );
end bonfire_basic_soc_top;

architecture Behavioral of bonfire_basic_soc_top is

constant NUM_GPIO : natural := 4;

constant TOTAL_GPIO : natural := NUM_GPIO;
constant BRAM_ADR_WIDTH : natural := 11;



constant reset_adr : std_logic_vector(31 downto 0) :=X"0C000000";

component bonfire_basic_soc is
  generic (
    ENABLE_EXT_RAM  : boolean := false;
    ENABLE_UART1    : boolean := false;
    ENABLE_SPI      : boolean := false;
    ENABLE_DCACHE   : boolean := false;
    ENABLE_GPIO     : boolean := true;
    UART_FIFO_DEPTH : natural := 6;
    BRAM_ADR_WIDTH  : natural := 13;
    BurstSize       : natural := 8;
    CacheSizeWords  : natural := 512;
    DCacheSizeWords : natural := 512;
    BRAM_ADR_BASE : std_logic_vector(7 downto 0) := X"0C";
    MUL_ARCH: string := "spartandsp";
    REG_RAM_STYLE : string := "block";
    NUM_GPIO        : natural := 8;
    DEVICE_FAMILY : string :=  ""
  );
  port (
    clk_i          : in  std_logic;
    reset_i        : in  std_logic;
    bram_dba_i     : in  std_logic_vector(31 downto 0);
    bram_dba_o     : out std_logic_vector(31 downto 0);
    bram_adra_o    : out std_logic_vector(BRAM_ADR_WIDTH-1 downto 0);
    bram_ena_o     : out STD_LOGIC;
    bram_wrena_o   : out STD_LOGIC_VECTOR (3 downto 0);
    bram_dbb_i     : in  std_logic_vector(31 downto 0);
    bram_adrb_o    : out std_logic_vector(BRAM_ADR_WIDTH-1 downto 0);
    bram_enb_o     : out STD_LOGIC;
    wbm_cyc_o      : out std_logic;
    wbm_stb_o      : out std_logic;
    wbm_we_o       : out std_logic;
    wbm_cti_o      : out std_logic_vector(2 downto 0);
    wbm_bte_o      : out std_logic_vector(1 downto 0);
    wbm_sel_o      : out std_logic_vector(3 downto 0);
    wbm_ack_i      : in  std_logic;
    wbm_adr_o      : out std_logic_vector(25 downto 2);
    wbm_dat_i      : in  std_logic_vector(31 downto 0);
    wbm_dat_o      : out std_logic_vector(31 downto 0);
    uart0_txd : out std_logic;
    uart0_rxd      : in  std_logic;
    uart1_txd : out std_logic;
    uart1_rxd      : in  std_logic;
    flash_spi_cs   : out std_logic;
    flash_spi_clk  : out std_logic;
    flash_spi_mosi : out std_logic;
    flash_spi_miso : in  std_logic;
    gpio_o         : out std_logic_vector(NUM_GPIO-1 downto 0);
    gpio_i         : in  std_logic_vector(NUM_GPIO-1 downto 0);
    gpio_t         : out std_logic_vector(NUM_GPIO-1 downto 0)
  );
end component;



signal clk32Mhz,   -- buffered osc clock
       clk,        -- logical CPU clock

       uart_clk    : std_logic;


signal reset,res1,res2  : std_logic;

signal clk_locked : std_logic;


signal bram_dba_i     : std_logic_vector(31 downto 0);
signal bram_dba_o     : std_logic_vector(31 downto 0);
signal bram_adra_o    : std_logic_vector(BRAM_ADR_WIDTH-1 downto 0);
signal bram_ena_o     : STD_LOGIC;
signal bram_wrena_o   : STD_LOGIC_VECTOR (3 downto 0);
signal bram_dbb_i     : std_logic_vector(31 downto 0);
signal bram_adrb_o    : std_logic_vector(BRAM_ADR_WIDTH-1 downto 0);
signal bram_enb_o     : STD_LOGIC;
signal wbm_cyc_o      : std_logic;
signal wbm_stb_o      : std_logic;
signal wbm_we_o       : std_logic;
signal wbm_cti_o      : std_logic_vector(2 downto 0);
signal wbm_bte_o      : std_logic_vector(1 downto 0);
signal wbm_sel_o      : std_logic_vector(3 downto 0);
signal wbm_ack_i      : std_logic;
signal wbm_adr_o      : std_logic_vector(25 downto 2);
signal wbm_dat_i      : std_logic_vector(31 downto 0);
signal wbm_dat_o      : std_logic_vector(31 downto 0);

signal gpio_o         : std_logic_vector(NUM_GPIO-1 downto 0);
signal gpio_i         : std_logic_vector(NUM_GPIO-1 downto 0);
signal gpio_t         : std_logic_vector(NUM_GPIO-1 downto 0);



signal irq_i : std_logic_vector(7 downto 0);


begin

  LED(0) <= not gpio_o(0);
  LED(1) <= not gpio_o(1);
  LED(2) <= not gpio_o(2);
  LED(3) <= not gpio_o(3);




  bonfire_basic_soc_i : bonfire_basic_soc
  generic map (
    ENABLE_EXT_RAM  => false,
    ENABLE_UART1    => false,
    UART_FIFO_DEPTH => 6,
    ENABLE_SPI      => false,
    ENABLE_DCACHE   => false,
    BRAM_ADR_WIDTH  => BRAM_ADR_WIDTH,
    BurstSize       => 1,
    CacheSizeWords  => 0,
    DCacheSizeWords => 0,
    NUM_GPIO        => NUM_GPIO
  )
  port map (
    clk_i          => clk,
    reset_i        => reset,
    bram_dba_i     => bram_dba_i,
    bram_dba_o     => bram_dba_o,
    bram_adra_o    => bram_adra_o,
    bram_ena_o     => bram_ena_o,
    bram_wrena_o   => bram_wrena_o,
    bram_dbb_i     => bram_dbb_i,
    bram_adrb_o    => bram_adrb_o,
    bram_enb_o     => bram_enb_o,
    wbm_cyc_o      => wbm_cyc_o,
    wbm_stb_o      => wbm_stb_o,
    wbm_we_o       => wbm_we_o,
    wbm_cti_o      => wbm_cti_o,
    wbm_bte_o      => wbm_bte_o,
    wbm_sel_o      => wbm_sel_o,
    wbm_ack_i      => wbm_ack_i,
    wbm_adr_o      => wbm_adr_o,
    wbm_dat_i      => wbm_dat_i,
    wbm_dat_o      => wbm_dat_o,
    uart0_txd      => uart0_txd,
    uart0_rxd      => uart0_rxd,
    uart1_txd      => open,
    uart1_rxd      => '1',
    flash_spi_cs   => open,
    flash_spi_clk  => open,
    flash_spi_mosi => open,
    flash_spi_miso => '0',
    gpio_o         => gpio_o,
    gpio_i         => gpio_i,
    gpio_t         => gpio_t
  );


ram: entity work.main_memory_laned
      generic map (
         ADDR_WIDTH =>BRAM_ADR_WIDTH,
         RamFileName => RamFileName,
         mode => "H",
         EnableSecondPort => true
      )

    PORT MAP(
       DBOut =>   bram_dba_i,
       DBIn =>    bram_dba_o,
       AdrBus =>  bram_adra_o,
       ENA =>     bram_ena_o,
       WREN =>    bram_wrena_o,
       CLK =>     clk,
       CLKB =>    clk,
       ENB =>     bram_enb_o,
       AdrBusB => bram_adrb_o,
       DBOutB =>  bram_dbb_i
    );



-- Clock and reset

clk <= sysclk;
o_resetn <= '1'; -- No Reset for PLL

process(clk) begin
   if rising_edge(clk) then
      res1 <= not resetn or not i_locked;
      res2 <= res1;
      reset <= res2;
   end if;
end process;





end Behavioral;
