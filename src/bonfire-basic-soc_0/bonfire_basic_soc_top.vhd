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
     RamFileName : string:="";    -- :="compiled_code/monitor.hex";
     mode : string := "H";       -- only used when UseBRAMPrimitives is false
     LANED_RAM : boolean := false; -- Implement RAM in Byte Lanes
     Swapbytes : boolean := true; -- SWAP Bytes in RAM word in low byte first order to use data2mem
     ExtRAM : boolean := false; -- "Simulate" External RAM as Bock RAM
     BurstSize : natural := 8;
     CacheSizeWords : natural := 512; -- 2KB Instruction Cache
     EnableDCache : boolean := false;
     DCacheSizeWords : natural := 512;
     MUL_ARCH: string := "spartandsp";
     REG_RAM_STYLE : string := "block";
     NUM_GPIO   : natural := 8;
     DEVICE_FAMILY : string :=  "";
     BYPASS_CLKGEN : boolean := false -- When TRUE use sysclk input directly as CPU clock

   );
   port(
        sysclk  : in  std_logic;
        I_RESET   : in  std_logic;

        -- UART0 signals:
        uart0_txd : out std_logic;
        uart0_rxd : in  std_logic :='1';

        -- UART1 signals:
        uart1_txd : out std_logic;
        uart1_rxd : in  std_logic :='1';

        -- SPI flash chip
        flash_spi_cs        : out   std_logic;
        flash_spi_clk       : out   std_logic;
        flash_spi_mosi      : out   std_logic;
        flash_spi_miso      : in    std_logic;


        -- GPIO pads - assign with UCF/XDC File
        GPIO   : inout STD_LOGIC_VECTOR(NUM_GPIO-1 downto 0)
    );
end bonfire_basic_soc_top;

architecture Behavioral of bonfire_basic_soc_top is



 constant ram_adr_width : natural := 13;
 constant ram_size : natural := 8192;


 constant reset_adr : std_logic_vector(31 downto 0) :=X"0C000000";


signal clk : std_logic;       -- logical CPU clock
signal reset,res1,res2  : std_logic;
signal clk_locked : std_logic;


-- gpio ports


constant TOTAL_GPIO : natural := NUM_GPIO;
constant BRAM_ADR_WIDTH : natural := 13;


signal irq_i : std_logic_vector(7 downto 0);

component bonfire_basic_soc is
  generic (
    ENABLE_EXT_RAM  : boolean := false;
    ENABLE_UART1    : boolean := false;
    ENABLE_SPI      : boolean := false;
    ENABLE_DCACHE   : boolean := false;
    BRAM_ADR_WIDTH  : natural := 13;
    BurstSize       : natural := 8;
    CacheSizeWords  : natural := 512;
    DCacheSizeWords : natural := 512;
    BRAM_ADR_BASE : std_logic_vector(7 downto 0) := X"0C";
    MUL_ARCH        : string;
    REG_RAM_STYLE   : string;
    NUM_GPIO        : natural := 8;
    DEVICE_FAMILY   : string
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


 component clkgen_arty
 port
 (-- Clock in ports
  -- Clock out ports
  clkout          : out    std_logic;
  -- Status and control signals
  reset             : in     std_logic;
  locked            : out    std_logic;
  sysclk           : in     std_logic
 );
end component;

signal  clkgen_rst: std_logic;



begin
   assert TOTAL_GPIO <= 32
     report "Total number of gpio ports cannot exceed 32"
     severity failure;

   -- Assignment of IOBs for GPIO


   gpio_pads: for i in GPIO'range generate
     pad : entity work.gpio_pad

     port map (
        O => gpio_i(i),   -- Buffer output
        IO => GPIO(i),    -- Buffer inout port (connect directly to top-level port)
        I => gpio_o(i),   -- Buffer input
        T => gpio_t(i)    -- 3-state enable input, high=input, low=output
     );

   end generate;



   bonfire_basic_soc_i : bonfire_basic_soc
   generic map (
     ENABLE_EXT_RAM  => ExtRAM,
     ENABLE_UART1    => true,
     ENABLE_SPI      => true,
     ENABLE_DCACHE   => EnableDCache,
     BurstSize       => BurstSize,
     CacheSizeWords  => CacheSizeWords,
     DCacheSizeWords => DCacheSizeWords,
     MUL_ARCH        => MUL_ARCH,
     REG_RAM_STYLE   => REG_RAM_STYLE,
     NUM_GPIO        => NUM_GPIO,
     DEVICE_FAMILY   => DEVICE_FAMILY
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
     uart1_txd      => uart1_txd,
     uart1_rxd      => uart1_rxd,
     flash_spi_cs   => flash_spi_cs,
     flash_spi_clk  => flash_spi_clk,
     flash_spi_mosi => flash_spi_mosi,
     flash_spi_miso => flash_spi_miso,
     gpio_o         => gpio_o,
     gpio_i         => gpio_i,
     gpio_t         => gpio_t
   );


  ram_nl:  if not LANED_RAM generate
     ram: entity work.MainMemory
          generic map (
             ADDR_WIDTH =>ram_adr_width,
             RamFileName => RamFileName,
             mode => mode,
             Swapbytes => Swapbytes,
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
  end generate;

  ram_l:  if LANED_RAM generate
     ram: entity work.main_memory_laned
          generic map (
             ADDR_WIDTH =>ram_adr_width,
             RamFileName => RamFileName,
             mode => mode,
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
  end generate;



-- Clock

g_clkgen: if not BYPASS_CLKGEN generate
  clkgen_inst: clkgen_arty
    port map (
    -- Clock out ports
    clkout => clk,
    -- Status and control signals
    reset => res2,
    locked => clk_locked,
    -- Clock in ports
    sysclk => sysclk
  );
end generate;

g_bypass: if BYPASS_CLKGEN generate

   --report "Clock generator bypassed" severity info;

   clk <= sysclk;
   clk_locked <= '1';

end generate;


    process(sysclk) begin
      if rising_edge(sysclk) then
         res1<= I_RESET;
         res2 <= res1;
      end if;

    end process;

    reset <= res2 or not clk_locked;

end Behavioral;
