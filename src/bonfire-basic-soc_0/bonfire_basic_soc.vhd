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

entity bonfire_basic_soc is
generic (

     ENABLE_EXT_RAM : boolean := false;
     ENABLE_UART1 : boolean := false;
     ENABLE_SPI : boolean := false;
     ENABLE_DCACHE : boolean := false;
     ENABLE_GPIO : boolean := true; 
     UART_FIFO_DEPTH : natural := 6;

     BRAM_ADR_WIDTH : natural := 13;

     BurstSize : natural := 8;
     CacheSizeWords : natural := 512; -- 2KB Instruction Cache
     DCacheSizeWords : natural := 512;
     BRAM_ADR_BASE : std_logic_vector(7 downto 0) := X"0C";
     MUL_ARCH: string := "spartandsp";
     REG_RAM_STYLE : string := "block";
     NUM_GPIO   : natural := 8;
     DEVICE_FAMILY : string :=  ""
   );
   port(
        clk_i  : in  std_logic;
        reset_i  : in  std_logic;

        -- Block RAM Port
        -- Interface to  dual port Block RAM
        -- Port A R/W, Byte Level Access, for Data

        bram_dba_i : in std_logic_vector(31 downto 0);
        bram_dba_o : out std_logic_vector(31 downto 0);
        bram_adra_o : out std_logic_vector(BRAM_ADR_WIDTH-1 downto 0);
        bram_ena_o :  out  STD_LOGIC;
        bram_wrena_o :out  STD_LOGIC_VECTOR (3 downto 0);

         -- Port B Read Only, Word level access, for Code
        bram_dbb_i : in std_logic_vector(31 downto 0);
        bram_adrb_o : out std_logic_vector(BRAM_ADR_WIDTH-1 downto 0);
        bram_enb_o :  out  STD_LOGIC;

        -- External Memory interface
        wbm_cyc_o: out std_logic;
        wbm_stb_o: out std_logic;
        wbm_we_o : out std_logic;
        wbm_cti_o: out std_logic_vector(2 downto 0);
        wbm_bte_o: out std_logic_vector(1 downto 0);
        wbm_sel_o : out std_logic_vector(3 downto 0);
        wbm_ack_i: in std_logic;
        wbm_adr_o: out std_logic_vector(25 downto 2);
        wbm_dat_i: in std_logic_vector(31 downto 0);
        wbm_dat_o: out std_logic_vector(31 downto 0);

        -- UART0 signals:
        uart0_txd : out std_logic;
        uart0_rxd : in  std_logic;

        -- UART1 signals:
        uart1_txd : out std_logic;
        uart1_rxd : in  std_logic;

        -- SPI flash chip
        flash_spi_cs        : out   std_logic;
        flash_spi_clk       : out   std_logic;
        flash_spi_mosi      : out   std_logic;
        flash_spi_miso      : in    std_logic;

       -- GPIO
        gpio_o : out std_logic_vector(NUM_GPIO-1 downto 0);
        gpio_i : in  std_logic_vector(NUM_GPIO-1 downto 0);
        gpio_t : out std_logic_vector(NUM_GPIO-1 downto 0)

    );
end bonfire_basic_soc;

architecture Behavioral of bonfire_basic_soc is



 constant ram_adr_width : natural := BRAM_ADR_WIDTH;
 constant ram_size : natural := 2** ram_adr_width;


 constant reset_adr : std_logic_vector(31 downto 0) := BRAM_ADR_BASE & X"000000";

 constant generate_dcache : boolean := ENABLE_EXT_RAM and ENABLE_DCACHE;


signal clk : std_logic;        -- logical CPU clock
signal reset : std_logic;


-- Instruction Bus Master from CPU
signal ibus_cyc_o:  std_logic;
signal ibus_stb_o:  std_logic;
signal ibus_cti_o:  std_logic_vector(2 downto 0);
signal ibus_bte_o:  std_logic_vector(1 downto 0);
signal ibus_ack_i:  std_logic;
signal ibus_adr_o:  std_logic_vector(29 downto 0);
signal ibus_dat_i:  std_logic_vector(31 downto 0);

-- Data Bus Master from CPU
signal  dbus_cyc_o :  std_logic;
signal  dbus_stb_o :  std_logic;
signal  dbus_we_o :  std_logic;
signal  dbus_sel_o :  std_logic_vector(3 downto 0);
signal  dbus_adr_o :  std_logic_vector(31 downto 2);
signal  dbus_dat_o :  std_logic_vector(31 downto 0);
signal  dbus_ack_i :  std_logic;
signal  dbus_dat_i :  std_logic_vector(31 downto 0);
--signal  dbus_cti_o:  std_logic_vector(2 downto 0);
--signal  dbus_bte_o:  std_logic_vector(1 downto 0);

-- Slaves
constant slave_adr_high : natural := 25;


-- Common bus to external RAM
signal mem_cyc,mem_stb,mem_we,mem_ack : std_logic;
signal mem_sel :  std_logic_vector(3 downto 0);
signal mem_dat_rd,mem_dat_wr : std_logic_vector(31 downto 0);
signal mem_adr : std_logic_vector(slave_adr_high downto 2);
signal mem_cti : std_logic_vector(2 downto 0);


-- Data bus to external RAM
signal dbmem_cyc,dbmem_stb,dbmem_we,dbmem_ack : std_logic;
signal dbmem_sel :  std_logic_vector(3 downto 0);
signal dbmem_dat_rd,dbmem_dat_wr : std_logic_vector(31 downto 0);
signal dbmem_adr : std_logic_vector(slave_adr_high downto 2);
signal dbmem_cti : std_logic_vector(2 downto 0);


-- "CPU" Side of Data Cache
signal dcm_cyc,dcm_stb,dcm_we,dcm_ack : std_logic;
signal dcm_sel :  std_logic_vector(3 downto 0);
signal dcm_dat_rd,dcm_dat_wr : std_logic_vector(31 downto 0);
signal dcm_adr : std_logic_vector(slave_adr_high downto 2);
signal dcm_cti : std_logic_vector(2 downto 0);
signal dcm_bte : std_logic_vector(1 downto 0);

--I/O Bus
signal io_cyc,io_stb,io_we,io_ack : std_logic;
signal io_sel :  std_logic_vector(3 downto 0);
signal io_dat_rd,io_dat_wr : std_logic_vector(31 downto 0);
signal io_adr : std_logic_vector(slave_adr_high downto 2);


-- gpio ports


constant TOTAL_GPIO : natural := NUM_GPIO;




signal irq_i : std_logic_vector(7 downto 0);


function d_cache_size return natural is
begin

  if DEVICE_FAMILY="SPARTAN6" then
    assert DCacheSizeWords=2048
    report "Due to XST synthesis bugs DCache Size will be hard coded to 2048*32Bit (8KByte)"
      severity warning;
      return  2048;
  else
    return DCacheSizeWords;
  end if;

end function;


begin
   assert TOTAL_GPIO <= 32
     report "Total number of gpio ports cannot exceed 32"
     severity failure;

   -- Assignment of IOBs for GPIO


    cpu_top: entity work.bonfire_cpu_top
     generic map (
       MUL_ARCH => MUL_ARCH,
       REG_RAM_STYLE => REG_RAM_STYLE,
       START_ADDR => reset_adr(31 downto 2),
       CACHE_LINE_SIZE_WORDS =>BurstSize,
       CACHE_SIZE_WORDS=>CacheSizeWords,
       BRAM_PORT_ADR_SIZE=>ram_adr_width,
       ENABLE_TIMER=>true,
       BRANCH_PREDICTOR=>true
     )

     PORT MAP(
        clk_i => clk,
        rst_i => reset_i,

        bram_dba_i => bram_dba_i,
        bram_dba_o => bram_dba_o,
        bram_adra_o => bram_adra_o,
        bram_ena_o =>  bram_ena_o,
        bram_wrena_o => bram_wrena_o,
        bram_dbb_i =>  bram_dbb_i,
        bram_adrb_o => bram_adrb_o,
        bram_enb_o =>  bram_enb_o,

        wb_ibus_cyc_o => ibus_cyc_o ,
        wb_ibus_stb_o => ibus_stb_o,
        wb_ibus_cti_o => ibus_cti_o,
        wb_ibus_bte_o => ibus_bte_o,
        wb_ibus_ack_i => ibus_ack_i,
        wb_ibus_adr_o => ibus_adr_o,
        wb_ibus_dat_i => ibus_dat_i,

        wb_dbus_cyc_o => dbus_cyc_o,
        wb_dbus_stb_o => dbus_stb_o,
        wb_dbus_we_o =>  dbus_we_o,
        wb_dbus_sel_o => dbus_sel_o,
        wb_dbus_ack_i => dbus_ack_i,
        wb_dbus_adr_o => dbus_adr_o,
        wb_dbus_dat_o => dbus_dat_o,
        wb_dbus_dat_i => dbus_dat_i,

        irq_i => irq_i
    );


   inst_busconnect:   entity  work.cpu_dbus_connect PORT MAP(
        clk_i => clk,
        rst_i => reset,

        -- Data bus
        s0_cyc_i => dbus_cyc_o,
        s0_stb_i => dbus_stb_o,
        s0_we_i =>  dbus_we_o,
        s0_sel_i => dbus_sel_o,
        s0_ack_o => dbus_ack_i,
        s0_adr_i => dbus_adr_o,
        s0_dat_i => dbus_dat_o,
        s0_dat_o => dbus_dat_i,


          -- External RAM at address   0x00000000-0x03FFFFFF
        m0_cyc_o =>  dbmem_cyc,
        m0_stb_o =>  dbmem_stb,
        m0_we_o =>   dbmem_we,
        m0_sel_o =>  dbmem_sel,
        m0_ack_i =>  dbmem_ack,
        m0_adr_o =>  dbmem_adr,
        m0_dat_o =>  dbmem_dat_wr,
        m0_dat_i =>  dbmem_dat_rd,

        --IO Space : 0x04000000-0x07FFFFF (Decode 0000 01)
        m1_cyc_o =>  io_cyc,
        m1_stb_o =>  io_stb,
        m1_we_o =>   io_we,
        m1_sel_o =>  io_sel,
        m1_ack_i =>  io_ack,
        m1_adr_o =>  io_adr,
        m1_dat_o =>  io_dat_wr,
        m1_dat_i =>  io_dat_rd
    );



no_dcache: if not generate_dcache  generate
      dcm_cyc <=   dbmem_cyc;
      dcm_stb <= dbmem_stb;
      dcm_adr <= dbmem_adr;
      dcm_we <= dbmem_we;
      dcm_sel <= dbmem_sel;
      dcm_cti <= "000";
      dcm_bte <= "00";
      dcm_adr <= dbmem_adr;
      dcm_dat_wr <= dbmem_dat_wr;

      dbmem_dat_rd <= dcm_dat_rd;
      dbmem_ack <=dcm_ack;

   end generate;

dache: if generate_dcache generate


   Inst_bonfire_dcache: entity work.bonfire_dcache
   GENERIC MAP (
     MASTER_DATA_WIDTH => 32,
     LINE_SIZE => BurstSize,
     CACHE_SIZE => d_cache_size,
     ADDRESS_BITS => dcm_adr'length,
     DEVICE_FAMILY => DEVICE_FAMILY-- hard coded work around...
   )

   PORT MAP(
        clk_i => clk,
        rst_i => reset,
        wbs_cyc_i => dbmem_cyc,
        wbs_stb_i => dbmem_stb,
        wbs_we_i =>  dbmem_we,
        wbs_sel_i => dbmem_sel,
        wbs_ack_o => dbmem_ack,
        wbs_adr_i => dbmem_adr,
        wbs_dat_o => dbmem_dat_rd,
        wbs_dat_i => dbmem_dat_wr,

        wbm_cyc_o => dcm_cyc,
        wbm_stb_o => dcm_stb,
        wbm_we_o =>  dcm_we,
        wbm_cti_o => dcm_cti,
        wbm_bte_o => dcm_bte,
        wbm_sel_o => dcm_sel,
        wbm_ack_i => dcm_ack,
        wbm_adr_o => dcm_adr,
        wbm_dat_i => dcm_dat_rd,
        wbm_dat_o => dcm_dat_wr
    );


   end generate;


-- Combine Dbus and ibus mem masters to one for interface to external RAM

extram:  if ENABLE_EXT_RAM generate
  extram_arbiter:  entity work.dram_arbiter PORT MAP(
          clk_i => clk,
          rst_i => reset,
          -- DBUS has higher prio

          s0_cyc_i => dcm_cyc,
          s0_stb_i => dcm_stb,
          s0_we_i =>  dcm_we,
          s0_sel_i => dcm_sel,
          s0_cti_i => dcm_cti,
          s0_bte_i => dcm_bte,
          s0_ack_o => dcm_ack,
          s0_adr_i => dcm_adr,
          s0_dat_i => dcm_dat_wr,
          s0_dat_o => dcm_dat_rd,

          -- IBUS
          s1_cyc_i => ibus_cyc_o ,
          s1_stb_i => ibus_stb_o,
          s1_we_i =>  '0',
          s1_sel_i => "1111",
          s1_cti_i => ibus_cti_o,
          s1_bte_i => ibus_bte_o,
          s1_ack_o => ibus_ack_i,
          s1_adr_i => ibus_adr_o(ibus_adr_o'low+23 downto ibus_adr_o'low),
          s1_dat_i => (others=>'0'),
          s1_dat_o => ibus_dat_i,
          -- Interace to memory controller
          m0_cyc_o => wbm_cyc_o,
          m0_stb_o => wbm_stb_o,
          m0_we_o =>  wbm_we_o,
          m0_sel_o => wbm_sel_o,
          m0_cti_o => wbm_cti_o,
          m0_bte_o => wbm_bte_o,
          m0_ack_i => wbm_ack_i,
          m0_adr_o => wbm_adr_o,
          m0_dat_o => wbm_dat_o,
          m0_dat_i => wbm_dat_i
      );
end generate;




Inst_bonfire_soc_io: entity  work.bonfire_soc_io
GENERIC MAP (
  NUM_GPIO_BITS => gpio_o'length,
  ADR_HIGH => io_adr'high,
  UART_FIFO_DEPTH => UART_FIFO_DEPTH,
  ENABLE_UART0 => true,
  ENABLE_UART1 => ENABLE_UART1,
  ENABLE_SPI => ENABLE_SPI,
  ENABLE_GPIO => ENABLE_GPIO
)
PORT MAP(
        uart0_txd => uart0_txd,
        uart0_rxd => uart0_rxd,
        uart1_txd => uart1_txd,
        uart1_rxd => uart1_rxd,
        gpio_o => gpio_o ,
        gpio_i => gpio_i,
        gpio_t =>  gpio_t,
        flash_spi_cs => flash_spi_cs,
        flash_spi_clk => flash_spi_clk,
        flash_spi_mosi => flash_spi_mosi,
        flash_spi_miso => flash_spi_miso,
        irq_o => irq_i,
        clk_i => clk,
        rst_i => reset,
        wb_cyc_i => io_cyc,
        wb_stb_i => io_stb,
        wb_we_i =>  io_we,
        wb_sel_i => io_sel,
        wb_ack_o => io_ack,
        wb_adr_i => io_adr,
        wb_dat_i => io_dat_wr,
        wb_dat_o => io_dat_rd
    );



 clk <= clk_i;
 reset <= reset_i;

end Behavioral;
