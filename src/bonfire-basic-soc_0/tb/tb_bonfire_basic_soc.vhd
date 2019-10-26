-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 30.4.2018 16:33:09 GMT

library ieee;
use ieee.std_logic_1164.all;

LIBRARY std;
USE std.textio.all;

use work.txt_util.all;


entity tb_bonfire_basic_soc is
generic(
         RamFileName : string :="../src/bonfire-basic-soc_0/compiled_code/sim_hello.hex";
         mode : string := "H";       -- only used when UseBRAMPrimitives is false
         Swapbytes : boolean := false; -- SWAP Bytes in RAM word in low byte first order to use data2mem
         ExtRAM : boolean := false; -- "Simulate" External RAM as Bock RAM
         BurstSize : natural := 8;
         CacheSizeWords : natural := 512; -- 2KB Instruction Cache
         EnableDCache : boolean := false;
         DCacheSizeWords : natural := 512;
         MUL_ARCH: string := "spartandsp";
         REG_RAM_STYLE : string := "block";
         NUM_GPIO   : natural := 8;
         DEVICE_FAMILY : string :=  "";
         UART_BAUDRATE : real := 115200.0

       );


end tb_bonfire_basic_soc;

architecture tb of tb_bonfire_basic_soc is

    component bonfire_basic_soc
    generic (
         RamFileName : string:="";    -- :="compiled_code/monitor.hex";
         mode : string := "H";       -- only used when UseBRAMPrimitives is false
         Swapbytes : boolean := true; -- SWAP Bytes in RAM word in low byte first order to use data2mem
         ExtRAM : boolean := false; -- "Simulate" External RAM as Bock RAM
         BurstSize : natural := 8;
         CacheSizeWords : natural := 512; -- 2KB Instruction Cache
         EnableDCache : boolean := false;
         DCacheSizeWords : natural := 512;
         MUL_ARCH: string := "spartandsp";
         REG_RAM_STYLE : string := "block";
         NUM_GPIO   : natural := 8;
         DEVICE_FAMILY : string :=  ""

       );

        port (sysclk         : in std_logic;
              I_RESET        : in std_logic;
              uart0_txd      : out std_logic;
              uart0_rxd      : in std_logic;
              uart1_txd      : out std_logic;
              uart1_rxd      : in std_logic;
              flash_spi_cs   : out std_logic;
              flash_spi_clk  : out std_logic;
              flash_spi_mosi : out std_logic;
              flash_spi_miso : in std_logic;
              GPIO           : inout std_logic_vector (NUM_GPIO-1 downto 0));
    end component;

    signal sysclk         : std_logic;
    signal I_RESET        : std_logic :='0';
    signal uart0_txd      : std_logic;
    signal uart0_rxd      : std_logic :='1';
    signal uart1_txd      : std_logic;
    signal uart1_rxd      : std_logic := '1';
    signal flash_spi_cs   : std_logic;
    signal flash_spi_clk  : std_logic;
    signal flash_spi_mosi : std_logic;
    signal flash_spi_miso : std_logic;
    signal GPIO           : std_logic_vector (num_gpio-1 downto 0);

    constant TbPeriod : time := 10 ns;  -- 100 Mhz (for ARTY)
    signal TbClock : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

-- UART Capture Module
    constant bit_time : time := ( 1_000_000.0 / UART_BAUDRATE ) * 1 us;
    subtype t_uartnum is natural range 0 to 1;
    type t_uart_kpi is array (t_uartnum) of natural;

    signal total_count : t_uart_kpi;
    signal framing_errors : t_uart_kpi;
    signal uart0_stop : boolean;

    COMPONENT tb_uart_capture_tx
    GENERIC (
      baudrate : natural;
      bit_time : time;
      SEND_LOG_NAME : string ;
      stop_mark : std_logic_vector(7 downto 0) -- Stop marker byte
     );
    PORT(
        txd : IN std_logic;
        stop : OUT boolean;
        framing_errors : OUT natural;
        total_count : OUT natural
        );
    END COMPONENT;



begin

    dut : bonfire_basic_soc
    generic map (
      RamFileName => RamFileName,
         mode => mode,
         Swapbytes => SwapBytes,
         ExtRAM => ExtRAM,
         BurstSize => BurstSize,
         CacheSizeWords => CacheSizeWords,
         EnableDCache => EnableDCache,
         DCacheSizeWords => DCacheSizeWords,
         MUL_ARCH => MUL_ARCH,
         REG_RAM_STYLE => REG_RAM_STYLE,
         NUM_GPIO  => NUM_GPIO,
         DEVICE_FAMILY => DEVICE_FAMILY

    )
    port map (sysclk         => sysclk,
              I_RESET        => I_RESET,
              uart0_txd      => uart0_txd,
              uart0_rxd      => uart0_rxd,
              uart1_txd      => uart1_txd,
              uart1_rxd      => uart1_rxd,
              flash_spi_cs   => flash_spi_cs,
              flash_spi_clk  => flash_spi_clk,
              flash_spi_mosi => flash_spi_mosi,
              flash_spi_miso => flash_spi_miso,
              GPIO           => GPIO);


   capture_tx_0 :  tb_uart_capture_tx
   GENERIC MAP (
       baudrate => natural(UART_BAUDRATE),
       bit_time => bit_time,
       SEND_LOG_NAME => "send0.log",
       stop_mark => X"1A"
   )
   PORT MAP(
        txd => uart0_txd,
        stop => uart0_stop ,
        framing_errors => framing_errors(0),
        total_count =>total_count(0)
    );

    process(total_count)
    begin
      report "Byte received over UART" severity note;

    end process;



    -- Clock generation
    TbClock <= not TbClock after TbPeriod/2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that sysclk is really your main clock signal
    sysclk <= TbClock;

    -- SPI Loopback

    flash_spi_miso <= flash_spi_mosi;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed



        -- Reset generation
        -- EDIT: Check that I_RESET is really your reset signal
        wait for 50 ns;
        I_RESET <= '1';
        wait for 50 ns;
        I_RESET <= '0';
      
        -- EDIT Add stimuli here


        -- Stop the clock and hence terminate the simulation
        --TbSimEnded <= '1';
        wait until uart0_stop;
        print(OUTPUT,"UART0 Test captured bytes: " & str(total_count(0)) & " framing errors: " & str(framing_errors(0)));
      
        TbSimEnded <= '1';
        wait;
    end process;

end tb;


