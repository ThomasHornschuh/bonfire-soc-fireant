--
--  UART for ZPUINO
--
--  Copyright 2010 Alvaro Lopes <alvieboy@alvie.com>
--
--  Version: 1.0
--
--  The FreeBSD license
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  1. Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above
--     copyright notice, this list of conditions and the following
--     disclaimer in the documentation and/or other materials
--     provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY
--  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
--  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
--  ZPU PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
--  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
--  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
--  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
--  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--UART Registers Decsription


--//    |--------------------|--------------------------------------------|
--//--! | Address(binary)    | Description                                |
--//--! |--------------------|--------------------------------------------|
--//--! | 00                 | Transmit register(W)/  Receive register(R) |
--//--! | 01                 | Status(R)) and control(W) register         |
--//--! |--------------------|--------------------------------------------|
--
--  The Status/Control register is used as Status Register when read,
--  and as Control register when written
--//--! The status register contains the following bits:
--//--! - Bit 0: UART RX Ready bit. Reads as 1 when there's received data in FIFO, 0 otherwise.
--//--! - Bit 1: UART TX Ready bit. Reads as 1 when there's space in TX FIFO for transmission, 0 otherwise.
--//--! - Bit 2: TX in progess
--
--// Control register:
--// Bit [15:0] - UARTPRES UART prescaler (16 bits)   (f_clk / (baudrate * 16)) - 1
--// Bit 16 - UARTEN UARTEN bit controls whether UART is enabled or not

-- Addtional register when running in extended Mode the UART contains the following  registers/bits

--//    |--------------------|--------------------------------------------|
--//--! | Address            | Description                                |
--//--! |--------------------|--------------------------------------------|
--//--! | 00                 | Transmit register(W)/  Receive register(R) |
--//--! | 01                 | Status(R)) and control(W) register         |
--//--! | 10                 | Extended Control Register  (RW)            |
--//--! | 11                 | Interrupt Register  (RW)                   |
--//--! |--------------------|--------------------------------------------|

--// Transmit Register:
--// Bit[7:0] : Byte to be transmitted

--// Receive Register:
--// Bit[7:0] : Received Byte
--// Bit[30:8]: Currently all 0, but reserved for future use in extended mode
--// Bit[31]  : When in extented mode, this bit is set when a framing error was encountered upon receving the byte
--//            in non-extended mode this bit is 0


--// Extended Control register:
--// Bit [15:0] - UARTPRES UART prescaler (16 bits)   (f_clk /baudrate) - 1
--//              Requires Prescaler value >= 16 because of times 16 oversampling in receiver

--// Bit 16 - UARTEN UARTEN bit controls whether UART is enabled or not
--// Bit 17 - EXT_EN: Enable extended mode - when set one the extended mode is activated
--// Bit [31:18] - FIFO "Nearly Full" Threshold. The number of bits actual used depends on the confirued FIFO size

--// Status(R)/ Control(W) Register:
--// When written, it works as control register, but limited to bit 16..0, so it is compatible with the non-extended mode
--// When read it works as status register:

--//--! The status register contains the following bits:
--//--! - Bit 0: UART RX Ready bit. Reads as 1 when there's received data in FIFO, 0 otherwise.
--//--! - Bit 1: UART TX Ready bit. Reads as 1 when there's space in TX FIFO for transmission, 0 otherwise.
--//--! - Bit 2: TX in progess
--//--! - Bit 3: UART RX FIFO occupation > threshold. This bit is only set when the extended mode is activated
--//--! - Bit[19:16]: log(2) FIFO Length (Number of bits of FIFO address)


--//--! Interrupt Register (R/W) (only visible in extended mode)
--//--! Bit 0 : Enable RX Interrupt
--//--! Bit 1 : Enable TX Interrupt
--//--! Bit 3 : Enable FIFO Nearly Full Interrupt
--//--! Bit 16 : R: RX Interrupt Pending W: Writing 1 will clear this pending bit
--//--! Bit 17 : R: TX Interrupt Pending W: Writing 1 will clear this pending bit
--//--! Bit 19 : R: RX FIFO Nearly Full Interrupt Pending W: Writing 1 will clear this pending bit

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;


entity zpuino_uart is
  generic (
    bits: integer := 11; -- log2 of FIFO depth
    wordSize : natural := 32; -- Bus width
    maxIObit : natural := 3;  -- Adr bus  high
    minIObit : natural := 2;   -- Adr bus  low
    extended : boolean := false -- TH: TRUE: Activate extended version
  );
  port (
    wb_clk_i: in std_logic;
    wb_rst_i: in std_logic;
    wb_dat_o: out std_logic_vector(wordSize-1 downto 0);
    wb_dat_i: in std_logic_vector(wordSize-1 downto 0);
    wb_adr_i: in std_logic_vector(maxIObit downto minIObit);
    wb_we_i:  in std_logic;
    wb_cyc_i: in std_logic;
    wb_stb_i: in std_logic;
    wb_ack_o: out std_logic;
    wb_inta_o:out std_logic;
    id:       out std_logic_vector(15 downto 0);
    enabled:  out std_logic;
    tx:       out std_logic;
    rx:       in std_logic
  );
end entity zpuino_uart;



architecture behave of zpuino_uart is

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

ATTRIBUTE X_INTERFACE_INFO OF tx: SIGNAL IS "xilinx.com:interface:uart:1.0 UART TxD";
ATTRIBUTE X_INTERFACE_INFO OF rx: SIGNAL IS "xilinx.com:interface:uart:1.0 UART RxD";


  component uart_rx is
  port (
    clk:      in std_logic;
    rst:      in std_logic;
    rx:       in std_logic;
    rxclk:    in std_logic;
    read:     in std_logic;
    data:     out std_logic_vector(7 downto 0);
    data_av:  out std_logic;
    framing_error : out std_logic
  );
  end component uart_rx;


  component TxUnit is
  port (
     clk_i    : in  std_logic;  -- Clock signal
     reset_i  : in  std_logic;  -- Reset input
     enable_i : in  std_logic;  -- Enable input
     load_i   : in  std_logic;  -- Load input
     txd_o    : out std_logic;  -- RS-232 data output
     busy_o   : out std_logic;  -- Tx Busy
     intx_o   : out std_logic;  -- Tx in progress
     datai_i  : in  std_logic_vector(7 downto 0)); -- Byte to transmit
  end component TxUnit;

  component uart_brgen is
  port (
     clk:     in std_logic;
     rst:     in std_logic;
     en:      in std_logic;
     count:   in std_logic_vector(15 downto 0);
     clkout:  out std_logic
     );
  end component uart_brgen;

  component zpuino_uart_fifo is
  generic (
    bits: integer := 11;
    datawidth: integer := 8
  );
  port (
    clk:      in std_logic;
    rst:      in std_logic;
    wr:       in std_logic;
    rd:       in std_logic;
    write:    in std_logic_vector(datawidth-1  downto 0);
    read :    out std_logic_vector(datawidth-1 downto 0);
    full:     out std_logic;
    empty:    out std_logic;
    used_o:   out unsigned(bits-1 downto 0)
  );
  end component zpuino_uart_fifo;


  signal uart_read: std_logic;
  signal uart_write: std_logic;
  signal divider_tx: std_logic_vector(15 downto 0) := (others=>'1');

  signal divider_rx_q: std_logic_vector(15 downto 0) :=(others=>'1');

  signal divider_reset : std_logic :='0';

  signal data_ready: std_logic;
  signal received_data: std_logic_vector(7 downto 0);
  signal fifo_received_data : std_logic_vector(8 downto 0);
  signal fifo_data: std_logic_vector(8 downto 0);
  signal uart_busy: std_logic;
  signal uart_intx: std_logic;
  signal fifo_empty: std_logic;
  signal rx_br: std_logic;
  signal tx_br: std_logic;
  signal rx_en: std_logic;

  signal dready_q: std_logic;
  signal data_ready_dly_q: std_logic;
  signal fifo_rd: std_logic;
  signal enabled_q: std_logic;

  signal rx_fifo_used : unsigned(bits-1 downto 0);

  signal Adr0Selected: std_logic;

  signal framing_error : std_logic;



-- Extended Mode register bits

  signal ext_mode_en : std_logic := '0';
  signal fifo_threshold : std_logic_vector(bits-1 downto 0) := (others=>'0');

  signal rx_int_en : std_logic := '0';
  signal tx_int_en : std_logic := '0';
  signal fifo_int_en : std_logic := '0';

  signal rx_int_pending : std_logic := '0';
  signal tx_int_pending : std_logic := '0';
  signal fifo_int_pending : std_logic := '0';


  signal rx_rdy, tx_rdy,fifo_nf : std_logic; -- Status Bits
  signal rx_rdy0, tx_rdy0, fifo_nf0 : std_logic; -- old value for edge detection

--  attribute mark_debug : string;
--  attribute mark_debug of rx_rdy : signal is "true";
--  attribute mark_debug of tx_rdy : signal is "true";
--  attribute mark_debug of data_ready : signal is "true";
--  attribute mark_debug of received_data : signal is "true";
--
--  attribute mark_debug of wb_cyc_i : signal is "true";
--  attribute mark_debug of wb_adr_i : signal is "true";
--  attribute mark_debug of wb_dat_o : signal is "true";

begin


  -- Adress decoder for RX/TX Register
  process(wb_adr_i) begin

    if extended and wb_adr_i(minIObit+1 downto minIObit)="00" then
       Adr0Selected <='1';
    elsif not extended and wb_adr_i(minIObit)='0' then
       Adr0Selected <='1';
    else
       Adr0Selected <='0';
    end if;

  end process;


  enabled <= enabled_q;
  wb_ack_o <= wb_cyc_i and wb_stb_i;
  id <= x"08" & x"11"; -- Vendor: ZPUino  Device: UART

  wb_inta_o <= rx_int_pending or tx_int_pending or fifo_int_pending;

  rx_inst: uart_rx
    port map(
      clk     => wb_clk_i,
      rst     => wb_rst_i,
      rxclk   => rx_br,
      read    => uart_read,
      rx      => rx,
      data_av => data_ready,
      data    => received_data,
      framing_error => framing_error
   );

   fifo_received_data <= framing_error & received_data;

  uart_read <= dready_q;

  tx_core: TxUnit
    port map(
      clk_i     => wb_clk_i,
      reset_i   => wb_rst_i,
      enable_i  => tx_br,
      load_i    => uart_write,
      txd_o     => tx,
      busy_o    => uart_busy,
      intx_o    => uart_intx,
      datai_i   => wb_dat_i(7 downto 0)
    );

  -- TODO: check multiple writes
  uart_write <= '1' when (wb_cyc_i='1' and wb_stb_i='1' and wb_we_i='1') and Adr0Selected='1' else '0';

   -- Rx timing
  rx_timer: uart_brgen
    port map(
      clk => wb_clk_i,
      rst => divider_reset,
      en => '1',
      clkout => rx_br,
      count => divider_rx_q
    );

   -- Tx timing
  tx_timer: uart_brgen
    port map(
      clk => wb_clk_i,
      rst => divider_reset,
      en => '1',
      clkout => tx_br,
      count => divider_tx
    );

  process(wb_clk_i)
  begin
    if rising_edge(wb_clk_i) then
      if wb_rst_i='1' then
        dready_q<='0';
        data_ready_dly_q<='0';
      else

        data_ready_dly_q<=data_ready;

        if data_ready='1' and data_ready_dly_q='0' then
          dready_q<='1';
        else
          dready_q<='0';
        end if;

      end if;
    end if;
  end process;

  fifo_instance: zpuino_uart_fifo
    generic map (
      bits => bits,
      datawidth => 9
    )
    port map (
      clk   => wb_clk_i,
      rst   => wb_rst_i,
      wr    => dready_q,
      rd    => fifo_rd,
      write => fifo_received_data,
      read  => fifo_data,
      full  => open,
      empty => fifo_empty,
      used_o => rx_fifo_used
    );


  fifo_rd<='1' when Adr0Selected='1' and (wb_cyc_i='1' and wb_stb_i='1' and wb_we_i='0') else '0';


-- Status Bits

   fifo_nf <= '1' when rx_fifo_used > unsigned(fifo_threshold) else '0';
   rx_rdy  <= not fifo_empty;
   tx_rdy  <= not uart_busy;

-- For change detection
   process(wb_clk_i) begin
     if rising_edge(wb_clk_i) then
       fifo_nf0 <= fifo_nf;
       rx_rdy0 <= rx_rdy;
       tx_rdy0 <= tx_rdy;
     end if;
   end process;



  process(wb_adr_i, received_data,  fifo_data,uart_intx,ext_mode_en,divider_rx_q,
          fifo_nf,tx_rdy,rx_rdy,enabled_q,fifo_threshold,rx_int_en,tx_int_en,fifo_int_en,
          rx_int_pending,tx_int_pending,fifo_int_pending,divider_tx)
  variable adr : std_logic_vector(1 downto 0);
  begin
    if extended then
       adr:= wb_adr_i(minIObit+1 downto minIObit);
    else
      adr:= '0' & wb_adr_i(minIObit);
    end if;

    wb_dat_o <= (others => '0');
    case adr is
      when "00" => -- Read RX Register
        wb_dat_o <= (others => '0');
        wb_dat_o(7 downto 0) <= fifo_data(7 downto 0);
        if ext_mode_en='1' then -- Read Framing error bit in extented mode
          wb_dat_o(wb_dat_o'high)<=fifo_data(8);
        end if;

      when "01" => -- Read Status Register
        wb_dat_o(0) <= rx_rdy;
        wb_dat_o(1) <= tx_rdy;
        wb_dat_o(2) <= uart_intx;
        if ext_mode_en='1' then
          wb_dat_o (3) <= fifo_nf;
          wb_dat_o(19 downto 16) <=std_logic_vector(to_unsigned(bits,4));
        end if;

      when "10" =>  -- Read Extended Control Register
        wb_dat_o(15 downto 0) <= divider_tx;
        wb_dat_o(16) <= enabled_q;
        wb_dat_o(17) <= ext_mode_en;
        wb_dat_o(18+fifo_threshold'high downto 18) <= fifo_threshold;

      when "11" => -- Read Interrupt Register
        if ext_mode_en='1' then
          wb_dat_o(0) <=rx_int_en;
          wb_dat_o(1) <=tx_int_en;
          wb_dat_o(3) <=fifo_int_en;
          wb_dat_o(16) <=rx_int_pending;
          wb_dat_o(17) <=tx_int_pending;
          wb_dat_o(19) <=fifo_int_pending;
        end if;


      when others =>
        wb_dat_o <= (others => 'X');
    end case;

  end process;

  process(wb_clk_i)
  variable adr : std_logic_vector(1 downto 0);
  variable tx_div : unsigned(divider_tx'range);
  variable div16 : unsigned(11 downto 0);
  begin

    if rising_edge(wb_clk_i) then
      if wb_rst_i='1' then
        enabled_q<='0';
        divider_reset<='1';
      else

        divider_reset <= '0';

        -- Interrupt detection
        if rx_int_en='1' and rx_rdy='1' and rx_rdy0='0' then
          rx_int_pending <= '1';
        end if;
        if tx_int_en='1' and tx_rdy='1' and tx_rdy0='0' then
          tx_int_pending <= '1';
        end if;
        if fifo_int_en='1' and fifo_nf='1' and fifo_nf0='0' then
          fifo_int_pending <= '1';
        end if;

        -- Register Write
        if wb_cyc_i='1' and wb_stb_i='1' and wb_we_i='1' then
            if extended then
              adr:= wb_adr_i(minIObit+1 downto minIObit);
            else
              adr:= '0' & wb_adr_i(minIObit);
            end if;

            case  adr is
              when "01" =>
                -- Backwards compatible mode
                -- Bit [15:0] - UARTPRES UART prescaler (16 bits)   (f_clk / (baudrate * 16)) - 1
                -- Attention: Did not work when Bits[15:13] != 0 because it will cause an overflow
                -- of the TX-Div
                divider_rx_q <= wb_dat_i(15 downto 0);
                tx_div:=resize((unsigned(wb_dat_i(11 downto 0))+1) & "0000" , tx_div'length);
                divider_tx <= std_logic_vector(tx_div);
                enabled_q  <= wb_dat_i(16);
                divider_reset<='1';

              when "10" =>
                ext_mode_en <= wb_dat_i(17);
                enabled_q  <= wb_dat_i(16);
                fifo_threshold <= wb_dat_i(18+fifo_threshold'high downto 18);
                -- Bit [15:0] - UARTPRES UART prescaler (16 bits)   (f_clk /baudrate) - 1
                -- Requires Prescaler value >= 16 because of times 16 oversampling in receiver
                divider_tx <=  wb_dat_i(15 downto 0);
                div16:=unsigned(wb_dat_i(15 downto 4));
                -- "Rounding" of result. if the lowest 4 Bits >=8 then suppress
                -- the subtraction of -1 which effectivly rounds up the result by 1
                if wb_dat_i(3)='0' then
                  div16 := div16 - 1;
                end if;
                divider_rx_q <= "0000" & std_logic_vector(div16);
                divider_reset<='1';
              when "11" =>
                rx_int_en <= wb_dat_i(0);
                tx_int_en <= wb_dat_i(1);
                fifo_int_en <= wb_dat_i(3);

                if wb_dat_i(16)='1' then
                  rx_int_pending<='0';
                end if;
                if wb_dat_i(17)='1' then
                  tx_int_pending<='0';
                end if;
                if wb_dat_i(19)='1' then
                  fifo_int_pending<='0';
                end if;
              when others =>
            end case;
         end if; -- bus cycle
      end if;
    end if;

  end process;

end behave;
