----------------------------------------------------------------------------------
-- Company:
-- Engineer:
--
-- Create Date:    13:03:14 10/16/2016
-- Design Name:
-- Module Name:    ram2048x8 - Behavioral
-- Project Name:
-- Target Devices:
-- Tool versions:
-- Description:
-- 2K*8 Bit Dual Port RAM with "write first" strategy
-- Using Xilinx Primitives instead of inference to have more control over BRAM allocation.
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
library UNISIM;
use UNISIM.VComponents.all;

entity ram2048x8 is
    Port ( DOutA : out  STD_LOGIC_VECTOR (7 downto 0);
           DInA : in  STD_LOGIC_VECTOR (7 downto 0);
           AdrA : in  STD_LOGIC_VECTOR (10 downto 0);
           ENA : in  STD_LOGIC;
           WRENA : in  STD_LOGIC;
           CLKA : in  STD_LOGIC;
           DoutB : out  STD_LOGIC_VECTOR (7 downto 0);
           DInB : in  STD_LOGIC_VECTOR (7 downto 0);
           AdrB : in  STD_LOGIC_VECTOR (10 downto 0);
           ENB : in  STD_LOGIC;
           WRENB : in  STD_LOGIC;
           CLKB : in  STD_LOGIC);
end ram2048x8;

architecture Behavioral of ram2048x8 is


signal DOA,DOB,DIA,DIB : std_logic_vector(31 downto 0);

signal DOPA,DOPB,DIPA,DIPB : std_logic_vector(3 downto 0);
signal ADDRA,ADDRB : std_logic_vector(13 downto 0);
signal WEA,WEB : std_logic_vector(3 downto 0);



begin

  DIPA<=(others=>'0');
  DIPB<=(others=>'0');

  DIA<= X"000000"&DinA;
  DIB<= X"000000"&DInB;

  ADDRA<=AdrA&"000";
  ADDRB<=AdrB&"000";

  DOutA<= DOA(7 downto 0);
  DoutB<= DOB(7 downto 0);

  wegen: for i in WEA'range generate
      WEA(i)<=WRENA;
      WEB(i)<=WRENB;
  end generate;


  RAMB16BWER_inst : RAMB16BWER
    generic map (
      -- DATA_WIDTH_A/DATA_WIDTH_B: 0, 1, 2, 4, 9, 18, or 36
      DATA_WIDTH_A => 9,
      DATA_WIDTH_B => 9,
      -- DOA_REG/DOB_REG: Optional output register (0 or 1)
      DOA_REG => 0,
      DOB_REG => 0,
      -- EN_RSTRAM_A/EN_RSTRAM_B: Enable/disable RST
      EN_RSTRAM_A => FALSE,
      EN_RSTRAM_B => FALSE,

      -- INIT_A/INIT_B: Initial values on output port
      INIT_A => X"000000000",
      INIT_B => X"000000000",
      -- INIT_FILE: Optional file used to specify initial RAM contents
      INIT_FILE => "NONE",

      -- SIM_COLLISION_CHECK: Collision check enable "ALL", "WARNING_ONLY", "GENERATE_X_ONLY" or "NONE"
      SIM_COLLISION_CHECK => "ALL",
      -- SIM_DEVICE: Must be set to "SPARTAN6" for proper simulation behavior
      SIM_DEVICE => "SPARTAN6",

      -- WRITE_MODE_A/WRITE_MODE_B: "WRITE_FIRST", "READ_FIRST", or "NO_CHANGE"
      WRITE_MODE_A => "WRITE_FIRST",
      WRITE_MODE_B => "WRITE_FIRST"
   )
   port map (
      -- Port A Data: 32-bit (each) output: Port A data
      DOA => DOA,       -- 32-bit output: A port data output
      DOPA => DOPA,     -- 4-bit output: A port parity output
      -- Port B Data: 32-bit (each) output: Port B data
      DOB => DOB,       -- 32-bit output: B port data output
      DOPB => DOPB,     -- 4-bit output: B port parity output
      -- Port A Address/Control Signals: 14-bit (each) input: Port A address and control signals
      ADDRA => ADDRA,   -- 14-bit input: A port address input
      CLKA => CLKA,     -- 1-bit input: A port clock input
      ENA => ENA,       -- 1-bit input: A port enable input
      REGCEA => '0', -- 1-bit input: A port register clock enable input
      RSTA => '0',     -- 1-bit input: A port register set/reset input
      WEA => WEA,       -- 4-bit input: Port A byte-wide write enable input
      -- Port A Data: 32-bit (each) input: Port A data
      DIA => DIA,       -- 32-bit input: A port data input
      DIPA => DIPA,     -- 4-bit input: A port parity input
      -- Port B Address/Control Signals: 14-bit (each) input: Port B address and control signals
      ADDRB => ADDRB,   -- 14-bit input: B port address input
      CLKB => CLKB,     -- 1-bit input: B port clock input
      ENB => ENB,       -- 1-bit input: B port enable input
      REGCEB => '0', -- 1-bit input: B port register clock enable input
      RSTB => '0',     -- 1-bit input: B port register set/reset input
      WEB => WEB,       -- 4-bit input: Port B byte-wide write enable input
      -- Port B Data: 32-bit (each) input: Port B data
      DIB => DIB,       -- 32-bit input: B port data input
      DIPB => DIPB      -- 4-bit input: B port parity input
   );


end Behavioral;

