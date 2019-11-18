----------------------------------------------------------------------------------

-- Module Name:    MainMemory - Behavioral


-- The Bonfire Processor Project, (c) 2016,2017 Thomas Hornschuh


-- License: See LICENSE or LICENSE.txt File in git project root.

-- Memory module to be synthesized as block RAM
-- can be initalized with a file

-- New "single process" version as recommended by Xilinx XST user guide

----------------------------------------------------------------------------------





library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_textio.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

library STD;
use STD.textio.all;

entity MainMemory is
    generic (RamFileName : string := "meminit.ram";
             mode : string := "B";
             ADDR_WIDTH: integer;
             Swapbytes : boolean; -- SWAP Bytes in RAM word in low byte first order to use data2mem
             EnableSecondPort : boolean := true -- enable inference of the second port
            );
    Port ( DBOut : out  STD_LOGIC_VECTOR (31 downto 0);
           DBIn : in  STD_LOGIC_VECTOR (31 downto 0);
           AdrBus : in  STD_LOGIC_VECTOR (ADDR_WIDTH-1 downto 0);
           ENA : in  STD_LOGIC;
           WREN : in  STD_LOGIC_VECTOR (3 downto 0);
           CLK : in  STD_LOGIC;
           -- Second Port ( read only)
           CLKB : in STD_LOGIC;
           ENB : in STD_LOGIC;
           AdrBusB : in  STD_LOGIC_VECTOR (ADDR_WIDTH-1 downto 0);
           DBOutB : out  STD_LOGIC_VECTOR (31 downto 0)

              );
end MainMemory;


architecture Behavioral of MainMemory is

attribute keep_hierarchy : string;
attribute keep_hierarchy of Behavioral: architecture is "TRUE";

constant SIZE : natural := 2**ADDR_WIDTH;

type tRam is array (0 to SIZE-1) of STD_LOGIC_VECTOR (31 downto 0);
subtype tWord is std_logic_vector(31 downto 0);


signal DOA,DOB,DIA : tWord;
signal WEA : STD_LOGIC_VECTOR (3 downto 0);


function doSwapBytes(d : tWord) return tWord is
begin

    return d(7 downto 0)&d(15 downto 8)&d(23 downto 16)&d(31 downto 24);

end;


-- Design time code...
-- Initalizes block RAM form memory file
-- The file does either contain hex values (mode = 'H') or binary values

impure function InitFromFile  return tRam is
FILE RamFile : text; -- is in RamFileName;
variable RamFileLine : line;
variable word : tWord;
variable r : tRam;
variable is_open : boolean;

begin
  report "RamFileName is " & RamFileName;
  if RamFileName /= "" then
    file_open(RamFile,RamFileName,READ_MODE);
    is_open:=true;
  else
    is_open:=false;
  end if;
  for I in tRam'range loop
    if is_open and not endfile(RamFile) then
      readline (RamFile, RamFileLine);
      if mode="H" then
        hread (RamFileLine, word); -- alternative: HEX read
      else
        read(RamFileLine,word);  -- Binary read
      end if;
      if SwapBytes then
        r(I) :=  DoSwapBytes(word);
      else
        r(I) := word;
      end if;
    else
      r(I) := (others=>'0');
    end if;
  end loop;
  if is_open then
    file_close(RamFile);
  end if;
  return r;
end function;

signal ram : tRam:= InitFromFile;

attribute ram_style: string; -- for Xilinx
attribute ram_style of ram: signal is "block";


-- helper component
-- for byte swapping
COMPONENT byte_swapper
    PORT(
        din : IN std_logic_vector(31 downto 0);
        dout : OUT std_logic_vector(31 downto 0)
        );
    END COMPONENT;



begin

   swap: if SwapBytes generate
     -- The Data input bus is swapped with the helper component to avoid
     -- confusing the xilinx synthesis tools which sometimes infer distributed
     -- instead of block RAM
     -- It is important that the byte swapper component has set the keep_hierarchy attribute to TRUE
     -- this will make the byte swap of the input bus invisble for the RAM inference
     bs: byte_swapper PORT MAP(
        din => DBIn,
        dout => DIA
      );

     DBOut<=DoSwapBytes(DOA);
     DBOutB<=DoSwapBytes(DOB);
     WEA(0)<=WREN(3);
     WEA(1)<=WREN(2);
     WEA(2)<=WREN(1);
     WEA(3)<=WREN(0);

   end generate;

   noswap: if not SwapBytes generate
     DIA<=DBIn;
     DBOut<=DOA;
     DBOutB<=DOB;
     WEA<=WREN;
   end generate;




  process(clk)
  variable adr : integer;
  begin
    if rising_edge(clk) then
        if ena = '1' then
           adr :=  to_integer(unsigned(AdrBus));

           for i in 0 to 3 loop
             if WEA(i) = '1' then
                ram(adr)((i+1)*8-1 downto i*8)<= DIA((i+1)*8-1 downto i*8);
             end if;
           end loop;

           DOA <= ram(adr);

         end if;
     end if;

  end process;

  portb:  if EnableSecondPort generate

     process(clkb) begin
        if rising_edge(clkb) then
            if ENB='1' then
               DOB <= ram(to_integer(unsigned(AdrBusB)));
             end if;
         end if;
     end process;

  end generate;

end Behavioral;
