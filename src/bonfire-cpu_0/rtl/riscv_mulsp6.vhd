----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    18:10:57 05/01/2017 
-- Design Name: 
-- Module Name:    riscv_mulsp6 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
-- Pipelined multiplier
-- Code dervied from Xilinx Documentation
-- The code pattern below will make XST to infer a pipelined multiplier with a latency of 4 cycles
-- out of 4 DSP48 blocks and some additional logic  
-- The multiplication operation placed outside the
-- process block and the pipeline stages represented
-- as single registers
-- 01.05.2017 TH: Added support for signed/unsigned 64 Bit result to implement the mulh and mulhsu instructions.
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity riscv_mulsp6 is
port(
      clk_i: in std_logic;
      rst_i: in std_logic;
      ce_i: in std_logic;
      op1_i: in std_logic_vector(31 downto 0);
      op2_i: in std_logic_vector(31 downto 0);
      op1_signed_i : in std_logic;
      op2_signed_i : in std_logic;
      
      ce_o: out std_logic;
      result_o: out std_logic_vector(31 downto 0);
      result_high_o : out std_logic_vector(31 downto 0)
   );
   
   attribute mult_style: string;
   attribute mult_style of riscv_mulsp6: entity is "pipe_block";
   
end riscv_mulsp6;

architecture rtl of riscv_mulsp6 is


constant  A_port_size : natural  := op1_i'length;
constant  B_port_size : natural  := op2_i'length;

subtype t_signed_33 is signed (A_port_size downto 0);
subtype t_signed_66 is signed ( (t_signed_33'length*2 -1) downto 0);

signal a_in, b_in : t_signed_33;
signal mult_res : t_signed_66;
signal pipe_1, pipe_2, pipe_3,MULT : signed(63 downto 0);  -- t_signed_66;

signal ce_1 : std_logic :='0';
signal ce_2 : std_logic :='0';
signal ce_3 : std_logic :='0';
signal ce_4 : std_logic :='0';



begin
   mult_res <= a_in * b_in;

   result_o <= std_logic_vector(MULT(31 downto 0));
   result_high_o <= std_logic_vector(MULT(63 downto 32));

process (clk_i)

 function extend_op(op_i : std_logic_vector(31 downto 0);is_signed:std_logic) return t_signed_33 is
 variable high_bit : std_logic;
 begin
   high_bit := is_signed and op_i(op_i'high); 
   return signed(high_bit & op_i);
 end;

begin

   if rising_edge(clk_i) then
        if rst_i='1' then
          ce_1 <= '0';
          ce_2 <= '0';
          ce_3 <= '0';
          ce_4 <= '0';
          ce_o <= '0';
        else
      
          -- input pipeline stage
          
          a_in <= extend_op(op1_i,op1_signed_i); 
          b_in <= extend_op(op2_i,op2_signed_i);
          ce_1 <= ce_i;
         
          -- internal pipeline stages
          pipe_1 <= mult_res(63 downto 0); ce_2 <= ce_1;
          pipe_2 <= pipe_1;   ce_3 <= ce_2;
          pipe_3 <= pipe_2;   ce_4 <= ce_3;
          MULT <= pipe_3;     ce_o <= ce_4;
        end if;  
    end if;
end process;


end rtl;

