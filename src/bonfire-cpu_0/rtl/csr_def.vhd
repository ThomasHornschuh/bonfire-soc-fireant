--   Bonfire CPU
--   (c) 2016,2017 Thomas Hornschuh
--   See license.md for License

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;

package csr_def is

subtype t_csr_adrprefix is std_logic_vector(3 downto 0);
constant m_stdprefix : t_csr_adrprefix := x"3";
constant m_nonstdprefix : t_csr_adrprefix :=x"7";
constant m_roprefix : t_csr_adrprefix :=x"F";

subtype t_csr_adr  is std_logic_vector(11 downto 0);
subtype t_csr_adr8 is std_logic_vector(7 downto 0);
subtype t_csr_word is std_logic_vector(31 downto 0);
subtype t_csr_adr4 is std_logic_vector(3 downto 0);
subtype t_mcause is std_logic_vector(4 downto 0);

-- Exception causes

--#define CAUSE_MISALIGNED_FETCH 0x0
--#define CAUSE_FAULT_FETCH 0x1
--#define CAUSE_ILLEGAL_INSTRUCTION 0x2
--#define CAUSE_BREAKPOINT 0x3
--#define CAUSE_MISALIGNED_LOAD 0x4
--#define CAUSE_FAULT_LOAD 0x5
--#define CAUSE_MISALIGNED_STORE 0x6
--#define CAUSE_FAULT_STORE 0x7
--#define CAUSE_USER_ECALL 0x8
--#define CAUSE_SUPERVISOR_ECALL 0x9
--#define CAUSE_HYPERVISOR_ECALL 0xa
--#define CAUSE_MACHINE_ECALL 0xb

constant CAUSE_MISALIGNED_FETCH : natural := 0;
constant CAUSE_ILLEGAL_INSTRUCTION : natural := 2;
constant CAUSE_BREAKPOINT : natural := 3;
constant CAUSE_MISALIGNED_LOAD : natural := 4;
constant CAUSE_MISALIGNED_STORE : natural := 6;
constant CAUSE_MACHINE_ECALL : natural := 11;

constant IRQ_CODE_MSOFTWARE : natural := 3;
constant IRQ_CODE_MTIMER : natural := 7;
constant IRQ_CODE_MEXTERNAL : natural := 11;
constant IRQ_CODE_LOCAL_BASE : natural := 16;


-- trap setup registers
constant status : t_csr_adr8 := x"00"; --  Machine status register.
constant isa    : t_csr_adr8 := x"01";
constant edeleg : t_csr_adr8 := x"02";
constant ideleg : t_csr_adr8 := x"03";
constant a_ie   : t_csr_adr8 := x"04";
constant tvec : t_csr_adr8 :=   x"05";
constant counteren : t_csr_adr8 :=   x"06";

--Read only Machine Information Registers
constant vendorid : t_csr_adr8 :=  X"11";
constant marchid :  t_csr_adr8 :=  X"12";
constant impid   :  t_csr_adr8 :=  X"13";
constant hartid  :  t_csr_adr8 :=  X"14";

--Trap Handling
constant scratch : t_csr_adr8 :=   x"40";
constant epc: t_csr_adr8 :=        x"41";
constant cause : t_csr_adr8 :=     x"42";
constant tval  : t_csr_adr8 :=     x"43";
constant a_ip : t_csr_adr8 :=      x"44";

--Couunters
-- Cycle counter
constant a_mcycle  : t_csr_adr := x"B00";
constant a_minstret : t_csr_adr := x"B02";

-- non standard registers
constant m_bonfire_csr : t_csr_adr :=x"7C0";

-- Version 1.30
constant major_version : natural := 1;
constant minor_version : natural := 31;
constant impvers : std_logic_vector(31 downto 0) := std_logic_vector(to_unsigned(major_version,16)) &
                                                    std_logic_vector(to_unsigned(minor_version,16));

-- Interrupts

subtype t_lirq_flags is std_logic_vector(15 downto 0);

type t_irq_enable is record
   msie,mtie : std_logic;
   meie : std_logic;
   lie : t_lirq_flags;
end record;

constant c_enable_init : t_irq_enable :=('0','0','0',(others=>'0'));

type t_irq_pending is record
   msip,mtip : std_logic;
   meip : std_logic;
   lip : t_lirq_flags;
end record;

constant c_pending_init : t_irq_pending :=('0','0','0',(others=>'0'));


type t_bonfire_csr is record
  sstep,dummy : std_logic;

end record;

function csr_group(adr : std_logic_vector) return t_csr_adr4;
function csr_item(adr : std_logic_vector) return t_csr_adr4;
function csr_mode(adr : std_logic_vector) return t_csr_adr4;



function get_misa(divider_en:boolean;mul_arch:string) return t_csr_word;
function get_mstatus(pie : std_logic; ie : std_logic) return t_csr_word;
function get_mip(ir: t_irq_pending) return t_csr_word;
function get_mie(ir: t_irq_enable) return t_csr_word;
function irq_cause(cause : natural) return t_mcause;
function get_bonfire_csr(b_csr : t_bonfire_csr) return t_csr_word;

function cause_csr(is_irq: std_logic;cause:t_mcause) return t_csr_word;

procedure set_mip(csr: in t_csr_word;signal ir : out t_irq_pending);
procedure set_mie(csr: in t_csr_word;signal ir : out t_irq_enable);
procedure set_bonfire_csr(csr: in t_csr_word;signal b_csr : out t_bonfire_csr);


end csr_def;

package body csr_def is

function csr_group(adr : std_logic_vector) return t_csr_adr4 is
begin
  return adr(7 downto 4);
end;


function csr_item(adr : std_logic_vector) return t_csr_adr4 is
begin
  return adr(3 downto 0);
end;

function csr_mode(adr : std_logic_vector) return t_csr_adr4 is
begin
  return adr(11 downto 8);
end;



function get_misa(divider_en:boolean;mul_arch:string) return t_csr_word is
variable misa : t_csr_word := "0100" & X"0000000";
begin
  misa(8):='1';
  if divider_en and mul_arch /= "none" then
    misa(12):='1';
  end if;
  return misa;
end;

function get_mstatus(pie : std_logic; ie : std_logic) return t_csr_word is
variable s : t_csr_word := (others=>'0');
begin
  s(12 downto 11) := "11"; -- MPP previous privilege level, always "machine" currently
  s(7) := pie;
  s(3) := ie;

  return s;

end;

function get_mip(ir: t_irq_pending) return t_csr_word is
variable s : t_csr_word := (others=>'0');
begin
  s(3):=ir.msip;
  s(7):=ir.mtip;
  s(11):=ir.meip;
  s(31 downto 16):=ir.lip;
  return s;
end;

function get_mie(ir: t_irq_enable) return t_csr_word is
variable s : t_csr_word := (others=>'0');
begin
  s(3):=ir.msie;
  s(7):=ir.mtie;
  s(11):=ir.meie;
  s(31 downto 16):=ir.lie;
  return s;
end;

function irq_cause(cause : natural) return t_mcause is
variable r : t_mcause;
begin
  r:=std_logic_vector(to_unsigned(cause,r'length));
  return r;
end;

function get_bonfire_csr(b_csr : t_bonfire_csr) return t_csr_word is
variable s : t_csr_word := (others=>'0');
begin
  s(0):=b_csr.sstep;
  return s;

end;

procedure set_mip(csr: in t_csr_word;signal ir : out t_irq_pending) is
begin
  ir.msip <= csr(3);
  ir.mtip <= csr(7);
  ir.meip <= csr(11);
  ir.lip <= csr(31 downto 16);
end;

procedure set_mie(csr: in t_csr_word;signal ir : out t_irq_enable) is
begin
  ir.msie <= csr(3);
  ir.mtie <= csr(7);
  ir.meie <= csr(11);
  ir.lie <= csr(31 downto 16);
end;

procedure set_bonfire_csr(csr: in t_csr_word;signal b_csr : out t_bonfire_csr) is
begin
  b_csr.sstep <= csr(0);

end;

function cause_csr(is_irq: std_logic;cause:t_mcause) return t_csr_word is
variable r: t_csr_word;
begin
  r:=(others=>'0');
  r(cause'range):=cause;
  r(r'high):=is_irq;
  return r;
end;

end csr_def;
