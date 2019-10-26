--   Bonfire CPU
--   (c) 2016,2017 Thomas Hornschuh
--   See license.md for License

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package log2 is

function LOG2(C:INTEGER) return INTEGER;

function power2(n:natural) return natural;

end log2;

package body log2 is

function LOG2(C:INTEGER) return INTEGER is -- C should be >0
variable TEMP,COUNT:INTEGER;
begin
  if C=1 then
    report "log2 called with argument value 1" severity note;
    return 0;
  end if;

  TEMP:=0;
  COUNT:=C;
  while COUNT>1 loop
    TEMP:=TEMP+1;
    assert (count mod 2) = 0
      report "log2 called with non power of two value"
      severity error;
    COUNT:=COUNT/2;
  end loop;

  return TEMP;
end;

function power2(n:natural) return natural is
variable res: natural;
begin
  --res:=1;
  --for i in 1 to n loop
    --res:=res*2;
  --end loop;
  --return res;
  return 2**n;
end;

end log2;
