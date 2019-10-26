----------------------------------------------------------------------------------

-- Create Date: 04/03/2019
-- Design Name:
-- Module Name: bonfire_dcache_set - Behavioral

-- The Bonfire Processor Project, (c) 2016-2019 Thomas Hornschuh

--

-- License: See LICENSE or LICENSE.txt File in git project root.
----------------------------------------------------------------------------------



library IEEE;
use IEEE.STD_LOGIC_1164.all;


package bonfire_dcache_help  is
  function max(a:integer; b: integer ) return integer;
end package  bonfire_dcache_help;

package body  bonfire_dcache_help is

  function max(a:integer; b: integer ) return integer  is
  begin
    if a>b then
      return a;
    else
      return b;
    end if;
  end function;
  
end package body;
