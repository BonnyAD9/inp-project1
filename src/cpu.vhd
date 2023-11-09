-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2023 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): jmeno <login AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
    port (
        CLK   : in std_logic;  -- hodinovy signal
        RESET : in std_logic;  -- asynchronni reset procesoru
        EN    : in std_logic;  -- povoleni cinnosti procesoru

        -- synchronni pamet RAM

        -- adresa do pameti
        DATA_ADDR  : out std_logic_vector(12 downto 0);
        -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
        DATA_WDATA : out std_logic_vector(7 downto 0);
        -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
        DATA_RDATA : in std_logic_vector(7 downto 0);
        -- cteni (0) / zapis (1)
        DATA_RDWR  : out std_logic;
        -- povoleni cinnosti
        DATA_EN    : out std_logic;

        -- vstupni port

        -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
        IN_DATA   : in std_logic_vector(7 downto 0);
        -- data platna
        IN_VLD    : in std_logic;
        -- pozadavek na vstup data
        IN_REQ    : out std_logic;

        -- vystupni port

        -- zapisovana data
        OUT_DATA : out  std_logic_vector(7 downto 0);
        -- LCD je zaneprazdnen (1), nelze zapisovat
        OUT_BUSY : in std_logic;
        -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
        OUT_WE   : out std_logic;

        -- stavove signaly

        -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat
        -- program
        READY    : out std_logic;
        -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu
        -- (narazil na instrukci halt)
        DONE     : out std_logic
    );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
begin

 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti,
 -- ze:
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty,
 --     protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene
 --      - signaly.

end behavioral;

