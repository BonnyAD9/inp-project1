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

    type State_t is (
        S_SCAN,
        S_HALT,
        S_FETCH,
        S_DECODE,
        S_JMPF,
        S_JMPB,
        S_OUT,
        S_IN,
    );

    type Instruction_t is (
        I_NEXT,    -- '>'
        I_PREV,    -- '<'
        I_INC,     -- '+'
        I_DEC,     -- '-'
        I_IFEZ,    -- '['
        I_IFNZ,    -- ']'
        I_BREAK,   -- '~'
        I_PRINT,   -- '.'
        I_READ,    -- ','
        I_TAPE,    -- '@'
        I_COMMENT, -- other
    );

    signal state      : State_t;
    signal decoded    : Instruction_t;
    signal decode     : std_logic;
    signal read_tape  : std_logic;
    signal value      : std_logic_vector(7  downto 0);
    signal tape_index : std_logic_vector(12 downto 0);
    signal code_index : std_logic_vector(12 downto 0);
begin
    -- FSM
    process (CLK, RST, EN) is
        variable next_state : State_t;
    begin
        if RST = '1' then
            state <= SCAN;
            DATA_EN <= '0';
            IN_REQ <= '0';
            OUT_WE <= '0';
            tape_index <= 0;
            code_index <= 0;
            decode <= '0';
        elsif rising_edge(CLK) and EN = '1' then
            next_state := state;

            DATA_ADDR <= code_index;
            DATA_EN <= '1';
            DATA_RDWR <= '0';
            IN_REQ <= '0';
            OUT_DATA <= value;
            OUT_WE <= '0';
            READY <= '1';
            DONE <= '0';
            decode <= '0';
            read_tape <= '0';

            case state is
                when S_SCAN =>
                    if decoded = I_TAPE then
                        next_state := S_FETCH;
                    end if;
                when S_FETCH =>
                    next_state := S_DECODE;
                when S_DECODE =>
                    next_state := S_FETCH;
            end case;

            case next_state is
                when S_SCAN =>
                    decode <= '1';
                    DATA_ADDR <= tape_index;
                    tape_index <= tape_index + 1;
                when S_FETCH =>
                    decode <= '1';
                    code_index <= code_index + 1;
                when S_DECODE =>
                    case decoded is
                        when I_NEXT =>
                            read_tape <= '1';
                            tape_index <= tape_index + 1;
                            DATA_ADDR <= tape_index + 1;
                        when I_PREV =>
                            read_tape <= '1';
                            tape_index <= tape_index - 1;
                            DATA_ADDR <= tape_index - 1;
                        when I_INC =>
                            value <= value + 1;
                            DATA_WDATA <= value + 1;
                            DATA_ADDR <= tape_index;
                            DATA_RDWR <= '1';
                        when I_DEC =>
                            value <= value - 1;
                            DATA_WDATA <= value - 1;
                            DATA_ADDR <= tape_index;
                            DATA_RDWR <= '1';
                        when I_IFEZ =>
                            if value = 0 then
                                code_index <= code_index + 1;
                                next_state := S_JMPF;
                                decode <= '1';
                            end if;
                        when I_IFNZ =>
                            if value /= 0 then
                                next_state := S_JMPB;
                                DATA_ADDR <= code_index - 2;
                                code_index <= code_index - 1;
                            end if;
                        when I_BREAK =>
                            code_index <= code_index + 1;
                            next_state := S_JMPF;
                            decode <= '1';
                        when I_PRINT =>
                            if OUT_BUSY = '0' then
                                OUT_WE <= '1';
                            else
                                next_state := S_OUT;
                            end if;
                        when I_READ =>
                            IN_REQ <= '1';
                            next_state := S_IN;
                        when I_TAPE =>
                            next_state := S_HALT;
                            DONE <= '1';
                        when I_COMMENT:
                            decode <= '1';
                            code_index <= code_index + 1;
                            next_state := I_FETCH;
                    end case;
            end case;

            state <= next_state;
        end if;
    end process;

    process (EN, RST, DATA_RDATA, read_tape) is
    begin
        if RST = '1' then
            value <= 0;
        elsif EN = '1' and read_tape = '1' then
            value <= DATA_RDATA;
        end if;
    end process;

    -- decoder
    process (EN, RST, DATA_RDATA, decode) is
        constant NEXTT : std_logic_vector(7 downto 0) := "0111110"; -- '>'
        constant PREVT : std_logic_vector(7 downto 0) := "0111100"; -- '<'
        constant INC   : std_logic_vector(7 downto 0) := "0101011"; -- '+'
        constant DEC   : std_logic_vector(7 downto 0) := "0101101"; -- '-'
        constant IFEZ  : std_logic_vector(7 downto 0) := "1011011"; -- '['
        constant IFNZ  : std_logic_vector(7 downto 0) := "1011101"; -- ']'
        constant BREAK : std_logic_vector(7 downto 0) := "1111110"; -- '~'
        constant PRINT : std_logic_vector(7 downto 0) := "0101110"; -- '.'
        constant READT : std_logic_vector(7 downto 0) := "0101100"; -- ','
        constant TAPE  : std_logic_vector(7 downto 0) := "1000000"; -- '@'
    begin
        if RST = '1' then
            decoded <= I_COMMENT;
        elsif EN = '1' and decode = '1' then
            decoded <= I_COMMENT;
            case DATA_RDATA is
                when NEXTT =>
                    decoded <= I_NEXT;
                when PREVT =>
                    decoded <= I_PREV;
                when INC =>
                    decoded <= I_INC;
                when DEC =>
                    decoded <= I_DEC;
                when IFEZ =>
                    decoded <= I_IFEZ;
                when IFNZ =>
                    decoded <= I_IFNZ;
                when BREAK =>
                    decoded <= I_BREAK;
                when PRINT =>
                    decoded <= I_PRINT;
                when READT =>
                    decoded <= I_READ;
                when TAPE =>
                    decoded <= I_TAPE;
            end case;
        end if;
    end process;
 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti,
 -- ze:
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty,
 --     protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene
 --      - signaly.

end behavioral;
