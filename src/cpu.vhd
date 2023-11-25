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
        S_INIT_START,
        S_INIT_FETCH,
        S_INIT,
        S_INIT_FINISH_WAIT,
        S_INIT_FINISH,
        S_FETCH,
        S_DECODE,
        S_NEXT_LOAD,
        S_NEXT_SAVE,
        S_NEXT_DECODE,
        S_PREV_LOAD,
        S_PREV_SAVE,
        S_PREV_DECODE,
        S_INC,
        S_DEC,
        S_JFORW,
        S_JBACK_FETCH,
        S_JBACK,
        S_PRINT_WAIT,
        S_PRINT_WAIT_DECODE,
        S_PRINT,
        S_PRINT_DECODE,
        S_READ_WAIT,
        S_READ_WAIT_DECODE,
        S_READ,
        S_FINISH,
        S_HALT
    );

    type Instruction_t is (
        I_NEXT,   -- '>'
        I_PREV,   -- '<'
        I_INC,    -- '+'
        I_DEC,    -- '-'
        I_IFEZ,   -- '['
        I_IFNZ,   -- ']'
        I_BREAK,  -- '~'
        I_PRINT,  -- '.'
        I_READ,   -- ','
        I_HALT,   -- '@'
        I_COMMENT -- other
    );

    signal state      : State_t;
    signal decoded    : Instruction_t;
    signal decode     : std_logic;
    signal value      : std_logic_vector(7  downto 0);
    signal tape_index : std_logic_vector(12 downto 0);
    signal code_index : std_logic_vector(12 downto 0);
begin
    -- FSM
    process (CLK, RESET, EN) is
        variable next_state  : State_t;
        variable next_value  : std_logic_vector(7 downto 0);
        variable choose_inst : std_logic;
    begin
        if RESET = '1' then
            state <= S_INIT_START;
            DATA_EN <= '0';
            IN_REQ <= '0';
            OUT_WE <= '0';
            tape_index <= "0000000000000";
            code_index <= "0000000000000";
            decode <= '0';
            DONE <= '0';
            READY <= '0';
        elsif rising_edge(CLK) and EN = '1' then
            next_state := state;
            choose_inst := '0';

            case state is
                when S_INIT_START =>
                    next_state := S_INIT_FETCH;
                when S_INIT_FETCH =>
                    next_state := S_INIT;
                when S_INIT =>
                    if decoded = I_HALT then
                        next_state := S_INIT_FINISH_WAIT;
                    end if;
                when S_INIT_FINISH_WAIT =>
                    next_state := S_INIT_FINISH;
                when S_INIT_FINISH =>
                    choose_inst := '1';
                when S_FETCH =>
                    next_state := S_DECODE;
                when S_DECODE =>
                    choose_inst := '1';
                when S_NEXT_LOAD =>
                    next_state := S_NEXT_SAVE;
                when S_NEXT_SAVE =>
                    next_state := S_NEXT_DECODE;
                when S_NEXT_DECODE =>
                    choose_inst := '1';
                when S_PREV_LOAD =>
                    next_state := S_PREV_SAVE;
                when S_PREV_SAVE =>
                    next_state := S_PREV_DECODE;
                when S_PREV_DECODE =>
                    choose_inst := '1';
                when S_INC =>
                    choose_inst := '1';
                when S_DEC =>
                    choose_inst := '1';
                when S_JFORW =>
                    if decoded = I_IFNZ then
                        next_state := S_DECODE;
                    end if;
                when S_JBACK_FETCH =>
                    next_state := S_JBACK;
                when S_JBACK =>
                    if decoded = I_IFEZ then
                        next_state := S_FETCH;
                    end if;
                when S_PRINT_WAIT =>
                    if OUT_BUSY = '0' then
                        next_state := S_PRINT;
                    end if;
                when S_PRINT_WAIT_DECODE =>
                    if OUT_BUSY = '0' then
                        next_state := S_PRINT;
                    else
                        next_state := S_PRINT_WAIT;
                    end if;
                when S_PRINT =>
                    choose_inst := '1';
                when S_PRINT_DECODE =>
                    choose_inst := '1';
                when S_READ_WAIT =>
                    if IN_VLD = '1' then
                        next_state := S_READ;
                    end if;
                when S_READ_WAIT_DECODE =>
                    if IN_VLD = '1' then
                        next_state := S_READ;
                    else
                        next_state := S_READ_WAIT;
                    end if;
                when S_READ =>
                    choose_inst := '1';
                when S_FINISH =>
                    next_state := S_HALT;
                when S_HALT =>
            end case;

            if choose_inst = '1' then
                case decoded is
                    when I_NEXT =>
                        next_state := S_NEXT_LOAD;
                    when I_PREV =>
                        next_state := S_PREV_LOAD;
                    when I_INC =>
                        next_state := S_INC;
                    when I_DEC =>
                        next_state := S_DEC;
                    when I_IFEZ =>
                        if value = 0 then
                            if decoded = I_IFNZ then
                                next_state := S_DECODE;
                            else
                                next_state := S_JFORW;
                            end if;
                        else
                            next_state := S_DECODE;
                        end if;
                    when I_IFNZ =>
                        if value /= 0 then
                            next_state := S_JBACK_FETCH;
                        else
                            next_state := S_FETCH;
                        end if;
                    when I_BREAK =>
                        if decoded = I_IFNZ then
                            next_state := S_DECODE;
                        else
                            next_state := S_JFORW;
                        end if;
                    when I_PRINT =>
                        if OUT_BUSY = '0' then
                            if state = S_PRINT_DECODE or state = S_PRINT then
                                -- OUT_BUSY wouldn't be yet updated, so we need
                                -- to wait
                                next_state := S_PRINT_WAIT_DECODE;
                            else
                                next_state := S_PRINT_DECODE;
                            end if;
                        else
                            next_state := S_PRINT_WAIT_DECODE;
                        end if;
                    when I_READ =>
                        next_state := S_READ_WAIT_DECODE;
                    when I_HALT =>
                        next_state := S_FINISH;
                    when I_COMMENT =>
                        next_state := S_FETCH;
                end case;
            end if;

            DATA_ADDR <= code_index;
            DATA_EN <= '1';
            DATA_RDWR <= '0';
            DATA_WDATA <= value;
            IN_REQ <= '0';
            OUT_DATA <= value;
            OUT_WE <= '0';
            READY <= '1';
            DONE <= '0';
            decode <= '1';

            case next_state is
                when S_INIT_START => -- this case should never happen
                    READY <= '0';
                    DATA_EN <= '0';
                    decode <= '0';
                when S_INIT_FETCH =>
                    -- fetch the first byte
                    READY <= '0';
                    DATA_ADDR <= tape_index;
                    decode <= '0';
                when S_INIT =>
                    -- fetch bytes until decoded byte is @
                    READY <= '0';
                    DATA_ADDR <= tape_index + 1;
                    tape_index <= tape_index + 1;
                when S_INIT_FINISH_WAIT =>
                    -- wait for the initial tape value
                    -- fetch the first instruction
                    READY <= '0';
                    decode <= '0';
                when S_INIT_FINISH =>
                    -- set the initial value
                    -- decode the first instruction
                    value <= DATA_RDATA;
                    code_index <= code_index + 1;
                    DATA_ADDR <= code_index + 1;
                when S_FETCH =>
                    -- fetch instruction
                    decode <= '0';
                when S_DECODE =>
                    -- decode the instruction
                    code_index <= code_index + 1;
                    DATA_ADDR <= code_index + 1;
                -- move tape forward
                when S_NEXT_LOAD =>
                    -- fetch value at the next tape index
                    -- decode the current instruction
                    DATA_ADDR <= tape_index + 1;
                    code_index <= code_index + 1;
                when S_NEXT_SAVE =>
                    -- (value from tape will arive next tick)
                    -- save the current value
                    DATA_ADDR <= tape_index;
                    tape_index <= tape_index + 1;
                    DATA_RDWR <= '1';
                    decode <= '0';
                when S_NEXT_DECODE =>
                    -- save the new value from tape
                    -- the code is already decoded, just fetch next instruction
                    value <= DATA_RDATA;
                    decode <= '0';
                -- move tape backward
                when S_PREV_LOAD =>
                    -- fetch the value at the previous tape index
                    -- decode the current instruction
                    DATA_ADDR <= tape_index - 1;
                    code_index <= code_index + 1;
                when S_PREV_SAVE =>
                    -- (value from tape will arive next tick)
                    -- save the current value
                    DATA_ADDR <= tape_index;
                    tape_index <= tape_index - 1;
                    DATA_RDWR <= '1';
                    decode <= '0';
                when S_PREV_DECODE =>
                    -- save the new value from tape
                    -- the code is already decoded, just fetch next instruction
                    value <= DATA_RDATA;
                    decode <= '0';
                when S_INC =>
                    -- increment the value
                    -- fetch the next instruction
                    -- decode the current instruction
                    value <= value + 1;
                    DATA_ADDR <= code_index + 1;
                    code_index <= code_index + 1;
                when S_DEC =>
                    -- decrement the value, fetch the next instruction
                    -- fetch the next instruction
                    -- decode the current instruction
                    value <= value - 1;
                    DATA_ADDR <= code_index + 1;
                    code_index <= code_index + 1;
                when S_JFORW =>
                    -- fetch the next instruciton
                    -- decode instruction that will arrive the next tick
                    code_index <= code_index + 1;
                    DATA_ADDR <= code_index + 1;
                when S_JBACK_FETCH =>
                    -- fetch the previous instruction
                    decode <= '0';
                    DATA_ADDR <= code_index - 2;
                when S_JBACK =>
                    -- fetch the previous instruction
                    -- decode instruciton that will arrive the next tick
                    DATA_ADDR <= code_index - 3;
                    code_index <= code_index - 1;
                when S_PRINT_WAIT =>
                    -- wait for the output device
                    decode <= '0';
                    DATA_EN <= '0';
                when S_PRINT_WAIT_DECODE =>
                    -- wait for the output device
                    -- decode the current instruction
                    DATA_EN <= '0';
                    DATA_ADDR <= code_index + 1;
                    code_index <= code_index + 1;
                when S_PRINT =>
                    -- write to output
                    -- fetch next instruciton
                    OUT_DATA <= value;
                    OUT_WE <= '1';
                    decode <= '0';
                when S_PRINT_DECODE =>
                    -- write to output
                    -- fetch next instruciton
                    -- decode the current instruction
                    OUT_DATA <= value;
                    OUT_WE <= '1';
                    DATA_ADDR <= code_index + 1;
                    code_index <= code_index + 1;
                when S_READ_WAIT =>
                    -- wait for the input
                    IN_REQ <= '1';
                    DATA_EN <= '0';
                    decode <= '0';
                when S_READ_WAIT_DECODE =>
                    -- wait for the input
                    -- decode current instruction
                    IN_REQ <= '1';
                    DATA_EN <= '0';
                    code_index <= code_index + 1;
                when S_READ =>
                    -- read the data
                    -- fetch next instruction
                    value <= IN_DATA;
                    decode <= '0';
                when S_FINISH =>
                    -- store the value
                    DATA_ADDR <= tape_index;
                    DATA_RDWR <= '1';
                    decode <= '0';
                when S_HALT =>
                    DATA_EN <= '0';
                    decode <= '0';
                    DONE <= '1';
            end case;

            state <= next_state;
        end if;
    end process;

    -- decoder
    process (EN, RESET, DATA_RDATA, decode) is
        constant NEXTT : std_logic_vector(7 downto 0) := "00111110"; -- '>'
        constant PREVT : std_logic_vector(7 downto 0) := "00111100"; -- '<'
        constant INC   : std_logic_vector(7 downto 0) := "00101011"; -- '+'
        constant DEC   : std_logic_vector(7 downto 0) := "00101101"; -- '-'
        constant IFEZ  : std_logic_vector(7 downto 0) := "01011011"; -- '['
        constant IFNZ  : std_logic_vector(7 downto 0) := "01011101"; -- ']'
        constant BREAK : std_logic_vector(7 downto 0) := "01111110"; -- '~'
        constant PRINT : std_logic_vector(7 downto 0) := "00101110"; -- '.'
        constant READT : std_logic_vector(7 downto 0) := "00101100"; -- ','
        constant TAPE  : std_logic_vector(7 downto 0) := "01000000"; -- '@'
    begin
        if RESET = '1' then
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
                    decoded <= I_HALT;
                when others =>
                    decoded <= I_COMMENT;
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
