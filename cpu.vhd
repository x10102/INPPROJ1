-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2025 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): <redacted for privacy>
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
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------

architecture behavioral of cpu is

  
  -- Idk how big of a register we need here tbh, 8 bits should be enough?
  signal CNT : std_logic_vector(7 downto 0) := (others => '0');
  signal INCCNT : std_logic := '0';
  signal DECCNT : std_logic := '0';

  -- 13 bits for a 8192 word memory
  signal PC : std_logic_vector(12 downto 0) := (others => '0');
  signal INCPC : std_logic := '0';
  signal DECPC : std_logic := '0';

  -- Same as above
  signal PTR : std_logic_vector(12 downto 0) := (others => '0');
  signal INCPTR : std_logic := '0';
  signal DECPTR : std_logic := '0';

  -- '1' if the PTR register is zero, '0' otherwise
  signal CNT_ZERO : std_logic;

  -- Switches address bus from program to data memory
  -- '0' is prog mem, '1' is data mem
  signal MUX_ADDR_SEL : std_logic := '0';

  -- Selects the data to be sent to RAM
  -- '00' loads a value from the input to RAM
  -- '01' decrements the read value by one
  -- '10' increments the read value by one
  -- '11' interprets the value read as a hex digit and writes back 0xA0 where A is the decoded digit
  -- If the value is outside of <0x30-0x39> union <0x41-0x46> then the output is invalid
  signal MUX_WR_SEL : std_logic_vector(1 downto 0) := "00";

  -- The hexadecimal value decoded from DATA_RDATA
  -- The register is 12 bits wide due to VHDL somehow refusing all my attempts to bit shift during the decoding
  -- So we just concat it with a 4-bit wide zero vector and select only the upper 8 bits in the data_wdata demux
  signal HEX_DIG_DEC : std_logic_vector(11 downto 0) := (others => '0');

  -- A value of 1 signals that the cycle is a do-while cycle
  -- We reuse the states for "end while" for the do cycle to reduce amount of code
  signal WHILE_DOCYCLE : std_logic := '0';

  type cpu_state is (
  s_init, s_memscan, s_memscan_check, s_idle, s_die, s_fetch, s_decode, s_waitwrite, s_nop, -- Non-instruction states
  i_incptr, i_decptr, i_hexval, i_increment, i_decrement,     -- Instructions
  i_while, i_endwhile, i_do, i_enddo,
  i_print, i_load,
  i_increment_wb, i_decrement_wb, i_hexval_wb,                 -- Memory writeback
  i_while_check, i_while_readprg, i_while_countnested, i_while_checkstop,
  i_endwhile_check, i_endwhile_readprg, i_endwhile_countnested, i_endwhile_checkstop,
  i_print_wait
  );
  signal STATE : cpu_state := s_init;
  signal NSTATE : cpu_state;
begin
  --Decode the value from a hex digit by substracting a constant and shifting the result, latches to previous value outside the aforementioned range
  HEX_DIG_DEC <=  (DATA_RDATA - x"30") & "0000" when (DATA_RDATA <= x"39" and DATA_RDATA >= x"30") else
                  (DATA_RDATA - x"37") & "0000" when (DATA_RDATA <= x"46" and DATA_RDATA >= x"41") else (others => '0');
  
  -- Check if CNT register is zero
  CNT_ZERO <= '1' when (CNT = x"00") else '0';

  -- Address MUX
  -- Switches data address between program counter and memory pointer
  DATA_ADDR <=  PC  when (MUX_ADDR_SEL = '0') else
                PTR when (MUX_ADDR_SEL = '1') else (others => '0');

  -- Data Write MUX
  -- Selects value to write back
  DATA_WDATA <= IN_DATA                 when (MUX_WR_SEL = "00") else
                DATA_RDATA - 1          when (MUX_WR_SEL = "01") else
                DATA_RDATA + 1          when (MUX_WR_SEL = "10") else
                HEX_DIG_DEC(7 downto 0) when (MUX_WR_SEL = "11") else (others => '0');

  -- CNT counter process
  PROC_CNT : process(CLK, RESET) begin
    if (RESET = '1') then
      CNT <= (others => '0');
    elsif rising_edge(CLK) and (EN = '1') then
      if (INCCNT = '1') then
        CNT <= CNT + 1;
      elsif (DECCNT = '1') then
        CNT <= CNT - 1;
      end if;
    end if;
  end process;

  -- PC counter process
  PROC_PC : process(CLK, RESET) begin
    if (RESET = '1') then
      PC <= (others => '0');
    elsif rising_edge(CLK) and (EN = '1') then
      if (INCPC = '1') then
        PC <= PC + 1;
      elsif (DECPC = '1') then
        PC <= PC - 1;
      end if;
    end if;
  end process;

  -- PTR counter process
  PROC_PTR : process(CLK, RESET) begin
    if (RESET = '1') then
      PTR <= (others => '0');
    elsif rising_edge(CLK) and (EN = '1') then
      if (INCPTR = '1') then
        PTR <= PTR + 1;
      elsif (DECPTR = '1') then
        PTR <= PTR - 1;
      end if;
    end if;
  end process;

  -- State register process
  PROC_SWITCH_STATE : process(CLK, RESET) begin
    if (RESET = '1') then
      STATE <= s_init;
    elsif rising_edge(CLK) and (EN = '1') then
      STATE <= NSTATE;
    end if;
  end process;

  PROC_CTRL_FSM: process(STATE, DATA_RDATA, IN_VLD, OUT_BUSY, CNT_ZERO, WHILE_DOCYCLE) begin
    -- Disable memory and set mode to read by default
    DATA_EN <= '0';
    DATA_RDWR <= '0';
    -- Clear output port, disable output and data request signals
    OUT_DATA <= (others => '0');
    OUT_WE <= '0';
    OUT_INV <= '0';
    IN_REQ <= '0';
    -- Clear counter signals
    INCPTR <= '0';
    DECPTR <= '0';
    INCPC <= '0';
    DECPC <= '0';
    INCCNT <= '0';
    DECCNT <= '0';
    case STATE is
      -- ============ NON-INSTRUCTION STATES ============
      -- Initial state on powerup
      when s_init =>
        -- Clear "ready" and "done" signals
        READY <= '0';
        DONE <= '0';
        -- Start memory test
        NSTATE <= s_memscan;
      -- Memory scan - initialize PTR
      when s_memscan =>
        -- Enable memory and select PTR as address source
        -- Start reading until we reach the mem/dat separator, then enable ready signal
        -- And move to idle state
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        MUX_ADDR_SEL <= '1';
        NSTATE <= s_memscan_check;
      when s_memscan_check =>
        INCPTR <= '1';
        if(DATA_RDATA = x"40") then
          NSTATE <= s_idle;
        else 
          NSTATE <= s_memscan;
        end if;
      when s_idle =>
        READY <= '1';
        NSTATE <= s_fetch;
      when s_fetch =>
        MUX_ADDR_SEL <= '0'; -- Select program memory
        DATA_RDWR <= '1'; -- Select read
        DATA_EN <= '1'; -- Enable memory
        NSTATE <= s_decode;
      -- Instruction decoder
      when s_decode =>
        if((DATA_RDATA >= x"30" and DATA_RDATA <= x"39") or (DATA_RDATA >= x"41" and DATA_RDATA <= x"46")) then
          NSTATE <= i_hexval;
        else
          case DATA_RDATA is
            when x"3E" => NSTATE <= i_incptr; -- >
            when x"3C" => NSTATE <= i_decptr; -- <
            when x"2B" => NSTATE <= i_increment; -- +
            when x"2D" => NSTATE <= i_decrement; -- -
            when x"5B" => NSTATE <= i_while; -- [
            when x"5D" => NSTATE <= i_endwhile; -- ]
            when x"28" => NSTATE <= i_do; -- (
            when x"29" => NSTATE <= i_enddo; -- )
            when x"2E" => NSTATE <= i_print; -- .
            when x"2C" => NSTATE <= i_load; -- ,
            when x"40" => NSTATE <= s_die;
            when others => INCPC <= '1'; NSTATE <= s_fetch;
          end case;
        end if;
      when s_nop =>  
        INCPC <= '1';
        NSTATE <= s_fetch;
      when s_waitwrite =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        NSTATE <= s_fetch;
      when s_die =>
        -- Halt the processor until reset
        INCPC <= '0';
        INCPTR <= '0';
        DONE <= '1';
        NSTATE <= s_die;
    
    -- ============ INSTRUCTIONS ============
    when i_incptr =>
      INCPTR <= '1';
      INCPC <= '1';
      NSTATE <= s_fetch;
    when i_decptr =>
      DECPTR <= '1';
      INCPC <= '1';
      NSTATE <= s_fetch;
    when i_hexval =>
      MUX_ADDR_SEL <= '0'; -- Select program memory
      DATA_RDWR <= '1';    -- Read
      DATA_EN <= '1';      -- Enable memory
      NSTATE <= i_hexval_wb;
    when i_hexval_wb =>
      MUX_ADDR_SEL <= '1'; -- Select data memory
      DATA_RDWR <= '0';    -- Write
      DATA_EN <= '1';      -- Enable memory
      MUX_WR_SEL <= "11";  -- Hex write mode
      INCPC <= '1';
      NSTATE <= s_waitwrite;
    when i_increment =>
      MUX_ADDR_SEL <= '1';
      DATA_RDWR <= '1';
      DATA_EN <= '1';
      MUX_WR_SEL <= "10"; -- Increment write mode
      NSTATE <= i_increment_wb;
    when i_increment_wb =>
      MUX_ADDR_SEL <= '1'; 
      DATA_RDWR <= '0';    
      DATA_EN <= '1';
      MUX_WR_SEL <= "10";
      INCPC <= '1';      
      NSTATE <= s_waitwrite;
    when i_decrement =>
      MUX_ADDR_SEL <= '1';
      DATA_RDWR <= '1';
      DATA_EN <= '1';
      MUX_WR_SEL <= "01"; -- Decrement write mode
      NSTATE <= i_decrement_wb;
    when i_decrement_wb =>
      MUX_ADDR_SEL <= '1'; 
      DATA_RDWR <= '0';    
      DATA_EN <= '1';
      MUX_WR_SEL <= "01";
      INCPC <= '1';   
      NSTATE <= s_waitwrite;
    when i_do =>
      INCPC <= '1';
      NSTATE <= s_fetch; -- Skip
    when i_while =>
      INCPC <= '1';
      MUX_ADDR_SEL <= '1';
      DATA_RDWR <= '1';
      DATA_EN <= '1';
      NSTATE <= i_while_check;
    when i_while_check =>
      if(DATA_RDATA = x"0") then
        MUX_ADDR_SEL <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        INCCNT <= '1';
        INCPC <= '1';
        NSTATE <= i_while_readprg;
      else
        NSTATE <= s_fetch;
      end if;
    when i_while_readprg =>
      MUX_ADDR_SEL <= '0';
      DATA_RDWR <= '1';
      DATA_EN <= '1';
      NSTATE <= i_while_countnested;
    when i_while_countnested =>
      if(DATA_RDATA = x"5D") then
        DECCNT <= '1';
      elsif(DATA_RDATA = x"5B") then
        INCCNT <= '1';
      end if;
      NSTATE <= i_while_checkstop;
    when i_while_checkstop =>
      INCPC <= '1';
      if(CNT_ZERO = '1') then
        NSTATE <= s_fetch;
      else
        NSTATE <= i_while_readprg;
      end if;
    when i_endwhile =>
      WHILE_DOCYCLE <= '0';
      MUX_ADDR_SEL <= '1';
      DATA_RDWR <= '1';
      DATA_EN <= '1';
      NSTATE <= i_endwhile_check;
    when i_endwhile_check =>
      if(DATA_RDATA = x"0") then
        INCPC <= '1';
        NSTATE <= s_fetch;
      else
        MUX_ADDR_SEL <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        INCCNT <= '1';
        DECPC <= '1';
        NSTATE <= i_endwhile_readprg;
      end if;
    when i_endwhile_readprg =>
      MUX_ADDR_SEL <= '0';
      DATA_RDWR <= '1';
      DATA_EN <= '1';
      NSTATE <= i_endwhile_countnested;
    when i_endwhile_countnested =>
      -- Choose the character based on cycle type
      if((DATA_RDATA = x"5D" and WHILE_DOCYCLE = '0') or (DATA_RDATA = x"29" and WHILE_DOCYCLE = '1')) then
        INCCNT <= '1';
      elsif((DATA_RDATA = x"5B" and WHILE_DOCYCLE = '0') or (DATA_RDATA = x"28" and WHILE_DOCYCLE = '1')) then
        DECCNT <= '1';
      end if;
      NSTATE <= i_endwhile_checkstop;
    when i_endwhile_checkstop =>
      if(CNT_ZERO = '1') then
        NSTATE <= s_fetch;
      else
        DECPC <= '1';
        NSTATE <= i_endwhile_readprg;
      end if;
    when i_enddo =>
      WHILE_DOCYCLE <= '1';
      MUX_ADDR_SEL <= '1';
      DATA_RDWR <= '1';
      DATA_EN <= '1';
      NSTATE <= i_endwhile_check;
    when i_load =>
      if(IN_VLD = '1') then
        MUX_ADDR_SEL <= '1';
        MUX_WR_SEL <= "00";
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        IN_REQ <= '0';
        INCPC <= '1';
        NSTATE <= s_waitwrite;
      elsif(IN_VLD = '0') then
        IN_REQ <= '1';
        NSTATE <= i_load;
      end if;
    when i_print =>
      if(OUT_BUSY = '0') then
        MUX_ADDR_SEL <= '1';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        NSTATE <= i_print_wait;
      elsif(OUT_BUSY = '1') then
        NSTATE <= i_print;
      end if;
    when i_print_wait =>
      OUT_DATA <= DATA_RDATA;
      OUT_WE <= '1';
      NSTATE <= s_nop;
    end case;
    
  end process;
  

end behavioral;

