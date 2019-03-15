-------------------------------------------------------------------------------
-- File       : TrueDualPortRam.vhd
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: This will infer this module as Block RAM only
--
-- NOTE: TDP ram with read enable logic is not supported.
-------------------------------------------------------------------------------
-- This file is part of 'SLAC Firmware Standard Library'.
-- It is subject to the license terms in the LICENSE.txt file found in the 
-- top-level directory of this distribution and at: 
--    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html. 
-- No part of 'SLAC Firmware Standard Library', including this file, 
-- may be copied, modified, propagated, or distributed except according to 
-- the terms contained in the LICENSE.txt file.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

use work.StdRtlPkg.all;

entity TrueDualPortRam is
   -- MODE_G = {"no-change","read-first","write-first"}
   generic (
      TPD_G            : time                   := 1 ns;
      COMMON_CLK_G     : boolean                := false;
      MEMORY_TYPE_G    : string                 := "block";
      -- init file?
      ADDR_WIDTH_A_G   : positive               := 4;
      ADDR_WIDTH_B_G   : positive               := 4;
      DATA_WIDTH_A_G   : positive               := 16;
      DATA_WIDTH_B_G   : positive               := 16;
      BYTE_WR_EN_A_G   : boolean                := false;
      BYTE_WR_EN_B_G   : boolean                := false;
      BYTE_WIDTH_A_G   : positive               := DATA_WIDTH_A_G;  -- Must be 8, 9
      BYTE_WIDTH_B_G   : positive               := DATA_WIDTH_B_G;  -- Must be 8, 9 or data width
      READ_LATENCY_A_G : natural range 0 to 100 := 1;
      READ_LATENCY_B_G : natural range 0 to 100 := 1;
      WRITE_MODE_A_G   : string;
      WRITE_MODE_B_G   : string);
   port (
      -- Port A     
      clka     : in  sl                                                        := '0';
      ena      : in  sl                                                        := '1';
      wea      : in  sl                                                        := '0';
      weaByte  : in  slv(wordCount(DATA_WIDTH_A_G, BYTE_WIDTH_A_G)-1 downto 0) := (others => '0');
      rsta     : in  sl                                                        := '0';
      addra    : in  slv(ADDR_WIDTH_A_G-1 downto 0)                            := (others => '0');
      dina     : in  slv(DATA_WIDTH_A_G-1 downto 0)                            := (others => '0');
      douta    : out slv(DATA_WIDTH_A_G-1 downto 0);
      regcea   : in  sl                                                        := '1';  -- Clock enable for extra output reg. Only used when DOA_REG_G = true
      -- Port B
      clkb     : in  sl                                                        := '0';
      enb      : in  sl                                                        := '1';
      web      : in  sl                                                        := '0';
      webBytes : in  slv(wordCount(DATA_WIDTH_B_G, BYTE_WIDTH_B_G)-1 downto 0) := (others => '0');
      rstb     : in  sl                                                        := '0';
      addrb    : in  slv(ADDR_WIDTH_B_G-1 downto 0)                            := (others => '0');
      dinb     : in  slv(DATA_WIDTH_B_G-1 downto 0)                            := (others => '0');
      doutb    : out slv(DATA_WIDTH_B_G-1 downto 0);
      regceb   : in  sl                                                        := '1');  -- Clock enable for extra output reg. Only used when DOA_REG_G = true
end TrueDualPortRam;

architecture rtl of TrueDualPortRam is

   -- Set byte width to word width if byte writes not enabled
   -- Otherwise block ram parity bits wont be utilized
   constant BYTE_WIDTH_A_C      : natural := ite(BYTE_WR_EN_A_G, BYTE_WIDTH_A_G, DATA_WIDTH_A_G);
   constant NUM_BYTES_A_C       : natural := wordCount(DATA_WIDTH_A_G, BYTE_WIDTH_A_C);
   constant FULL_DATA_WIDTH_A_C : natural := NUM_BYTES_A_C*BYTE_WIDTH_A_C;

   constant BYTE_WIDTH_B_C      : natural := ite(BYTE_WR_EN_B_G, BYTE_WIDTH_B_G, DATA_WIDTH_B_G);
   constant NUM_BYTES_B_C       : natural := wordCount(DATA_WIDTH_B_G, BYTE_WIDTH_B_C);
   constant FULL_DATA_WIDTH_B_C : natural := NUM_BYTES_B_C*BYTE_WIDTH_B_C;

   constant DEPTH_A_C : positive := 2**ADDR_WIDTH_A_G;
   constant DEPTH_B_C : positive := 2**ADDR_WIDTH_B_G;

   constant MIN_WIDTH_C : integer := minimum(DATA_WIDTH_A_G, DATA_WIDTH_B_G);
   constant MAX_WIDTH_C : integer := maximum(DATA_WIDTH_A_G, DATA_WIDTH_B_G);
   constant MAX_DEPTH_C : integer := maximum(DEPTH_A_C, DEPTH_B_C);

   constant WORD_WIDTH_C : integer := minimum(DATA_WIDTH_A_G, DATA_WIDTH_B_G);
   constant RATIO_C      : integer := MAX_WIDTH_C / MIN_WIDTH_C;

   -- Number of A words that fit in B, or 1 if 0
   constant NUM_WORDS_A_C : integer := wordCount(WIDTH_B_C, WIDTH_A_C);
   constant NUM_WORDS_B_C : integer := wordCount(WIDTH_A_C, WIDTH_B_C);

   constant BYTES_PER_WORD_A_C : integer := wordCount(NUM_WORDS_A_C, NUM_BYTES_A_C);
   constant BYTES_PER_WORD_B_C : integer := wordCount(NUM_WORDS_B_C, NUM_BYTES_B_C);

   constant WIDER_A_C : boolean := (DATA_WIDTH_A_G > DATA_WIDTH_B_G);
   constant WIDER_B_C : boolean := (DATA_WIDTH_B_G < DATA_WIDTH_A_G);
   constant EQUAL_C   : boolean := (DATA_WIDTH_B_G = DATA_WIDTH_A_G);

   constant INIT_C     : slv(MIN_WIDTH_C-1 downto 0) := (others => '0');
   -- Shared memory 
   type mem_type is array (0 to MAX_DEPTH_C-1) of slv(MIN_WIDTH_C-1 downto 0);
   shared variable mem : mem_type                    := (others => INIT_C);

   signal iDoutA : slv(WIDTH_A_C-1 downto 0);
   signal iDoutB : slv(WIDTH_B_C-1 downto 0);

   signal iWeaByte : slv(weaByte'range);
   signal iWebByte : slv(webByte'range);

   function subAddrA (wordNum : integer) return slv
   is
      variable ret : slv(log2(NUM_WORDS_A_C)-1 downto 0);
   begin
      ret := toSlv(part, ret'length);
      return ret;
   end function;

   function subAddrB (wordNum : integer) return slv
   is
      variable ret : slv(log2(NUM_WORDS_B_C)-1 downto 0);
   begin
      ret := toSlv(part, ret'length);
      return ret;
   end function;

begin

   -- MODE_G check
--    assert (MODE_G = "no-change") or (MODE_G = "read-first") or (MODE_G = "write-first")
--       report "MODE_G must be either no-change, read-first, or write-first"
--       severity failure;
--    -- ALTERA_RAM_G check
--    assert ((ALTERA_RAM_G = "M512")
--            or (ALTERA_RAM_G = "M4K")
--            or (ALTERA_RAM_G = "M9K")
--            or (ALTERA_RAM_G = "M10K")
--            or (ALTERA_RAM_G = "M20K")
--            or (ALTERA_RAM_G = "M144K")
--            or (ALTERA_RAM_G = "M-RAM"))
--       report "Invalid ALTERA_RAM_G string"
--       severity failure;

   iWeaByte <= weaByte when BYTE_WR_EN_A_G else (others => wea);
   iWebByte <= webByte when BYTE_WR_EN_B_G else (others => web);

   -------------------------------------------------------------------------------------------------
   -- No Change Mode
   -------------------------------------------------------------------------------------------------
   NO_CHANGE_MODE : if MODE_G = "no-change" generate
      -- Port A
      process(clka)
      begin
         if rising_edge(clka) then
            if (ena = '1') then
               for i in 0 to NUM_BYTES_C-1 loop
                  if (weaByteInt(i) = '1') then
                     mem(conv_integer(addra))((i+1)*BYTE_WIDTH_C-1 downto i*BYTE_WIDTH_C) :=
                        resize(dina(minimum(DATA_WIDTH_G-1, (i+1)*BYTE_WIDTH_C-1) downto i*BYTE_WIDTH_C), BYTE_WIDTH_C);
                  end if;
               end loop;
            end if;
         end if;
      end process;

      -- Vivado does crazy stupid things if output isn't broken out into its own process in
      -- no-change mode
      process (clka) is
      begin
         if (rising_edge(clka)) then
            if (ena = '1' and weaByte = 0 and wea = '0') then
               iDoutA <= mem(conv_integer(addra)) after TPD_G;
            end if;
            if rsta = RST_POLARITY_G and DOA_REG_G = false then
               iDoutA <= INIT_C after TPD_G;
            end if;
         end if;
      end process;

      -- Port B
      process(clkb)
      begin
         if rising_edge(clkb) then
            if (enb = '1') then
               for i in 0 to NUM_BYTES_C-1 loop
                  if (webByteInt(i) = '1') then
                     mem(conv_integer(addrb))((i+1)*BYTE_WIDTH_C-1 downto i*BYTE_WIDTH_C) :=
                        resize(dinb(minimum(DATA_WIDTH_G-1, (i+1)*BYTE_WIDTH_C-1) downto i*BYTE_WIDTH_C), BYTE_WIDTH_C);
                  end if;
               end loop;
            end if;
         end if;
      end process;

      process (clkb) is
      begin
         if (rising_edge(clkb)) then
            if (enb = '1' and webByte = 0 and web = '0') then
               iDoutB <= mem(conv_integer(addrb)) after TPD_G;
            end if;
            if rstb = RST_POLARITY_G and DOA_REG_G = false then
               iDoutB <= INIT_C after TPD_G;
            end if;
         end if;
      end process;

   end generate;

   -------------------------------------------------------------------------------------------------
   -- Read first mode
   -------------------------------------------------------------------------------------------------
   READ_FIRST_MODE : if MODE_G = "read-first" generate
      -- Port A
      process(clka)
      begin
         if rising_edge(clka) then
            if (ena = '1') then
               -- Read
               for i in 0 to NUM_WORDS_A_C-1 loop
                  iDoutA((i+1)*WIDTH_A_C -1 downto i * WIDTH_A_C) <=
                     mem(conv_integer(addra & subAddrA(i)));
               end loop;

               -- Write
               for i in 0 to NUM_WORDS_A_C-1 loop
                  if (BYTES_PER_WORD_A_C > 0) then
                     -- Multiple bytes per mem word
                     for j in 0 to BYTES_PER_WORD_A_C-1 loop
                        if (iWeaByte(i*BYTES_PER_WORD_C+j) = '1') then
                           ram(conv_integer(addrA & subAddrA(i)))((j+1)*BYTE_WIDTH_A_C-1 downto j*BYTE_WIDTH_A_C) :=
                              resize(dina(minimum(DATA_WIDTH_A_G-1, (j+1)*BYTE_WIDTH_A_C-1) downto j*BYTE_WIDTH_A_C), BYTE_WIDTH_A_C);
                        end if;
                     end loop;
                  else
                     -- multiple mem words per byte
                     if (iWeaByte(i/NUM_BYTES_A_C) = '1') then
                        ram(conv_integer(addrA & subAddrA(i))) :=
                           dina((i+1)*WIDTH_A_C-1 downto i*WIDTH_A_C);
                     end if;
                  end if;
               end loop;
            end if;
         end if;
         if rsta = '1' and READ_LATENCY_A_G = 1 then
            iDoutA <= INIT_C after TPD_G;
         end if;
      end if;
   end process;

   -- Port B
   process(clkb)
   begin
      if rising_edge(clkb) then
         if (enb = '1') then
            -- Read
            for i in 0 to NUM_WORDS_B_C-1 loop
               iDoutB((i+1)*WIDTH_B_C -1 downto i * WIDTH_B_C) <=
                  mem(conv_integer(addrb & subAddrB(i)));
            end loop;

            -- Write
            for i in 0 to NUM_WORDS_B_C-1 loop
               if (BYTES_PER_WORD_B_C > 0) then
                  -- Multiple bytes per mem word
                  for j in 0 to BYTES_PER_WORD_B_C-1 loop
                     if (iWebByte(i*BYTES_PER_WORD_C+j) = '1') then
                        ram(conv_integer(addrB & subAddrB(i)))((j+1)*BYTE_WIDTH_B_C-1 downto j*BYTE_WIDTH_B_C) :=
                           resize(dinb(minimum(DATA_WIDTH_B_G-1, (j+1)*BYTE_WIDTH_B_C-1) downto j*BYTE_WIDTH_B_C), BYTE_WIDTH_B_C);
                     end if;
                  end loop;
               else
                  -- multiple mem words per byte
                  if (iWebByte(i/NUM_BYTES_B_C) = '1') then
                     ram(conv_integer(addrB & subAddrB(i))) :=
                        dinb((i+1)*WIDTH_B_C-1 downto i*WIDTH_B_C);
                  end if;
               end if;
            end loop;
         end if;
      end if;
      if rstb = '1' and READ_LATENCY_B_G = 1 then
         iDoutB <= INIT_C after TPD_G;
      end if;
   end if;
end process;

end generate;

-------------------------------------------------------------------------------------------------
-- Write first mode
-------------------------------------------------------------------------------------------------
WRITE_FIRST_MODE : if MODE_G = "write-first" generate
   -- Port A
   process(clka)
   begin
      if rising_edge(clka) then
         if (ena = '1') then
            for i in 0 to NUM_BYTES_C-1 loop
               if (weaByteInt(i) = '1') then
                  mem(conv_integer(addra))((i+1)*BYTE_WIDTH_C-1 downto i*BYTE_WIDTH_C) :=
                     resize(dina(minimum(DATA_WIDTH_G-1, (i+1)*BYTE_WIDTH_C-1) downto i*BYTE_WIDTH_C), BYTE_WIDTH_C);
               end if;
            end loop;
            iDoutA <= mem(conv_integer(addra)) after TPD_G;
         end if;
         if rsta = RST_POLARITY_G and DOA_REG_G = false then
            iDoutA <= INIT_C after TPD_G;
         end if;
      end if;

   end process;

   -- Port B
   process(clkb)
   begin
      if rising_edge(clkb) then
         if (enb = '1') then
            for i in 0 to NUM_BYTES_C-1 loop
               if (webByteInt(i) = '1') then
                  mem(conv_integer(addrb))((i+1)*BYTE_WIDTH_C-1 downto i*BYTE_WIDTH_C) :=
                     resize(dinb(minimum(DATA_WIDTH_G-1, (i+1)*BYTE_WIDTH_C-1) downto i*BYTE_WIDTH_C), BYTE_WIDTH_C);
               end if;
            end loop;
            iDoutB <= mem(conv_integer(addrb)) after TPD_G;
         end if;
         if rstb = RST_POLARITY_G and DOB_REG_G = false then
            iDoutB <= INIT_C after TPD_G;
         end if;
      end if;

   end process;

end generate;

                   -------------------------------------------------------------------------------------------------
                   -- Optional data output registers
                   -------------------------------------------------------------------------------------------------
                   NO_DOUT_A_REG : if (not DOA_REG_G) generate
                      douta <= iDoutA(DATA_WIDTH_G-1 downto 0);
                   end generate NO_DOUT_A_REG;

                                   DOUT_A_REG : if (DOA_REG_G) generate
                                      process (clka) is
                                      begin
                                         if (rising_edge(clka)) then
                                            if (rstA = RST_POLARITY_G) then
                                               douta <= (others => '0') after TPD_G;
                                            elsif (regcea = '1') then
                                               douta <= iDoutA(DATA_WIDTH_G-1 downto 0) after TPD_G;
                                            end if;
                                         end if;
                                      end process;
                                   end generate DOUT_A_REG;

                                                NO_DOUT_B_REG : if (not DOB_REG_G) generate
                                                   doutb <= iDoutB(DATA_WIDTH_G-1 downto 0);
                                                end generate NO_DOUT_B_REG;

                                                                DOUT_B_REG : if (DOB_REG_G) generate
                                                                   process (clkb) is
                                                                   begin
                                                                      if (rising_edge(clkb)) then
                                                                         if (rstB = RST_POLARITY_G) then
                                                                            doutb <= (others => '0') after TPD_G;
                                                                         elsif (regceb = '1') then
                                                                            doutb <= iDoutB(DATA_WIDTH_G-1 downto 0) after TPD_G;
                                                                         end if;
                                                                      end if;
                                                                   end process;
                                                                end generate DOUT_B_REG;

                                                                             end rtl;
