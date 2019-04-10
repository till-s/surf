-------------------------------------------------------------------------------
-- File       : UartAxiLiteMaster.vhd
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description: 
-- Implementation of a UART register access protocol
-- Converts ASCII UART messages into AXI-Lite bus accesses
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
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.StdRtlPkg.all;
use work.TextUtilPkg.all;
use work.AxiLitePkg.all;

entity UartAxiLiteMaster is

   generic (
      TPD_G                  : time                  := 1 ns;
      AXIL_CLK_FREQ_G        : real                  := 125.0e6;
      UART_BAUD_RATE_G       : integer               := 115200;
      UART_STOP_BITS_G       : integer range 1 to 2  := 1;
      UART_PARITY_G          : string                := "NONE";  -- "NONE" "ODD" "EVEN"
      UART_FIFO_BRAM_EN_G    : boolean               := false;
      UART_FIFO_ADDR_WIDTH_G : integer range 4 to 48 := 5);
   port (
      axilClk          : in  sl;
      axilRst          : in  sl;
      -- Transmit parallel interface
      mAxilWriteMaster : out AxiLiteWriteMasterType;
      mAxilWriteSlave  : in  AxiLiteWriteSlaveType;
      mAxilReadMaster  : out AxiLiteReadMasterType;
      mAxilReadSlave   : in  AxiLiteReadSlaveType;
      -- Serial IO
      tx               : out sl;
      rx               : in  sl);

end entity UartAxiLiteMaster;

architecture rtl of UartAxiLiteMaster is

   -- SRPv3 Constants
   constant SRP_VERSION_C      : slv(7 downto 0) := x"03";
   constant NON_POSTED_READ_C  : slv(1 downto 0) := "00";
   constant NON_POSTED_WRITE_C : slv(1 downto 0) := "01";
   constant POSTED_WRITE_C     : slv(1 downto 0) := "10";
   constant NULL_C             : slv(1 downto 0) := "11";

   constant AXIS_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(4);

   type CmdStateType is (
      WAIT_START_S);

   type RspStateType is (
      WAIT_START_S);

   type RegType is record
      cmdState    : CmdStateType;
      cmdOpCode : slv(1 downto 0);
      uartRxReady : sl;
      sAxisMaster : AxiStreamMasterType;

      rspState    : RspStateType;
      uartTxData  : slv(7 downto 0);
      uartTxValid : sl;
      mAxisSlave  : AxiStreamSlaveType;
   end record RegType;

   constant REG_INIT_C : RegType := (
      cmdOpCode => NULL_C;
      uartTxData  => (others => '0'),
      uartTxValid => '0',
      uartRxReady => '1');

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

   signal uartRxData  : slv(7 downto 0);
   signal uartRxValid : sl;
   signal sAxisSlave  : AxiStreamSlaveType;

   signal uartTxReady : sl;
   signal mAxisMaster : AxiStreamMasterType;

   -- translate a hex character 0-9 A-F into an slv
   function hexToSlv (hex : slv(7 downto 0)) return slv is
      variable char : character;
   begin
      char := character'val(conv_integer(hex));
      return toSlv(int(char), 4);
   end function;

   function slvToHex (nibble : slv(3 downto 0)) return slv is
   begin
      return toSlv(character'pos(chr(conv_integer(nibble))), 8);
   end function;

begin

   -------------------------------------------------------------------------------------------------
   -- Instantiate UART
   -------------------------------------------------------------------------------------------------
   U_UartWrapper_1 : entity work.UartWrapper
      generic map (
         TPD_G             => TPD_G,
         CLK_FREQ_G        => AXIL_CLK_FREQ_G,
         BAUD_RATE_G       => BAUD_RATE_G,
         STOP_BITS_G       => STOP_BITS_G,
         PARITY_G          => PARITY_G,
         DATA_WIDTH_G      => 8,
         FIFO_BRAM_EN_G    => FIFO_BRAM_EN_G,
         FIFO_ADDR_WIDTH_G => FIFO_ADDR_WIDTH_G)
      port map (
         clk     => axilClk,            -- [in]
         rst     => axilRst,            -- [in]
         wrData  => r.uartTxData,       -- [in]
         wrValid => r.uartTxValid,      -- [in]
         wrReady => uartTxReady,        -- [out]
         rdData  => uartRxData,         -- [out]
         rdValid => uartRxValid,        -- [out]
         rdReady => rin.uartRxReady,    -- [in]
         tx      => tx,                 -- [out]
         rx      => rx);                -- [in]

   -------------------------------------------------------------------------------------------------
   -- SRPv3
   -- UART messages are converted into SRPv3 frames and this SRPv3 module does the heavy lifting
   -------------------------------------------------------------------------------------------------
   U_SrpV3AxiLite_1 : entity work.SrpV3AxiLite
      generic map (
         TPD_G               => TPD_G,
         SLAVE_READY_EN_G    => true,
         GEN_SYNC_FIFO_G     => true,
         AXIL_CLK_FREQ_G     => AXIL_CLK_FREQ_G,
         AXI_STREAM_CONFIG_G => ssiAxiStreamConfig(4))
      port map (
         sAxisClk         => axilClk,           -- [in]
         sAxisRst         => axilRst,           -- [in]
         sAxisMaster      => r.sAxisMaster,     -- [in]
         sAxisSlave       => sAxisSlave,        -- [out]
--         sAxisCtrl        => sAxisCtrl,         -- [out]
         mAxisClk         => axilClk,           -- [in]
         mAxisRst         => axilRst,           -- [in]
         mAxisMaster      => mAxisMaster,       -- [out]
         mAxisSlave       => rin.mAxisSlave,    -- [in]
         axilClk          => axilClk,           -- [in]
         axilRst          => axilRst,           -- [in]
         mAxilWriteMaster => mAxilWriteMaster,  -- [out]
         mAxilWriteSlave  => mAxilWriteSlave,   -- [in]
         mAxilReadMaster  => mAxilReadMaster,   -- [out]
         mAxilReadSlave   => mAxilReadSlave);   -- [in]

   comb : process (axilAck, axilRst, r, uartRxData, uartRxValid, uartTxReady) is
      variable v : RegType;

      procedure uartTx (byte : in slv(7 downto 0)) is
      begin
         v.uartTxValid := '1';
         v.uartTxData  := byte;
      end procedure uartTx;

      procedure uartTx (char : in character) is
      begin
         uartTx(toSlv(character'pos(char), 8));
      end procedure uartTx;

      function isSpace (byte : slv(7 downto 0)) return boolean is
      begin
         return (byte = character'pos(' '));
      end function isSpace;

      function isEOL (byte : slv(7 downto 0)) return boolean is
      begin
         return (byte = character'pos(CR) or
                 byte = character'pos(LF));
      end function isEOL;

   begin
      v := r;

      -- Format:
      -- "w|W IDHEX ADDRHEX DATAHEX0 [DATAHEX1] [DATAHEX2]...\r|\n"
      -- Writes echo'd back with resp code
      -- "r|R IDHEX ADDRHEX [NUMREADSHEX] \r|\n"
      -- Resp: "r|R ADDRHEX DATAHEX0 [DATAHEX1] [DATAHEX2]...\r|\n"
      -- Blank lines ignored
      -- Extra words ignored.

      -- Auto clear flow control signals
      if (uartTxReady = '1') then
         v.uartTxValid := '0';
      end if;

      if (sAxisSlave.tReady = '1') then
         v.sAxisMaster.tValid := '0';
      end if;

      v.uartTxReady       := '0';
      v.mAxisSlave.tReady := '0';

      case r.cmdState is
         when CLEAR_S =>
            v.cmdOpCode := NULL_C;
            v.cmdId := (others => '0');
            v.cmdAddr := (others => '0');
            v.cmdData := (others => '0');
            
         when WAIT_OPCODE_S =>
            -- Any characters before 'r', 'w' or 'p' are thrown out
            v.sAxisMaster.tData := (others => '0');
            v.sAxisMaster.tData(7 downto 0) := SRP_VERSION_C;
            
            if (uartRxValid = '1' and v.sAxisMaster.tValid = '0') then
               v.uartRxReady := '1';
               -- Write
               if (uartRxData = toSlv(character'pos('w'), 8) or
                   uartRxData = toSlv(character'pos('W'), 8)) then
                  v.cmdOpCode := NON_POSTED_WRITE_C;

               --Read
               elsif (uartRxData = toSlv(character'pos('r'), 8) or
                      uartRxData = toSlv(character'pos('R'), 8)) then
                  v.cmdOpCode := NON_POSTED_READ_C;

               -- Posted write
               elsif (uartRxData = toSlv(character'pos('p'), 8) or
                      uartRxData = toSlv(character'pos('P'), 8)) then
                  v.cmdOpCode := POSTED_WRITE_C;
               end if;

               -- Send the first txn when a space is seen
               v.sAxisMaster.tData(9 downto 8) := v.cmdOpCode;               
               if (isSpace(uartRxData) and r.cmdOpCode != NULL_C) then
                  ssiSetUserSof(AXIS_CONFIG_C, v.sAxisMaster, '1');
                  v.sAxisMaster.tValid := '1';
                  v.cmdState := ID_S;
               end if;

            end if;

         when ID_S =>
            if (uartRxValid = '1' v.sAxisMaster.tValid = '0') then
               v.uartRxReady := '1';
               v.cmdId := r.cmdId(27 downto 0) & hexToSlv(uartRxData);

               -- Send the ID txn when a space is seen
               if (isSpace(uartRxData)) then
                  v.cmdId := r.cmdId;
                  v.sAxisMaster.tData(31 downto 0) := r.cmdId;
                  v.sAxisMaster.tValid := '1';
                  v.cmdState := ADDR0_S;
               end if;

               -- Go back to start if EOL
               if (isEOL(uartRxData)) then
                  v.cmdState := TERMINATE_S;
               end if;
            end if;

         when ADDR0_S =>
            if (uartRxValid = '1' v.sAxisMaster.tValid = '0') then
               v.uartRxReady := '1';
               v.cmdAddr := r.cmdAddr(27 downto 0) & hexToSlv(uartRxData);

               -- Send the ID txn when a space is seen
               if (isSpace(uartRxData)) then
                  v.cmdAddr := r.cmdAddr;
                  v.sAxisMaster.tData(31 downto 0) := r.cmdAddr;
                  v.sAxisMaster.tValid := '1';
                  v.cmdState := ADDR1_S;
               end if;

               -- Go back to start if EOL
               if (isEOL(uartRxData)) then
                  v.cmdState := TERMINATE_S;
               end if;
            end if;
            
         when ADDR1_S =>
            v.sAxisMaster.tData(31 downto 0) := (others => '0');            
            if (v.sAxisMaster.tValid = '0') then
               v.sAxisMaster.tValid := '1';
               v.cmdState := 
            end if;
            

         when RD_DATA_S =>
            v.count  := r.count + 1;
            uartTx(slvToHex(r.rdData(31 downto 28)));
            v.rdData := r.rdData(27 downto 0) & "0000";
            if (r.count = 7) then
               v.state := RD_DATA_SPACE_S;
            end if;


         when RD_DATA_SPACE_S =>
            uartTx(' ');
            v.state := DONE_S;

         when DONE_S =>
            -- Send resp code first cycle of this state
            if (r.axilReq.request = '1') then
               -- Send the response code                  
               uartTx(slvToHex(resize(axilAck.resp, 4)));
            end if;

            -- Release request and wait for done to fall
            -- Send closing CR when it does
            v.axilReq.request := '0';
            if (axilAck.done = '0') then
               uartTx(CR);
               v.state := WAIT_START_S;
            end if;

      end case;

      if (axilRst = '1') then
         v := REG_INIT_C;
      end if;

      rin <= v;
   end process comb;

   seq : process (axilClk) is
   begin
      if (rising_edge(axilClk)) then
         r <= rin after TPD_G;
      end if;
   end process seq;


end architecture rtl;
