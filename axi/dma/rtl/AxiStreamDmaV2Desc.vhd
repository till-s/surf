-------------------------------------------------------------------------------
-- File       : AxiStreamDmaV2Desc.vhd
-- Company    : SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------
-- Description:
-- Descriptor manager for AXI DMA read and write engines.
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
use ieee.NUMERIC_STD.all;

use work.StdRtlPkg.all;
use work.AxiPkg.all;
use work.AxiLitePkg.all;
use work.AxiDmaPkg.all;
use work.ArbiterPkg.all;

entity AxiStreamDmaV2Desc is
   generic (
      TPD_G             : time := 1 ns;
      
      SYNTH_MODE_G      : string := "inferred";
      MEMORY_TYPE_G     : string := "block";       

      -- Number of read & write DMA engines to support for each descriptor engine
      CHAN_COUNT_G      : integer range 1 to 16 := 1;

      -- Base address of descriptor registers & FIFOs
      AXIL_BASE_ADDR_G  : slv(31 downto 0) := x"00000000";

      -- Support ready handshaking for output AXI transactions. If false user
      -- must provide an AXI FIFO at the output with SLAVE_READY_EN_G = False
      AXI_READY_EN_G    : boolean := false;

      -- Configuration of AXI bus, must be 64 bits or 128 bits wide
      AXI_CONFIG_G      : AxiConfigType         := AXI_CONFIG_INIT_C;

      -- Number of descriptor entries in write FIFO and return ring buffers 
      DESC_AWIDTH_G     : integer range 4 to 32 := 12;

      -- Choose between one-clock arbitration for return descritors or count and check selection
      DESC_ARB_G        : boolean := true;

      -- Set to true to wait for bvalid status return before moving on to next descriptor DMA
      ACK_WAIT_BVALID_G : boolean := true);

   port (
      -- Clock/Reset
      axiClk          : in  sl;
      axiRst          : in  sl;
      -- Local AXI Lite Bus
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;
      -- Additional signals
      interrupt       : out sl;
      online          : out slv(CHAN_COUNT_G-1 downto 0);
      acknowledge     : out slv(CHAN_COUNT_G-1 downto 0);
      -- DMA write descriptor request, ack and return
      dmaWrDescReq    : in  AxiWriteDmaDescReqArray(CHAN_COUNT_G-1 downto 0);
      dmaWrDescAck    : out AxiWriteDmaDescAckArray(CHAN_COUNT_G-1 downto 0);
      dmaWrDescRet    : in  AxiWriteDmaDescRetArray(CHAN_COUNT_G-1 downto 0);
      dmaWrDescRetAck : out slv(CHAN_COUNT_G-1 downto 0);
      -- DMA read descriptor request, ack and return
      dmaRdDescReq    : out AxiReadDmaDescReqArray(CHAN_COUNT_G-1 downto 0);
      dmaRdDescAck    : in  slv(CHAN_COUNT_G-1 downto 0);
      dmaRdDescRet    : in  AxiReadDmaDescRetArray(CHAN_COUNT_G-1 downto 0);
      dmaRdDescRetAck : out slv(CHAN_COUNT_G-1 downto 0);
      -- Config
      axiRdCache      : out slv(3 downto 0);
      axiWrCache      : out slv(3 downto 0);
      -- AXI Interface
      axiWriteMaster  : out AxiWriteMasterType;
      axiWriteSlave   : in  AxiWriteSlaveType;
      axiWriteCtrl    : in  AxiCtrlType := AXI_CTRL_UNUSED_C);
end AxiStreamDmaV2Desc;

architecture rtl of AxiStreamDmaV2Desc is

   -- Descriptor width, 64-bits or 128-bits
   constant DESC_128_EN_C  : boolean := AXI_CONFIG_G.DATA_BYTES_C >= 16;
  
   constant CB_COUNT_C : integer := 2;

   constant LOC_INDEX_C     : natural          := 0;
   constant LOC_BASE_ADDR_C : slv(31 downto 0) := AXIL_BASE_ADDR_G(31 downto 16) & x"0000";
   constant LOC_NUM_BITS_C  : natural          := 14;

   constant ADDR_INDEX_C     : natural          := 1;
   constant ADDR_BASE_ADDR_C : slv(31 downto 0) := AXIL_BASE_ADDR_G(31 downto 16) & x"4000";
   constant ADDR_NUM_BITS_C  : natural          := 14;

   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(CB_COUNT_C-1 downto 0) := (
      LOC_INDEX_C     => (
         baseAddr     => LOC_BASE_ADDR_C,
         addrBits     => LOC_NUM_BITS_C,
         connectivity => x"FFFF"),
      ADDR_INDEX_C    => (
         baseAddr     => ADDR_BASE_ADDR_C,
         addrBits     => ADDR_NUM_BITS_C,
         connectivity => x"FFFF"));

   constant CHAN_SIZE_C  : integer := bitSize(CHAN_COUNT_G-1);
   constant RET_COUNT_C  : integer := CHAN_COUNT_G*2;
   constant RET_SIZE_C   : integer := bitSize(RET_COUNT_C-1);

   constant RD_FIFO_CNT_C  : integer := ite(DESC_128_EN_C,4,2);
   constant RD_FIFO_BITS_C : integer := RD_FIFO_CNT_C * 32;

   constant WR_FIFO_CNT_C  : integer := ite(DESC_128_EN_C,2,1);
   constant WR_FIFO_BITS_C : integer := WR_FIFO_CNT_C * 32;

   type DescStateType is (IDLE_S, WRITE_S, READ_S, WAIT_S);

   type RegType is record

      -- Write descriptor interface
      dmaWrDescAck    : AxiWriteDmaDescAckArray(CHAN_COUNT_G-1 downto 0);
      dmaWrDescRetAck : slv(CHAN_COUNT_G-1 downto 0);

      -- Read descriptor interface
      dmaRdDescReq    : AxiReadDmaDescReqArray(CHAN_COUNT_G-1 downto 0);
      dmaRdDescRetAck : slv(CHAN_COUNT_G-1 downto 0);

      -- Axi-Lite
      axilReadSlave  : AxiLiteReadSlaveType;
      axilWriteSlave : AxiLiteWriteSlaveType;

      -- AXI
      axiWriteMaster : AxiWriteMasterType;

      -- Configuration
      wrBaseAddr   : slv(63 downto 0);   -- For wr ring buffer
      rdBaseAddr   : slv(63 downto 0);   -- For rd ring buffer
      maxSize      : slv(31 downto 0);
      contEn       : sl;
      dropEn       : sl;
      enable       : sl;
      forceInt     : sl;
      intEnable    : sl;
      online       : slv(CHAN_COUNT_G-1 downto 0);
      acknowledge  : slv(CHAN_COUNT_G-1 downto 0);
      fifoReset    : sl;
      intSwAckReq  : sl;
      intAckCount  : slv(31 downto 0);
      descWrCache  : slv(3 downto 0);
      buffRdCache  : slv(3 downto 0);
      buffWrCache  : slv(3 downto 0);

      -- FIFOs
      fifoDin        : slv(31 downto 0);
      wrFifoWr       : slv(WR_FIFO_CNT_C-1 downto 0);
      rdFifoWr       : slv(RD_FIFO_CNT_C-1 downto 0);
      addrFifoSel    : sl;
      wrFifoRd       : sl;
      wrFifoValidDly : slv(1 downto 0);
      wrAddr         : slv(31 downto 0);
      wrAddrValid    : sl;
      rdFifoRd       : sl;
      rdFifoValidDly : slv(1 downto 0);
      rdAddr         : slv(31 downto 0);
      rdAddrValid    : sl;

      -- Write Desc Request
      wrReqValid  : sl;
      wrReqCnt    : natural range 0 to CHAN_COUNT_G-1;
      wrReqNum    : slv(CHAN_SIZE_C-1 downto 0);
      wrReqAcks   : slv(CHAN_COUNT_G-1 downto 0);
      wrReqMissed : slv(31 downto 0);

      -- Desc Return
      descRetList : slv(RET_COUNT_C-1 downto 0);
      descState   : DescStateType;
      descRetCnt  : natural range 0 to RET_COUNT_C-1;
      descRetNum  : slv(RET_SIZE_C-1 downto 0);
      descRetAcks : slv(RET_COUNT_C-1 downto 0);
      wrIndex     : slv(DESC_AWIDTH_G-1 downto 0);
      wrMemAddr   : slv(63 downto 0);
      rdIndex     : slv(DESC_AWIDTH_G-1 downto 0);
      rdMemAddr   : slv(63 downto 0);
      intReqEn    : sl;
      intReqCount : slv(31 downto 0);
      interrupt   : sl;

   end record RegType;

   constant REG_INIT_C : RegType := (
      dmaWrDescAck    => (others => AXI_WRITE_DMA_DESC_ACK_INIT_C),
      dmaWrDescRetAck => (others => '0'),
      dmaRdDescReq    => (others => AXI_READ_DMA_DESC_REQ_INIT_C),
      dmaRdDescRetAck => (others => '0'),
      axilReadSlave   => AXI_LITE_READ_SLAVE_INIT_C,
      axilWriteSlave  => AXI_LITE_WRITE_SLAVE_INIT_C,
      axiWriteMaster  => axiWriteMasterInit(AXI_CONFIG_G, '1', "01", "0000"),
      wrBaseAddr      => (others => '0'),
      rdBaseAddr      => (others => '0'),
      maxSize         => (others => '0'),
      contEn          => '0',
      dropEn          => '0',
      enable          => '0',
      forceInt        => '0',
      intEnable       => '0',
      online          => (others => '0'),
      acknowledge     => (others => '0'),
      fifoReset       => '1',
      intSwAckReq     => '0',
      intAckCount     => (others => '0'),
      descWrCache     => (others => '0'),
      buffRdCache     => (others => '0'),
      buffWrCache     => (others => '0'),
      fifoDin         => (others => '0'),
      wrFifoWr        => (others => '0'),
      rdFifoWr        => (others => '0'),
      addrFifoSel     => '0',
      wrFifoRd        => '0',
      wrFifoValidDly  => (others => '0'),
      wrAddr          => (others => '0'),
      wrAddrValid     => '0',
      rdFifoRd        => '0',
      rdFifoValidDly  => (others => '0'),
      rdAddr          => (others => '0'),
      rdAddrValid     => '0',
      wrReqValid      => '0',
      wrReqCnt        => 0,
      wrReqNum        => (others => '0'),
      wrReqAcks       => (others => '0'),
      wrReqMissed     => (others => '0'),
      descRetList     => (others => '0'),
      descState       => IDLE_S,
      descRetCnt      => 0,
      descRetNum      => (others => '0'),
      descRetAcks     => (others => '0'),
      wrIndex         => (others => '0'),
      wrMemAddr       => (others => '0'),
      rdIndex         => (others => '0'),
      rdMemAddr       => (others => '0'),
      intReqEn        => '0',
      intReqCount     => (others => '0'),
      interrupt       => '0'
      );

   signal r            : RegType := REG_INIT_C;
   signal rin          : RegType;
   signal pause        : sl;
   signal rdFifoValid  : slv(RD_FIFO_CNT_C-1 downto 0);
   signal rdFifoDout   : slv(RD_FIFO_BITS_C-1 downto 0);
   signal wrFifoValid  : slv(WR_FIFO_CNT_C-1 downto 0);
   signal wrFifoDout   : slv(WR_FIFO_BITS_C-1 downto 0);
   signal addrRamDout  : slv(31 downto 0);
   signal addrRamAddr  : slv(DESC_AWIDTH_G-1 downto 0);
   signal intSwAckEn   : sl;
   signal intCompValid : sl;
   signal intDiffValid : sl;
   signal invalidCount : sl;
   signal diffCnt      : slv(31 downto 0);

   signal intReadMasters  : AxiLiteReadMasterArray(CB_COUNT_C-1 downto 0);
   signal intReadSlaves   : AxiLiteReadSlaveArray(CB_COUNT_C-1 downto 0);
   signal intWriteMasters : AxiLiteWriteMasterArray(CB_COUNT_C-1 downto 0);
   signal intWriteSlaves  : AxiLiteWriteSlaveArray(CB_COUNT_C-1 downto 0);

   -- attribute dont_touch                 : string;
   -- attribute dont_touch of r            : signal is "true";
   -- attribute dont_touch of intSwAckEn   : signal is "true";
   -- attribute dont_touch of invalidCount : signal is "true";
   -- attribute dont_touch of diffCnt      : signal is "true";

begin

   -----------------------------------------
   -- Crossbar
   -----------------------------------------
   U_CbEn: if DESC_128_EN_C = False generate
      U_AxiCrossbar : entity work.AxiLiteCrossbar
         generic map (
            TPD_G              => TPD_G,
            NUM_SLAVE_SLOTS_G  => 1,
            NUM_MASTER_SLOTS_G => CB_COUNT_C,
            DEC_ERROR_RESP_G   => AXI_RESP_OK_C,
            MASTERS_CONFIG_G   => AXI_CROSSBAR_MASTERS_CONFIG_C)
         port map (
            axiClk              => axiClk,
            axiClkRst           => axiRst,
            sAxiWriteMasters(0) => axilWriteMaster,
            sAxiWriteSlaves(0)  => axilWriteSlave,
            sAxiReadMasters(0)  => axilReadMaster,
            sAxiReadSlaves(0)   => axilReadSlave,
            mAxiWriteMasters    => intWriteMasters,
            mAxiWriteSlaves     => intWriteSlaves,
            mAxiReadMasters     => intReadMasters,
            mAxiReadSlaves      => intReadSlaves);
   end generate;

   U_CbDis: if DESC_128_EN_C = True generate
      intWriteMasters(0) <= axilWriteMaster;
      axilWriteSlave     <= intWriteSlaves(0);
      intReadMasters(0)  <= axilReadMaster;
      axilReadSlave      <= intReadSlaves(0);

      intWriteMasters(1) <= AXI_LITE_WRITE_MASTER_INIT_C;
      intReadMasters(1)  <= AXI_LITE_READ_MASTER_INIT_C;
   end generate;

   -----------------------------------------
   -- Write Free List FIFOs
   -----------------------------------------
   U_DescGen: for i in 0 to WR_FIFO_CNT_C-1 generate
      U_DescFifo : entity work.Fifo
         generic map (
            TPD_G           => TPD_G,
            SYNTH_MODE_G    => SYNTH_MODE_G,
            MEMORY_TYPE_G   => MEMORY_TYPE_G,            
            GEN_SYNC_FIFO_G => true,
            FWFT_EN_G       => true,
            DATA_WIDTH_G    => 32,
            ADDR_WIDTH_G    => DESC_AWIDTH_G)
         port map (
            rst    => r.fifoReset,
            wr_clk => axiClk,
            wr_en  => r.wrFifoWr(i),
            din    => r.fifoDin,
            rd_clk => axiClk,
            rd_en  => r.wrFifoRd,
            dout   => wrFifoDout((i*32)+31 downto i*32),
            valid  => wrFifoValid(i));
   end generate;

   -----------------------------------------
   -- Read Transaction FIFOs
   -----------------------------------------
   U_RdFifoGen: for i in 0 to RD_FIFO_CNT_C-1 generate
      U_RdFifo : entity work.Fifo
         generic map (
            TPD_G           => TPD_G,
            SYNTH_MODE_G    => SYNTH_MODE_G,
            MEMORY_TYPE_G   => MEMORY_TYPE_G,   
            GEN_SYNC_FIFO_G => true,
            FWFT_EN_G       => true,
            DATA_WIDTH_G    => 32,
            ADDR_WIDTH_G    => DESC_AWIDTH_G)
         port map (
            rst    => r.fifoReset,
            wr_clk => axiClk,
            wr_en  => r.rdFifoWr(i),
            din    => r.fifoDin,
            rd_clk => axiClk,
            rd_en  => r.rdFifoRd,
            dout   => rdFifoDout((i*32)+31 downto i*32),
            valid  => rdFifoValid(i));
   end generate;

   -----------------------------------------
   -- Address RAM, only used for width=64
   -----------------------------------------
   U_AddrEnGen: if DESC_128_EN_C = false generate
      U_AddrRam : entity work.AxiDualPortRam
         generic map (
            TPD_G        => TPD_G,
            COMMON_CLK_G => true,
            ADDR_WIDTH_G => DESC_AWIDTH_G,
            DATA_WIDTH_G => 32)
         port map (
            axiClk         => axiClk,
            axiRst         => axiRst,
            axiReadMaster  => intReadMasters(ADDR_INDEX_C),
            axiReadSlave   => intReadSlaves(ADDR_INDEX_C),
            axiWriteMaster => intWriteMasters(ADDR_INDEX_C),
            axiWriteSlave  => intWriteSlaves(ADDR_INDEX_C),
            clk            => axiClk,
            rst            => axiRst,
            addr           => addrRamAddr,
            dout           => addrRamDout);
   end generate;

   U_AddrDisGen: if DESC_128_EN_C = true generate
      addrRamDout <= (others=>'0');
      intWriteSlaves(ADDR_INDEX_C) <= AXI_LITE_WRITE_SLAVE_INIT_C;
      intReadSlaves(ADDR_INDEX_C)  <= AXI_LITE_READ_SLAVE_INIT_C;
   end generate;

   addrRamAddr <= wrFifoDout(DESC_AWIDTH_G-1 downto 0) when r.addrFifoSel = '0' else
                  rdFifoDout(DESC_AWIDTH_G+3 downto 4);

   -----------------------------------------
   -- Interrupt ACK Counter
   -----------------------------------------

   -- Check for invalid count
   U_DspComparator : entity work.DspComparator
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => 32)
      port map (
         clk     => axiClk,
         ibValid => r.intSwAckReq,
         ain     => r.intReqCount,
         bin     => r.intAckCount,
         obValid => intCompValid,
         ls      => invalidCount);  --  (a <  b) <--> r.intAckCount > r.intReqCount

   U_DspSub : entity work.DspAddSub
      generic map (
         TPD_G   => TPD_G,
         WIDTH_G => 32)
      port map (
         clk     => axiClk,
         ibValid => r.intSwAckReq,
         ain     => r.intReqCount,
         bin     => r.intAckCount,
         add     => '0',            -- '0' = subtract
         obValid => intDiffValid,   -- sync'd up with U_DspComparator
         pOut    => diffCnt);       -- a - b <--> r.intReqCount - r.intAckCount

   -- Both DSPs are done
   intSwAckEn <= intDiffValid and intCompValid;

   -----------------------------------------
   -- Control Logic
   -----------------------------------------

   -- Choose pause source
   pause <= '0' when (AXI_READY_EN_G) else axiWriteCtrl.pause;

   comb : process (addrRamDout, axiRst, axiWriteSlave, diffCnt, dmaRdDescAck,
                   dmaRdDescRet, dmaWrDescReq, dmaWrDescRet, intSwAckEn,
                   intReadMasters, intWriteMasters, invalidCount, pause, r,
                   rdFifoDout, rdFifoValid, wrFifoDout, wrFifoValid) is

      variable v            : RegType;
      variable wrReqList    : slv(CHAN_COUNT_G-1 downto 0);
      variable descRetValid : sl;
      variable descIndex    : natural;
      variable dmaRdReq     : AxiReadDmaDescReqType;
      variable rdIndex      : natural;
      variable regCon       : AxiLiteEndPointType;
   begin

      -- Latch the current value
      v := r;

      -- Clear one shot signals
      v.rdFifoWr    := (others=>'0');
      v.rdFifoRd    := '0';
      v.wrFifoWr    := (others=>'0');
      v.wrFifoRd    := '0';
      v.acknowledge := (others => '0');

      -----------------------------
      -- Register access
      -----------------------------

      -- Start transaction block
      axiSlaveWaitTxn(regCon, intWriteMasters(LOC_INDEX_C), intReadMasters(LOC_INDEX_C), v.axilWriteSlave, v.axilReadSlave);

      axiSlaveRegister(regCon, x"000", 0, v.enable);
      axiSlaveRegisterR(regCon, x"000", 16, toSl(DESC_128_EN_C));
      axiSlaveRegisterR(regCon, x"000", 24, toSlv(2, 8));  -- Version 2 = 2, Version1 = 0
      axiSlaveRegister(regCon, x"004", 0, v.intEnable);
      axiSlaveRegister(regCon, x"008", 0, v.contEn);
      axiSlaveRegister(regCon, x"00C", 0, v.dropEn);
      axiSlaveRegister(regCon, x"010", 0, v.wrBaseAddr(31 downto 0));
      axiSlaveRegister(regCon, x"014", 0, v.wrBaseAddr(63 downto 32));
      axiSlaveRegister(regCon, x"018", 0, v.rdBaseAddr(31 downto 0));
      axiSlaveRegister(regCon, x"01C", 0, v.rdBaseAddr(63 downto 32));
      axiSlaveRegister(regCon, x"020", 0, v.fifoReset);
      axiSlaveRegister(regCon, x"028", 0, v.maxSize);
      axiSlaveRegister(regCon, x"02C", 0, v.online);
      axiSlaveRegister(regCon, x"030", 0, v.acknowledge);

      axiSlaveRegisterR(regCon, x"034", 0, toSlv(CHAN_COUNT_G, 8));
      axiSlaveRegisterR(regCon, x"034", 8, toSlv(AXI_CONFIG_G.ADDR_WIDTH_C, 8));
      axiSlaveRegisterR(regCon, x"034", 16,toSlv(AXI_CONFIG_G.DATA_BYTES_C, 8));
      axiSlaveRegisterR(regCon, x"038", 0, toSlv(DESC_AWIDTH_G, 8));
      axiSlaveRegister(regCon, x"03C",  0, v.descWrCache);
      axiSlaveRegister(regCon, x"03C",  8, v.buffWrCache);
      axiSlaveRegister(regCon, x"03C", 12, v.buffRdCache);

      axiSlaveRegister(regCon, x"040", 0, v.fifoDin);
      axiWrDetect(regCon, x"040", v.rdFifoWr(0));

      axiSlaveRegister(regCon, x"044", 0, v.fifoDin);
      axiWrDetect(regCon, x"044", v.rdFifoWr(1));

      axiSlaveRegister(regCon, x"048", 0, v.fifoDin);
      axiWrDetect(regCon, x"048", v.wrFifoWr(0));

      axiSlaveRegister(regCon, x"04C", 0, v.intAckCount(15 downto 0));
      axiSlaveRegister(regCon, x"04C", 17, v.intEnable);
      axiWrDetect(regCon, x"04C", v.intSwAckReq);

      axiSlaveRegisterR(regCon, x"050", 0, r.intReqCount);
      axiSlaveRegisterR(regCon, x"054", 0, r.wrIndex);
      axiSlaveRegisterR(regCon, x"058", 0, r.rdIndex);

      axiSlaveRegisterR(regCon, x"05C", 0, r.wrReqMissed);

      if DESC_128_EN_C then
         axiSlaveRegister(regCon, x"060", 0, v.fifoDin);
         axiWrDetect(regCon, x"060", v.rdFifoWr(2));

         axiSlaveRegister(regCon, x"064", 0, v.fifoDin);
         axiWrDetect(regCon, x"064", v.rdFifoWr(3));
      
         axiSlaveRegister(regCon, x"070", 0, v.fifoDin);
         axiWrDetect(regCon, x"070", v.wrFifoWr(1));
      end if;

      axiSlaveRegister(regCon, x"080", 0, v.forceInt);

      -- End transaction block
      axiSlaveDefault(regCon, v.axilWriteSlave, v.axilReadSlave, AXI_RESP_DECERR_C);

      --------------------------------------
      -- Address FIFO Control
      --------------------------------------
      -- Alternate between read and write FIFOs to common address pool
      v.addrFifoSel := not(r.addrFifoSel);

      -- Write pipeline
      if r.wrFifoRd = '1' then
         v.wrFifoValidDly := (others => '0');
         v.wrAddr         := (others => '1');
         v.wrAddrValid    := '0';
      else
         v.wrFifoValidDly := (uAnd(wrFifoValid) and (not r.addrFifoSel)) & r.wrFifoValidDly(1);
         if r.wrFifoValidDly(0) = '1' then
            v.wrAddr      := addrRamDout;
            v.wrAddrValid := '1';
         end if;
      end if;

      -- Read pipeline
      if r.rdFifoRd = '1' then
         v.rdFifoValidDly := (others => '0');
         v.rdAddr         := (others => '1');
         v.rdAddrValid    := '0';
      else
         v.rdFifoValidDly := (uAnd(rdFifoValid) and r.addrFifoSel) & r.rdFifoValidDly(1);
         if r.rdFifoValidDly(0) = '1' then
            v.rdAddr      := addrRamDout;
            v.rdAddrValid := '1';
         end if;
      end if;

      --------------------------------------
      -- Write Descriptor Requests
      --------------------------------------

      -- Clear acks
      for i in 0 to CHAN_COUNT_G-1 loop
         v.dmaWrDescAck(i).valid := '0';
      end loop;

      -- Arbitrate
      if r.wrReqValid = '0' then

         -- Format requests
         wrReqList := (others => '0');
         for i in 0 to CHAN_COUNT_G-1 loop
            wrReqList(i) := dmaWrDescReq(i).valid;
         end loop;

         -- Arbitrate between requesters
         if r.enable = '1' and r.wrFifoRd = '0' and r.wrAddrValid = '1' then
            if (DESC_ARB_G = true) then
               arbitrate(wrReqList, r.wrReqNum, v.wrReqNum, v.wrReqValid, v.wrReqAcks);
            else
               -- Check the counter
               if (r.wrReqCnt = (CHAN_COUNT_G-1)) then
                  -- Reset the counter
                  v.wrReqCnt := 0;
               else
                  -- Increment the counter
                  v.wrReqCnt := r.wrReqCnt + 1;
               end if;
               -- Check for valid 
               if (wrReqList(r.wrReqCnt) = '1') then
                  v.wrReqValid := '1';
                  v.wrReqNum   := toSlv(r.wrReqCnt, CHAN_SIZE_C);
               else
                  v.wrReqValid := '0';
               end if;
            end if;
         end if;

         if r.enable = '0' then
            v.wrReqMissed := (others => '0');
         elsif wrReqList /= 0 and uAnd(wrFifoValid) = '0' then
            v.wrReqMissed := r.wrReqMissed + 1;
         end if;

      -- Valid arbitration result
      else
         for i in 0 to CHAN_COUNT_G-1 loop

            if DESC_128_EN_C then
               v.dmaWrDescAck(i).address(63 downto 40) := (others=>'0');
               v.dmaWrDescAck(i).address(39 downto  4) := wrFifoDout(63 downto 28);
               v.dmaWrDescAck(i).address(3  downto  0) := (others=>'0');
            else
               v.dmaWrDescAck(i).address(63 downto 32) := (others=>'0');
               v.dmaWrDescAck(i).address(31 downto  0) := r.wrAddr;
            end if;

            v.dmaWrDescAck(i).dropEn  := r.dropEn;
            v.dmaWrDescAck(i).contEn  := r.contEn;
            v.dmaWrDescAck(i).maxSize := r.maxSize;

            v.dmaWrDescAck(i).buffId(27 downto 0) := wrFifoDout(27 downto 0);

         end loop;

         v.dmaWrDescAck(conv_integer(r.wrReqNum)).valid := '1';
         v.wrFifoRd                                     := '1';
         v.wrReqValid                                   := '0';

      end if;

      --------------------------------------
      -- Read/Write Descriptor Returns
      --------------------------------------

      -- Clear acks
      v.dmaWrDescRetAck := (others => '0');
      v.dmaRdDescRetAck := (others => '0');

      -- Axi Cache
      v.axiWriteMaster.awcache := r.descWrCache;

      -- Reset strobing Signals
      if (axiWriteSlave.awready = '1') or (AXI_READY_EN_G = false) then
         v.axiWriteMaster.awvalid := '0';
      end if;
      if (axiWriteSlave.wready = '1') or (AXI_READY_EN_G = false) then
         v.axiWriteMaster.wvalid := '0';
         v.axiWriteMaster.wlast  := '0';
      end if;

      -- Generate descriptor ring addresses
      if DESC_128_EN_C then
         v.wrMemAddr := r.wrBaseAddr + (r.wrIndex & "0000");
         v.rdMemAddr := r.rdBaseAddr + (r.rdIndex & "0000");
      else
         v.wrMemAddr := r.wrBaseAddr + (r.wrIndex & "000");
         v.rdMemAddr := r.rdBaseAddr + (r.rdIndex & "000");
      end if;

      -- State machine
      case r.descState is
         ----------------------------------------------------------------------
         when IDLE_S =>

            -- Format requests
            v.descRetList := (others => '0');
            for i in 0 to CHAN_COUNT_G-1 loop
               v.descRetList(i*2)   := dmaWrDescRet(i).valid;
               v.descRetList(i*2+1) := dmaRdDescRet(i).valid;
            end loop;

            -- Arbitrate between requesters
            if r.enable = '1' and pause = '0' then
               if (DESC_ARB_G = true) then
                  arbitrate(v.descRetList, r.descRetNum, v.descRetNum, descRetValid, v.descRetAcks);
               else
                  -- Check the counter
                  if (r.descRetCnt = (RET_COUNT_C-1)) then
                     -- Reset the counter
                     v.descRetCnt := 0;
                  else
                     -- Increment the counter
                     v.descRetCnt := r.descRetCnt + 1;
                  end if;
                  -- Check for valid 
                  if (v.descRetList(r.descRetCnt) = '1') then
                     descRetValid := '1';
                     v.descRetNum := toSlv(r.descRetCnt, RET_SIZE_C);
                  else
                     descRetValid := '0';
                  end if;
               end if;

               -- Valid request
               if descRetValid = '1' then
                  if v.descRetNum(0) = '1' then
                     v.descState := READ_S;
                  else
                     v.descState := WRITE_S;
                  end if;
               end if;
            end if;

         ----------------------------------------------------------------------
         when WRITE_S =>
            if CHAN_COUNT_G > 1 then
               descIndex := conv_integer(r.descRetNum(RET_SIZE_C-1 downto 1));
            else
               descIndex := 0;
            end if;

            -- Write address channel
            v.axiWriteMaster.awaddr := r.wrMemAddr;
            v.axiWriteMaster.awlen  := x"00";  -- Single transaction

            -- Write data channel
            v.axiWriteMaster.wlast := '1';

            -- Descriptor data, 128-bits
            if DESC_128_EN_C then
               v.axiWriteMaster.wdata(127)            := '1';
               v.axiWriteMaster.wdata(126 downto 108) := (others=>'0');
               v.axiWriteMaster.wdata(107 downto 104) := toSlv(descIndex,4); -- Channel
               v.axiWriteMaster.wdata(103 downto  96) := dmaWrDescRet(descIndex).dest;
               v.axiWriteMaster.wdata(95  downto  64) := dmaWrDescRet(descIndex).size;
               v.axiWriteMaster.wdata(63  downto  32) := dmaWrDescRet(descIndex).buffId;
               v.axiWriteMaster.wdata(31  downto  24) := dmaWrDescRet(descIndex).firstUser;
               v.axiWriteMaster.wdata(23  downto  16) := dmaWrDescRet(descIndex).lastUser;
               v.axiWriteMaster.wdata(15  downto   4) := (others=>'0');
               v.axiWriteMaster.wdata(3)              := dmaWrDescRet(descIndex).continue;
               v.axiWriteMaster.wdata(2   downto   0) := dmaWrDescRet(descIndex).result;

               v.axiWriteMaster.wstrb := resize(x"FFFF", 128);

            -- Descriptor data, 64-bits
            else
               v.axiWriteMaster.wdata(63 downto 56) := dmaWrDescRet(descIndex).dest;
               v.axiWriteMaster.wdata(55 downto 32) := dmaWrDescRet(descIndex).size(23 downto 0);
               v.axiWriteMaster.wdata(31 downto 24) := dmaWrDescRet(descIndex).firstUser;
               v.axiWriteMaster.wdata(23 downto 16) := dmaWrDescRet(descIndex).lastUser;
               v.axiWriteMaster.wdata(15 downto 4)  := dmaWrDescRet(descIndex).buffId(11 downto 0);
               v.axiWriteMaster.wdata(3)            := dmaWrDescRet(descIndex).continue;
               v.axiWriteMaster.wdata(2 downto 0)   := dmaWrDescRet(descIndex).result;

               v.axiWriteMaster.wstrb := resize(x"FF", 128);

               -- Encoded channel into upper destination bits
               if CHAN_COUNT_G > 1 then
                  v.axiWriteMaster.wdata(63 downto 64-CHAN_SIZE_C) := toSlv(descIndex, CHAN_SIZE_C);
               end if;
            end if;

            v.axiWriteMaster.awvalid := '1';
            v.axiWriteMaster.wvalid  := '1';
            v.wrIndex                := r.wrIndex + 1;
            v.descState              := WAIT_S;

            v.dmaWrDescRetAck(descIndex) := '1';

         ----------------------------------------------------------------------
         when READ_S =>
            if CHAN_COUNT_G > 1 then
               descIndex := conv_integer(r.descRetNum(RET_SIZE_C-1 downto 1));
            else
               descIndex := 0;
            end if;

            -- Write address channel
            v.axiWriteMaster.awaddr := r.rdMemAddr;
            v.axiWriteMaster.awlen  := x"00";  -- Single transaction

            -- Write data channel
            v.axiWriteMaster.wlast := '1';

            -- Descriptor data, 128-bits
            if DESC_128_EN_C then
               v.axiWriteMaster.wdata(127)           := '1';
               v.axiWriteMaster.wdata(126 downto 64) := (others => '0');
               v.axiWriteMaster.wdata(63  downto 32) := dmaRdDescRet(descIndex).buffId;
               v.axiWriteMaster.wdata(31  downto  3) := (others => '0');
               v.axiWriteMaster.wdata(2   downto  0) := dmaRdDescRet(descIndex).result;

               v.axiWriteMaster.wstrb := resize(x"FFFF", 128);

            -- Descriptor data, 64-bits
            else
               v.axiWriteMaster.wdata(63 downto 32) := x"00000001";
               v.axiWriteMaster.wdata(31 downto 16) := (others => '0');
               v.axiWriteMaster.wdata(15 downto 4)  := dmaRdDescRet(descIndex).buffId(11 downto 0);
               v.axiWriteMaster.wdata(3)            := '0';
               v.axiWriteMaster.wdata(2 downto 0)   := dmaRdDescRet(descIndex).result;

               v.axiWriteMaster.wstrb := resize(x"FF", 128);

            end if;

            v.axiWriteMaster.awvalid := '1';
            v.axiWriteMaster.wvalid  := '1';
            v.rdIndex                := r.rdIndex + 1;
            v.descState              := WAIT_S;

            v.dmaRdDescRetAck(descIndex) := '1';

         ----------------------------------------------------------------------
         when WAIT_S =>
            if v.axiWriteMaster.awvalid = '0' and v.axiWriteMaster.wvalid = '0' and
               (axiWriteSlave.bvalid = '1' or ACK_WAIT_BVALID_G = false) then
               v.intReqEn  := '1';
               v.descState := IDLE_S;
            end if;

         when others =>
            v.descState := IDLE_S;

      end case;
      
      -- Copy the lowest words to the entire bus (refer to  "section 9.3 Narrow transfers" of the AMBA spec)
      if DESC_128_EN_C then
         for i in 7 downto 1 loop
            v.axiWriteMaster.wdata((128*i)+127 downto (128*i)) := v.axiWriteMaster.wdata(127 downto 0);
         end loop;      
      else
         for i in 15 downto 1 loop
            v.axiWriteMaster.wdata((64*i)+63 downto (64*i)) := v.axiWriteMaster.wdata(63 downto 0);
         end loop;      
      end if;

      -- Drive interrupt, avoid false firings during ack
      if (r.intReqCount /= 0 or r.forceInt = '1') and r.intSwAckReq = '0' then
         v.interrupt := r.intEnable;
      else
         v.interrupt := '0';
      end if;

      -- Ack request from software
      if r.intSwAckReq = '1' then
         v.forceInt := '0';

         -- DSPs are done
         if intSwAckEn = '1' then
            v.intSwAckReq := '0';

            -- Just in case
            if invalidCount = '1' then     -- r.intAckCount > r.intReqCount
               v.intReqCount := (others => '0');
            else
               v.intReqCount := diffCnt;   -- r.intReqCount - r.intAckCount
            end if;
         end if;

      -- Firmware posted an entry
      elsif r.intReqEn = '1' then
         v.intReqCount := r.intReqCount + 1;
         v.intReqEn    := '0';
      end if;

      -- Engine disabled
      if r.enable = '0' then
         v.intReqEn    := '0';
         v.intReqCount := (others => '0');
         v.interrupt   := '0';
         v.forceInt    := '0';
      end if;

      --------------------------------------
      -- Read Descriptor Requests
      --------------------------------------

      -- Clear requests
      for i in 0 to CHAN_COUNT_G-1 loop
         if dmaRdDescAck(i) = '1' then
            v.dmaRdDescReq(i).valid := '0';
         end if;
      end loop;

      dmaRdReq       := AXI_READ_DMA_DESC_REQ_INIT_C;
      dmaRdReq.valid := r.rdAddrValid;

      -- Format request, 128-bits
      if DESC_128_EN_C then
         dmaRdReq.address(63 downto 40) := (others=>'0');
         dmaRdReq.address(39 downto  4) := rdFifoDout(127 downto 92);
         dmaRdReq.address(3  downto  0) := (others=>'0');
         dmaRdReq.buffId(27 downto 0)   := rdFifoDout(91 downto 64);
         dmaRdReq.size                  := rdFifoDout(63 downto 32);
         dmaRdReq.firstUser             := rdFifoDout(31 downto 24);
         dmaRdReq.lastUser              := rdFifoDout(23 downto 16);
         dmaRdReq.dest                  := rdFifoDout(15 downto  8);
         dmaRdReq.continue              := rdFifoDout(3);

         rdIndex := conv_integer(rdFifoDout(7 downto 4));

      -- Format request, 64-bits
      else 
         dmaRdReq.address(63 downto 32) := (others=>'0');
         dmaRdReq.address(31 downto  0) := r.rdAddr;
         dmaRdReq.dest                  := rdFifoDout(63 downto 56);
         dmaRdReq.size(23 downto 0)     := rdFifoDout(55 downto 32);
         dmaRdReq.firstUser             := rdFifoDout(31 downto 24);
         dmaRdReq.lastUser              := rdFifoDout(23 downto 16);
         dmaRdReq.buffId(11 downto 0)   := rdFifoDout(15 downto 4);
         dmaRdReq.continue              := rdFifoDout(3);

         -- Upper dest bits select channel
         if CHAN_COUNT_G > 1 then
            rdIndex                               := conv_integer(dmaRdReq.dest(7 downto 8-CHAN_SIZE_C));
            dmaRdReq.dest(7 downto 8-CHAN_SIZE_C) := (others => '0');
         else
            rdIndex := 0;
         end if;
      end if;

      -- Pull next entry if we are not waiting for ack on given channel
      if r.rdFifoRd = '0' and dmaRdReq.valid = '1' and v.dmaRdDescReq(rdIndex).valid = '0' then
         v.dmaRdDescReq(rdIndex) := dmaRdReq;
         v.rdFifoRd              := '1';
      end if;

      --------------------------------------
      if r.enable = '0' then
         v.wrIndex := (others => '0');
         v.rdIndex := (others => '0');
      end if;

      -- Reset      
      if (axiRst = '1') then
         v := REG_INIT_C;
      end if;

      -- Register the variable for next clock cycle      
      rin <= v;

      -- Outputs   
      intReadSlaves(LOC_INDEX_C)  <= r.axilReadSlave;
      intWriteSlaves(LOC_INDEX_C) <= r.axilWriteSlave;

      online          <= r.online;
      interrupt       <= r.interrupt;
      acknowledge     <= r.acknowledge;
      dmaWrDescAck    <= r.dmaWrDescAck;
      dmaWrDescRetAck <= r.dmaWrDescRetAck;
      dmaRdDescReq    <= r.dmaRdDescReq;
      dmaRdDescRetAck <= r.dmaRdDescRetAck;
      axiWriteMaster  <= r.axiWriteMaster;
      axiRdCache      <= r.buffRdCache;
      axiWrCache      <= r.buffWrCache;

   end process comb;

   seq : process (axiClk) is
   begin
      if (rising_edge(axiClk)) then
         r <= rin after TPD_G;
      end if;
   end process seq;

end rtl;

