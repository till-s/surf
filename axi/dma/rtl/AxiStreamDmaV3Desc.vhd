-------------------------------------------------------------------------------
-- File       : AxiStreamDmaV3Desc.vhd
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2018-06-29
-- Last update: 2018-07-05
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

entity AxiStreamDmaV3Desc is
   generic (
      TPD_G             : time                  := 1 ns;             -- Propagation Delay
      CHAN_COUNT_G        : integer range 1 to 16 := 1;              -- Channel count
      AXIL_BASE_ADDR_G  : slv(31 downto 0)      := x"00000000";      -- Axi Lite Base Address
      AXI_READY_EN_G    : boolean               := false;            -- Axi ready signal
      AXI_CONFIG_G      : AxiConfigType         := AXI_CONFIG_INIT_C;
      DESC_AWIDTH_G     : integer range 4 to 12 := 12;               -- Descriptor Address width
      DESC_ARB_G        : boolean               := true;
      DESC_VERSION_G    : integer range 1 to 2  := 1;                -- If descriptor version is 1, descriptor size is 64 bits
                                                                     -- else 128 bits if the version is 2
      ACK_WAIT_BVALID_G : boolean               := true);            -- Wait ack valid
   port(
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
      online          : out slv (CHAN_COUNT_G-1 downto 0);
      acknowledge     : out slv (CHAN_COUNT_G-1 downto 0);
      -- DMA write descriptor request, ack and return
      dmaWrDescReq    : in  AxiWriteDmaDescReqArray(CHAN_COUNT_G-1 downto 0);
      dmaWrDescAck    : out AxiWriteDmaDescAckArray(CHAN_COUNT_G-1 downto 0);
      dmaWrDescRet    : in  AxiWriteDmaDescRetArray(CHAN_COUNT_G-1 downto 0);
      dmaWrDescRetAck : out slv(CHAN_COUNT_G-1 downto 0);
      -- DMA read descriptor request, ack and return
      dmaRdDescReq    : out AxiReadDmaDescReqArray
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
end AxiStreamDmaV3Desc;

architecture rtl of AxiStreamDmaV3Desc is

   constant CROSSBAR_CONN_C : slv(15 downto 0) := x"FFFF";                                      -- 16 bit Crossbar connection

   constant CB_COUNT_C : integer := 2;                                                          -- Crossbar count = 2

   constant LOC_INDEX_C       : natural            := 0;                                        -- Local index
   constant LOC_BASE_ADDR_C   : slv(31 downto 0)   := AXIL_BASE_ADDR_G(31 downto 16) & x"0000"; -- Local Base Address
   constant LOC_NUM_BITS_C    : natural            := 14;                                       -- Local number of bits

   constant ADDR_INDEX_C      : natural            := 0;                                        -- Address index
   constant ADDR_BASE_ADDR_C  : slv(31 downto 0)   := AXIL_BASE_ADDR_G(31 downto 16) & x"4000"; -- Base address
   constant ADDR_NUM_BITS_C   : natural            := 14;                                       -- Number of bits

   -- Crossbar Master configuration
   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(CB_COUNT_C-1 downto 0) := (
      LOC_INDEX_C       => (
         baseAddr       => LOC_BASE_ADDR_C,
         addrBits       => LOC_NUM_BITS_C,
         connectivity   => CROSSBAR_CONN_C),
      ADDR_INDEX_C      => (
         baseAddr       => ADDR_BASE_ADDR_C,
         addrBits       => ADDR_NUM_BITS_C,
         connectivity   => CROSSBAR_CONN_C));

   signal intReadMasters   : AxiLiteReadMasterArray(CB_COUNT_C-1 downto 0);   -- 2 bit Read master Signal
   signal intReadSlaves    : AxiLiteReadSlaveArray(CB_COUNT_C-1 downto 0);    -- 2 bit Read slave signal
   signal intWriteMasters  : AxiLiteWriteMasterArray(CB_COUNT_C-1 downto 0);  -- 2 bit Write master signal
   signal intWriteSlaves   : AxiLiteWriteSlaveArray(CB_COUNT_C-1 downto 0);   -- 2 bit Write slave signal

   type DescStateType is (IDLE_S, WRITE_S, READ_S, WAIT_S); -- Descriptor state type

   constant CHAN_SIZE_C    : integer := bitSize(CHAN_COUNT_G-1);  -- Descriptor channel size = 0
   constant DESC_COUNT_C   : integer := CHAN_COUNT_G*2;           -- Descriptor count = 1*2 = 2
   constant DESC_SIZE_C    : integer := bitSize(DESC_COUNT_C-1);  -- Descriptor Size  = bitSize(2-1) = bitSize(1) = 1

   -- Register Type
   type RegType is record

      -- Write descriptor interface
      dmaWrDescAck    : AxiWriteDmaDescAckArray(CHAN_COUNT_G-1 downto 0); -- DMA write descriptor ack (1 bit)
      dmaWrDescRetAck : slv(CHAN_COUNT_G-1 downto 0);                     -- DMA write descriptor return ack (1 bit)

      -- Read descriptor interface
      dmaRdDescReq    : AxiReadDmaDescReqArray(CHAN_COUNT_G-1 downto 0);  -- DMA read descriptor request (1 bit)
      dmaRdDescRetAck : slv(CHAN_COUNT_G-1 downto 0);                     -- DMA read descriptor return ack (1 bit)

      -- Axi-Lite
      axilReadSlave  : AxiLiteReadSlaveType;  -- Axi-Lite Read Slave
      axilWriteSlave : AxiLiteWriteSlaveType; -- Axi-Lite Write Slave

      -- AXI
      axiWriteMaster : AxiWriteMasterType;    -- Axi Write Master

      -- Configuration
      buffBaseAddr : slv(63 downto 32);            -- For buffer entries - 32 bit buffer base address
      wrBaseAddr   : slv(63 downto 0);             -- For write ring buffer - 64 bit write base address
      rdBaseAddr   : slv(63 downto 0);             -- For read ring buffer - 64 bit read base address
      maxSize      : slv(23 downto 0);             -- 24 bit Max buffer size
      contEn       : sl;
      dropEn       : sl;
      enable       : sl;
      intEnable    : sl;
      online       : slv(CHAN_COUNT_G-1 downto 0); -- buffer online (1 bit)
      acknowledge  : slv(CHAN_COUNT_G-1 downto 0); -- buffer acknowledge (1 bit)
      fifoReset    : sl;                           -- FIFO reset
      intSwAckReq  : sl;                           --
      intAckCount  : slv(31 downto 0);             -- Ack count (32 bits)
      descWrCache  : slv(3 downto 0);              -- Descriptor write cache (4 bits)
      buffRdCache  : slv(3 downto 0);              -- Buffer read cache (4 bits)
      buffWrCache  : slv(3 downto 0);              -- Buffer write cache (4 bits)

      -- FIFOs
      fifoDin        : slv(31 downto 0); -- 32 bit input FIFO
      wrFifoWr       : sl;               -- write-write FIFO
      rdFifoWr       : slv(1 downto 0);  -- read-write FIFO
      addrFifoSel    : sl;               -- Fifo address select?
      wrFifoRd       : sl;               -- write-read FIFO
      wrFifoValiDly  : slv(1 downto 0);  -- Write FIFO valid
      wrAddr         : slv(31 downto 0); -- Write address (32 bits)
      wrAddrValid    : sl;               -- Write address valid
      rdFifoRd       : sl;               -- Read-read FIFO
      rdFifoValiDly  : slv(1 downto 0);  -- Read FIFO valid
      rdAddr         : slv(31 downto 0); -- Read Address
      rdAddrValid    : sl;               -- Read address valid

      -- Write Desc Request
      wrReqValid  : sl;                                -- Write request valid
      wrReqCnt    : natural range 0 to CHAN_COUNT_G-1; -- Write request count
      wrReqNum    : slv(CHAN_SIZE_C-1 downto 0);       -- Write request number
      wrReqAcks   : slv(CHAN_COUNT_G-1 downto 0);      -- Write request acks
      wrReqMissed : slv(31 downto 0);                  -- Write Request missed

      -- Desc Return
      descRetList : slv(DESC_COUNT_C-1 downto 0);      -- Descriptor return list
      descState   : DescStateType;                     -- Descriptor state
      descRetCnt  : natural range 0 to DESC_COUNT_C-1; -- Descriptor return count
      descRetNum  : slv(DESC_SIZE_C-1 downto 0);       -- Descriptor return number
      descRetAcks : slv(DESC_COUNT_C-1 downto 0);      -- Descriptor return acks
      wrIndex     : slv(DESC_AWIDTH_G-1 downto 0);     -- write index
      wrMemAddr   : slv(63 downto 0);                  -- write memory address
      rdIndex     : slv(DESC_AWIDTH_G-1 downto 0);     -- read index
      rdMemAddr   : slv(63 downto 0);                  -- read memory address
      intReqEn    : sl;                                -- Request enable?
      intReqCount : slv(31 downto 0);                  -- Request count
      interrupt   : sl;

   end record RegType;
