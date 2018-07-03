-------------------------------------------------------------------------------
-- File       : AxiStreamDmaV3Desc.vhd
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2018-06-29
-- Last update: 2018-07-02
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
      CHAN_COUNT        : integer range 1 to 16 := 1;                -- Channel count
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

   constant CROSSBAR_CONN_C : slv(15 downto 0) := x"FFFF"; -- 16 bit Crossbar connection

   constant CB_COUNT_C : integer := 2;                     -- Crossbar count

   constant LOC_INDEX_C       : natural            := 0;                                        -- Local index
   constant LOC_BASE_ADDR_C   : slv(31 downto 0)   := AXIL_BASE_ADDR_G(31 downto 16) & x"0000"; -- Local Base Address
   constant LOC_NUM_BITS_C    : natural            := 14;                                       -- Local number of bits

   constant ADDR_INDEX_C      : natural            := 0;
   constant ADDR_BASE_ADDR_C  : slv(31 downto 0)   := AXIL_BASE_ADDR_G(31 downto 16) & x"4000";
   constant ADDR_NUM_BITS_C   : natural            := 14;
   
   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(CB_COUNT_C-1 downto 0) := (
      LOC_INDEX_C       => (
         baseAddr       => LOC_BASE_ADDR_C,
         addrBits       => LOC_NUM_BITS_C,
         connectivity   => CROSSBAR_CONN_C),
      ADDR_INDEX_C      => (
         baseAddr       => ADDR_BASE_ADDR_C,
         addrBits       => ADDR_NUM_BITS_C,
         connectivity   => CROSSBAR_CONN_C));

   signal intReadMasters   : AxiLiteReadMasterArray(CB_COUNT_C-1 downto 0);
   signal intReadSlaves    : AxiLiteReadSlaveArray(CB_COUNT_C-1 downto 0);
   signal intWriteMasters  : AxiLiteWriteMasterArray(CB_COUNT_C-1 downto 0);
   signal intWriteSlaves   : AxiLiteWriteSlaveArray(CB_COUNT_C-1 downto 0);
