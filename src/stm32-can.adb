with STM32.Device;  use STM32.Device;
with System;        use System;
with LCD_Std_Out;   use LCD_Std_Out;
with STM32_SVD.RCC; use STM32_SVD.RCC;
with STM32.GPIO;

package body STM32.CAN is

   type CAN_Timing is record
      BRP : Natural;  -- prescaler - 1
      TS1 : Natural;  -- BS1 - 1
      TS2 : Natural;  -- BS2 - 1
      SJW : Natural;  -- SJW - 1
   end record;

   function Get_Timing (Speed : CAN_Speed) return CAN_Timing is
      PCLK1_Hz : constant UInt32 :=
        STM32.Device.System_Clock_Frequencies.PCLK1;
   begin
      if PCLK1_Hz = 50_000_000 then
         case Speed is
            when CAN_1M =>
               return (BRP => 4, TS1 => 6, TS2 => 1, SJW => 1);

            when CAN_500K =>
               return (BRP => 4, TS1 => 15, TS2 => 2, SJW => 1);

            when CAN_250K =>
               return (BRP => 9, TS1 => 15, TS2 => 2, SJW => 1);

            when CAN_125K =>
               return (BRP => 19, TS1 => 15, TS2 => 2, SJW => 1);
         end case;
      end if;

      raise Constraint_Error with "Unsupported PCLK1 frequency";
   end Get_Timing;

   procedure Initialize
     (This : aliased in out CAN_Port'Class; Speed : CAN_Speed)
   is
      Bus_Timing : constant CAN_Timing := Get_Timing (Speed);
   begin
      Enable_Clock (This);
      Reset (This);
      Configure_IO (This);

      if This.Periph.all'Address = STM32_SVD.CAN1_Base then

         -- 36.4.3 :: RM0385
         -- If software requests entry to initialization mode by setting
         -- the INRQ bit while bxCAN is in Sleep mode, it must also clear the SLEEP bit.
         CAN1_Periph.MCR.SLEEP := False;

         -- 36.4.3 :: RM0385
         -- Sleep mode is exited once the SLAK bit has
         -- been cleared by hardware.
         loop
            exit when CAN1_Periph.MSR.SLAK = False;
         end loop;

         --  36.4.1 :: RM0385
         --  To enter initialization mode, the software sets the
         --  INRQ bit in the CAN_MCR register
         CAN1_Periph.MCR.INRQ := True;

         -- 36.4.1 :: RM0385
         -- The software waits until the hardware has confirmed
         -- the request by setting the INAK bit in the CAN_MSR register
         loop
            exit when CAN1_Periph.MSR.INAK = True;
         end loop;

         -- 36.4.1 :: RM0385
         -- To initialize the CAN Controller, software has to set up
         -- the CAN options (CAN_MCR) registers

         -- Disable time triggered communication mode
         CAN1_Periph.MCR.TTCM := False;

         -- Disable automatic bus-off management
         CAN1_Periph.MCR.ABOM := False;

         -- Disable automatic wakeup
         CAN1_Periph.MCR.AWUM := False;

         -- Allow retransmission
         CAN1_Periph.MCR.NART := False;

         -- Disable Rx FIFO lock. Old messages will be discarded
         CAN1_Periph.MCR.RFLM := False;

         -- Disable Tx FIFO Priority. Use ID priority
         CAN1_Periph.MCR.TXFP := False;

         -- 36.4.1
         -- To initialize the CAN Controller, software has to set up
         -- the Bit timing (CAN_BTR) registers
         CAN1_Periph.BTR.SJW := UInt2 (Bus_Timing.SJW);
         CAN1_Periph.BTR.TS1 := UInt4 (Bus_Timing.TS1);
         CAN1_Periph.BTR.TS2 := UInt3 (Bus_Timing.TS2);
         CAN1_Periph.BTR.BRP := UInt10 (Bus_Timing.BRP);

         -- Temp enabling loopback
         -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         -- !!!!!!!REMOVE ME LATER!!!!!!!
         -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         -- CAN1_Periph.BTR.LBKM := True;

         -- Enable FIFO 0 message pending Interrupt
         CAN1_Periph.IER.FMPIE0 := True;
         CAN1_Periph.IER.FMPIE1 := True;

         --  -- Assign 14 filters to CAN1 and enter filter init mode
         CAN1_Periph.FMR.CAN2SB := 14;

         --  36.4.1 :: RM0385
         -- To initialize the registers associated with the CAN filter banks (mode, scale, FIFO
         -- assignment, activation, and filter values), software has to set the FINIT bit (CAN_FMR).
         CAN1_Periph.FMR.FINIT := True;

         CAN1_Periph.FM1R.FBM.Val  := 16#000_0000#;
         CAN1_Periph.FS1R.FSC.Val  := 16#0000_0001#;
         CAN1_Periph.FFA1R.FFA.Val := 16#000_0000#;
         CAN1_Periph.FA1R.FACT.Val := 16#0000_0001#;

         CAN1_Periph.F0R1.Val := 16#0000_0000#;
         CAN1_Periph.F0R2.Val := 16#0000_0000#;

         -- 36.9.4 :: RM0385
         -- Set active filters mode
         CAN1_Periph.FMR.FINIT := False;

         -- 36.4.1 :: RM0385
         -- To leave Initialization mode, the software clears the INQR bit.
         CAN1_Periph.MCR.INRQ := False;

         -- 36.4.1 :: RM0385
         -- bxCAN has left Initialization mode once the INAK bit has been cleared by hardware.
         loop
            exit when CAN1_Periph.MSR.INAK = False;
         end loop;

         Put_Line ("Initialized CAN1 Peripheral");
      end if;
   end Initialize;

   procedure Configure_IO (This : aliased in out CAN_Port'Class) is
      CAN1_GPIO_Points : constant STM32.GPIO.GPIO_Points := (PB9, PB8);
   begin
      -- TODO: Add in Configure_IO for CAN 2!
      if This.Periph.all'Address = STM32_SVD.CAN1_Base then
         STM32.GPIO.Configure_IO
           (CAN1_GPIO_Points,
            (Mode => STM32.GPIO.Mode_AF, AF => STM32.Device.GPIO_AF_CAN1_9,
             AF_Speed       => STM32.GPIO.Speed_50MHz,
             AF_Output_Type => STM32.GPIO.Push_Pull,
             Resistors      => STM32.GPIO.Pull_Up));
      else
         raise Unknown_Device;
      end if;
      Put_Line ("Configured CAN I/O pins");
   end Configure_IO;

   procedure Enable_Clock (This : aliased in out CAN_Port'Class) is
   begin
      if This.Periph.all'Address = STM32_SVD.CAN1_Base then
         Put_Line ("Enabling CAN1 Clock");
         Enable_Clock (PB8);
         Enable_Clock (PB9);
         RCC_Periph.APB1ENR.CAN1EN := True;
      elsif This.Periph.all'Address = STM32_SVD.CAN2_Base then
         Put_Line ("Enabling CAN2 Clock");
         RCC_Periph.APB1ENR.CAN2EN := True;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   function To_Extended_ID
     (STID : HAL.UInt11; EXID : HAL.UInt18) return HAL.UInt29
   is
      U : constant Unsigned_32 :=
        Shift_Left (Unsigned_32 (STID), 18) or Unsigned_32 (EXID);
   begin
      -- mask to 29 bits, then cast back
      return HAL.UInt29 (U and 2#0001_1111_1111_1111_1111_1111_1111_1111#);
   end To_Extended_ID;

   procedure From_Extended_ID
     (ID : HAL.UInt29; STID : out HAL.UInt11; EXID : out HAL.UInt18)
   is
      U : constant Unsigned_32 := Unsigned_32 (ID);
   begin
      -- 11 bits
      STID := HAL.UInt11 (Shift_Right (U, 18) and 2#0111_1111_1111#);

      -- 18 bits
      EXID := HAL.UInt18 (U and 2#0011_1111_1111_1111_1111#);
   end From_Extended_ID;

   procedure Reset (This : aliased in out CAN_Port'Class) is
   begin
      if This.Periph.all'Address = STM32_SVD.CAN1_Base then
         Put_Line ("Resetting CAN1");
         RCC_Periph.APB1RSTR.CAN1RST := True;
         RCC_Periph.APB1RSTR.CAN1RST := False;
      elsif This.Periph.all'Address = STM32_SVD.CAN2_Base then
         Put_Line ("Resetting CAN2");
         RCC_Periph.APB1RSTR.CAN2RST := True;
         RCC_Periph.APB1RSTR.CAN2RST := False;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   protected body Receiver is
      procedure Interrupt_Handler is
         Frame  : CAN_Frame;
         Status : CAN_Status;
      begin
         Status := Receive (CAN_1, Frame);
         Put_Line
           ("ISR! " & Status'Image & " ID: " &
            (if Frame.ID_Type = Standard then Frame.Standard_ID'Image
             else Frame.Extended_ID'Image));
      end Interrupt_Handler;
   end Receiver;

   function Receive
     (This : aliased in out CAN_Port'Class; Frame : out CAN_Frame)
      return CAN_Status
   is
   begin
      if This.Periph.all'Address /= STM32_SVD.CAN1_Base then
         raise Unknown_Device;
      end if;

      if CAN1_Periph.RF0R.FMP0 /= 0 then
         if CAN1_Periph.RI0R.IDE then
            Frame :=
              (ID_Type     => STM32.CAN.Extended, RTR => CAN1_Periph.RI0R.RTR,
               DLC         => CAN1_Periph.RDT0R.DLC, Data => (others => 0),
               Extended_ID =>
                 To_Extended_ID
                   (CAN1_Periph.RI0R.STID, CAN1_Periph.RI0R.EXID));
         else
            Frame :=
              (ID_Type     => STM32.CAN.Standard, RTR => CAN1_Periph.RI0R.RTR,
               DLC         => CAN1_Periph.RDT0R.DLC, Data => (others => 0),
               Standard_ID => CAN1_Periph.RI0R.STID);
         end if;

         for I in 0 .. 3 loop
            Frame.Data (I + 1) := CAN1_Periph.RDL0R.Arr (I);
         end loop;

         for I in 4 .. 7 loop
            Frame.Data (I + 1) := CAN1_Periph.RDH0R.Arr (I);
         end loop;
      else
         return No_Message;
      end if;

      CAN1_Periph.RF0R.RFOM0 := True;

      if CAN1_Periph.RF0R.FMP0 /= 0 then
         return Ok;
      end if;

      return No_Message;
   end Receive;

   function Transmit
     (This : aliased in out CAN_Port'Class; Frame : CAN_Frame)
      return CAN_Status
   is
      --  C          : constant Time      := Clock;
      --  Timeout    : constant Time_Span := Milliseconds (1_000);
      TX_Mailbox : Natural;
   begin
      if This.Periph.all'Address /= STM32_SVD.CAN1_Base then
         raise Unknown_Device;
      end if;

      if CAN1_Periph.TSR.TME.Arr (0) then
         TX_Mailbox := 0;
      elsif CAN1_Periph.TSR.TME.Arr (1) then
         TX_Mailbox := 1;
      elsif CAN1_Periph.TSR.TME.Arr (2) then
         TX_Mailbox := 2;
      else
         return No_Mailbox;
      end if;

      if Frame.ID_Type = Standard then
         if TX_Mailbox = 0 then
            CAN1_Periph.TI0R          := (others => <>);
            CAN1_Periph.TI0R.TXRQ     := False;
            CAN1_Periph.TI0R.STID     := Frame.Standard_ID;
            CAN1_Periph.TI0R.IDE      := False;
            CAN1_Periph.TI0R.RTR      := False;
            CAN1_Periph.TDT0R.DLC     := Frame.DLC;
            CAN1_Periph.TDT0R.TGT     := False;
            CAN1_Periph.TDL0R.Arr (0) := Frame.Data (1);
            CAN1_Periph.TDL0R.Arr (1) := Frame.Data (2);
            CAN1_Periph.TDL0R.Arr (2) := Frame.Data (3);
            CAN1_Periph.TDL0R.Arr (3) := Frame.Data (4);
            CAN1_Periph.TDH0R.Arr (4) := Frame.Data (5);
            CAN1_Periph.TDH0R.Arr (5) := Frame.Data (6);
            CAN1_Periph.TDH0R.Arr (6) := Frame.Data (7);
            CAN1_Periph.TDH0R.Arr (7) := Frame.Data (8);
            CAN1_Periph.TI0R.TXRQ     := True;

            loop
               exit when CAN1_Periph.TSR.RQCP0 = True;
            end loop;
         elsif TX_Mailbox = 1 then
            CAN1_Periph.TI1R          := (others => <>);
            CAN1_Periph.TI1R.STID     := Frame.Standard_ID;
            CAN1_Periph.TI1R.IDE      := False;
            CAN1_Periph.TI1R.RTR      := False;
            CAN1_Periph.TDT1R.DLC     := Frame.DLC;
            CAN1_Periph.TDT1R.TGT     := False;
            CAN1_Periph.TDL1R.Arr (0) := Frame.Data (1);
            CAN1_Periph.TDL1R.Arr (1) := Frame.Data (2);
            CAN1_Periph.TDL1R.Arr (2) := Frame.Data (3);
            CAN1_Periph.TDL1R.Arr (3) := Frame.Data (4);
            CAN1_Periph.TDH1R.Arr (4) := Frame.Data (5);
            CAN1_Periph.TDH1R.Arr (5) := Frame.Data (6);
            CAN1_Periph.TDH1R.Arr (6) := Frame.Data (7);
            CAN1_Periph.TDH1R.Arr (7) := Frame.Data (8);
            CAN1_Periph.TI1R.TXRQ     := True;

            loop
               exit when CAN1_Periph.TSR.RQCP1 = True;
            end loop;
         elsif TX_Mailbox = 2 then
            CAN1_Periph.TI2R          := (others => <>);
            CAN1_Periph.TI2R.STID     := Frame.Standard_ID;
            CAN1_Periph.TI2R.IDE      := False;
            CAN1_Periph.TI2R.RTR      := False;
            CAN1_Periph.TDT2R.DLC     := Frame.DLC;
            CAN1_Periph.TDT2R.TGT     := False;
            CAN1_Periph.TDL2R.Arr (0) := Frame.Data (1);
            CAN1_Periph.TDL2R.Arr (1) := Frame.Data (2);
            CAN1_Periph.TDL2R.Arr (2) := Frame.Data (3);
            CAN1_Periph.TDL2R.Arr (3) := Frame.Data (4);
            CAN1_Periph.TDH2R.Arr (4) := Frame.Data (5);
            CAN1_Periph.TDH2R.Arr (5) := Frame.Data (6);
            CAN1_Periph.TDH2R.Arr (6) := Frame.Data (7);
            CAN1_Periph.TDH2R.Arr (7) := Frame.Data (8);
            CAN1_Periph.TI2R.TXRQ     := True;

            loop
               exit when CAN1_Periph.TSR.RQCP2 = True;
            end loop;
         end if;
      elsif Frame.ID_Type = Extended then
         if TX_Mailbox = 0 then
            CAN1_Periph.TI0R     := (others => <>);
            CAN1_Periph.TI0R.IDE := True;
            From_Extended_ID
              (Frame.Extended_ID, STID => CAN1_Periph.TI0R.STID,
               EXID                    => CAN1_Periph.TI0R.EXID);
            CAN1_Periph.TDT0R.DLC     := Frame.DLC;
            CAN1_Periph.TDT0R.TGT     := False;
            CAN1_Periph.TDL0R.Arr (0) := Frame.Data (1);
            CAN1_Periph.TDL0R.Arr (1) := Frame.Data (2);
            CAN1_Periph.TDL0R.Arr (2) := Frame.Data (3);
            CAN1_Periph.TDL0R.Arr (3) := Frame.Data (4);
            CAN1_Periph.TDH0R.Arr (4) := Frame.Data (5);
            CAN1_Periph.TDH0R.Arr (5) := Frame.Data (6);
            CAN1_Periph.TDH0R.Arr (6) := Frame.Data (7);
            CAN1_Periph.TDH0R.Arr (7) := Frame.Data (8);
            CAN1_Periph.TI0R.TXRQ     := True;
         elsif TX_Mailbox = 1 then
            CAN1_Periph.TI1R     := (others => <>);
            CAN1_Periph.TI1R.IDE := True;
            From_Extended_ID
              (Frame.Extended_ID, STID => CAN1_Periph.TI1R.STID,
               EXID                    => CAN1_Periph.TI1R.EXID);
            CAN1_Periph.TDT1R.DLC     := Frame.DLC;
            CAN1_Periph.TDT1R.TGT     := False;
            CAN1_Periph.TDL1R.Arr (0) := Frame.Data (1);
            CAN1_Periph.TDL1R.Arr (1) := Frame.Data (2);
            CAN1_Periph.TDL1R.Arr (2) := Frame.Data (3);
            CAN1_Periph.TDL1R.Arr (3) := Frame.Data (4);
            CAN1_Periph.TDH1R.Arr (4) := Frame.Data (5);
            CAN1_Periph.TDH1R.Arr (5) := Frame.Data (6);
            CAN1_Periph.TDH1R.Arr (6) := Frame.Data (7);
            CAN1_Periph.TDH1R.Arr (7) := Frame.Data (8);
            CAN1_Periph.TI1R.TXRQ     := True;
         elsif TX_Mailbox = 2 then
            CAN1_Periph.TI2R     := (others => <>);
            CAN1_Periph.TI2R.IDE := True;
            From_Extended_ID
              (Frame.Extended_ID, STID => CAN1_Periph.TI2R.STID,
               EXID                    => CAN1_Periph.TI2R.EXID);
            CAN1_Periph.TDT2R.DLC     := Frame.DLC;
            CAN1_Periph.TDT2R.TGT     := False;
            CAN1_Periph.TDL2R.Arr (0) := Frame.Data (1);
            CAN1_Periph.TDL2R.Arr (1) := Frame.Data (2);
            CAN1_Periph.TDL2R.Arr (2) := Frame.Data (3);
            CAN1_Periph.TDL2R.Arr (3) := Frame.Data (4);
            CAN1_Periph.TDH2R.Arr (4) := Frame.Data (5);
            CAN1_Periph.TDH2R.Arr (5) := Frame.Data (6);
            CAN1_Periph.TDH2R.Arr (6) := Frame.Data (7);
            CAN1_Periph.TDH2R.Arr (7) := Frame.Data (8);
            CAN1_Periph.TI2R.TXRQ     := True;
         end if;
      end if;

      if CAN1_Periph.TSR.TERR0 = True then
         return Transmission_Error;
      elsif CAN1_Periph.TSR.TERR1 = True then
         return Transmission_Error;
      elsif CAN1_Periph.TSR.TERR2 = True then
         return Transmission_Error;
      end if;

      if CAN1_Periph.TSR.ALST0 = True then
         return Arbitration_Loss;
      elsif CAN1_Periph.TSR.ALST1 = True then
         return Arbitration_Loss;
      elsif CAN1_Periph.TSR.ALST2 = True then
         return Arbitration_Loss;
      end if;

      if CAN1_Periph.TSR.TXOK0 = True then
         return Ok;
      elsif CAN1_Periph.TSR.TXOK1 = True then
         return Ok;
      elsif CAN1_Periph.TSR.TXOK2 = True then
         return Ok;
      end if;

      return Ok;
   end Transmit;

end STM32.CAN;
