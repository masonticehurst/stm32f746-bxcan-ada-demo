------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with STM32.SDRAM; use STM32.SDRAM;

package body STM32.Board is

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Clear (All_LEDs);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Set (All_LEDs);
   end All_LEDs_On;

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
   begin
      Enable_Clock (All_LEDs);

      Configure_IO (All_LEDs,
                    (Mode        => Mode_Out,
                     Output_Type => Push_Pull,
                     Speed       => Speed_100MHz,
                     Resistors   => Floating));
   end Initialize_LEDs;

   ---------------------------
   -- Initialize_SDRAM_GPIO --
   ---------------------------

   procedure Initialize_SDRAM_GPIO is
   begin
      Enable_Clock (SDRAM_PINS);

      Configure_IO (SDRAM_PINS,
                    (Mode           => Mode_AF,
                     AF             => GPIO_AF_FMC_12,
                     AF_Speed       => Speed_50MHz,
                     AF_Output_Type => Push_Pull,
                     Resistors      => Pull_Up));

      Lock (SDRAM_PINS);
   end Initialize_SDRAM_GPIO;

   ----------------------
   -- Initialize_SDRAM --
   ----------------------
   procedure Initialize_SDRAM is
   begin
      Initialize_SDRAM_GPIO;
      STM32.SDRAM.Initialize (STM32.Device.System_Clock_Frequencies.SYSCLK,
                              SDRAM_Base,
                              SDRAM_Bank,
                              SDRAM_CAS_Latency,
                              SDRAM_Refresh_Cnt,
                              SDRAM_Min_Delay_In_ns,
                              SDRAM_Row_Bits,
                              SDRAM_Mem_Width,
                              SDRAM_CLOCK_Period,
                              SDRAM_Read_Burst,
                              SDRAM_Read_Pipe);
   end Initialize_SDRAM;

   -------------------------
   -- Initialize_I2C_GPIO --
   -------------------------

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
   is
      Id : constant I2C_Port_Id := As_Port_Id (Port);
      Points     : constant GPIO_Points (1 .. 2) :=
                     (if Id = I2C_Id_1 then (PB8, PB9)
                      elsif Id = I2C_Id_3 then (PH7, PH8)
                      else  (PA0, PA0));

   begin
      if Id = I2C_Id_2 or else Id = I2C_Id_4 then
         raise Unknown_Device with
           "This I2C_Port cannot be used on this board";
      end if;

      Enable_Clock (Points);

      Configure_IO (Points,
                    (Mode           => Mode_AF,
                     AF             => GPIO_AF_I2C2_4,
                     AF_Speed       => Speed_25MHz,
                     AF_Output_Type => Open_Drain,
                     Resistors      => Floating));
      Lock (Points);
   end Initialize_I2C_GPIO;

      ----------------------
   -- Setup_I2C_Master --
   ----------------------

   procedure Setup_I2C_Master (Port           : in out I2C_Port'Class;
                               SDA, SCL       : GPIO_Point;
                               SDA_AF, SCL_AF : GPIO_Alternate_Function;
                               Clock_Speed    : UInt32)
   is
      I2C_Conf : I2C_Configuration;
   begin
      --  GPIO --
      Enable_Clock (SDA & SCL);

      Configure_IO (SDA,
                    (Mode           => Mode_AF,
                     AF             => SDA_AF,
                     AF_Speed       => Speed_High,
                     AF_Output_Type => Open_Drain,
                     Resistors      => Floating));
      Configure_IO (SCL,
                    (Mode           => Mode_AF,
                     AF             => SCL_AF,
                     AF_Speed       => Speed_High,
                     AF_Output_Type => Open_Drain,
                     Resistors      => Floating));
      Lock (SDA & SCL);

      -- I2C --

      Enable_Clock (Port);
      delay until Clock + Milliseconds (200);
      Reset (Port);

      I2C_Conf.Own_Address := 16#00#;
      I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
      I2C_Conf.General_Call_Enabled := False;
      I2C_Conf.Clock_Stretching_Enabled := True;

      I2C_Conf.Clock_Speed := Clock_Speed;
      I2C_Conf.Enable_DMA  := True;

      Port.Configure (I2C_Conf);
   end Setup_I2C_Master;

   --------------------------
   -- Initialize_UART_GPIO --
   --------------------------

   --  procedure Initialize_UART_GPIO (Port : in out USART_Port)
   --  is
   --     Id : constant USART_Port_Id := As_Port_Id (Port);
   --     Points     : constant GPIO_Points (1 .. 3) :=
   --                    (if Id = USART_Id_1 then (PA9, PB7)     --  STLINK port
   --                     elsif Id = USART_Id_6 then (PC6, PC7)  --  CLK on PG6
   --                     elsif Id = UART_Id_7 then (PF7, PF6)   --  RTS on PF8, CTS on PF9
   --                     else  (PA0, PA0));
   --     AF : constant GPIO_Alternate_Function :=
   --                    (if Id in USART_Id_1 | UART_Id_5 then GPIO_AF_USART1_7
   --                     else GPIO_AF_UART4_8);

   --  begin
   --     if Id in USART_Id_2 | USART_Id_3 | UART_Id_8 then
   --        raise Unknown_Device with
   --          "This USART_Port cannot be used on this board";
   --     end if;

   --     Enable_Clock (Points);

   --     Configure_IO (Points,
   --                   (Mode           => Mode_AF,
   --                    AF             => AF,
   --                    AF_Speed       => Speed_25MHz,
   --                    AF_Output_Type => Open_Drain,
   --                    Resistors      => Floating));
   --     Lock (Points);
   --  end Initialize_UART_GPIO;

   --  -----------------
   --  -- Setup_USART --
   --  -----------------

   --  procedure Setup_USART (Port                 : in out USART_Port'Class;
   --                         TX, RX, CLK          : GPIO_Point;
   --                         TX_AF, RX_AF, CLK_AF : GPIO_Alternate_Function;
   --                         Baud_Rate            : UInt32;
   --                         Synchronous          : Boolean := True)
   --  is
   --     USART_Conf : USART_Configuration;
   --  begin
   --     --  GPIO --
   --     Enable_Clock (TX & RX & CLK);

   --     Configure_IO (TX,
   --                   (Mode           => Mode_AF,
   --                    AF             => TX_AF,
   --                    AF_Speed       => Speed_High,
   --                    AF_Output_Type => Open_Drain,
   --                    Resistors      => Floating));
   --     Configure_IO (RX,
   --                   (Mode           => Mode_AF,
   --                    AF             => RX_AF,
   --                    AF_Speed       => Speed_High,
   --                    AF_Output_Type => Open_Drain,
   --                    Resistors      => Floating));
   --     Lock (TX & RX);

   --     if Synchronous then
   --        Configure_IO (CLK,
   --                      (Mode           => Mode_AF,
   --                       AF             => CLK_AF,
   --                       AF_Speed       => Speed_High,
   --                       AF_Output_Type => Open_Drain,
   --                       Resistors      => Floating));
   --        Lock (CLK);
   --     end if;

   --     -- USART --
   --     Enable_Clock (Port);
   --     delay until Clock + Milliseconds (200);
   --     Reset (Port);

   --     USART_Conf.Mode := (
   --        if Synchronous then USART.Synchronous else USART.Asynchronous
   --     );

   --     USART_Conf.Baud_Rate := Baud_Rate;

   --     if Port in USART_Port_DMA'Class then
   --        USART_Conf.DMA_Config := USART_DMA_Config'Access;
   --        -- Disable synchronous receive messages for other tasks to run
   --        USART_Port_DMA'Class (Port) .Set_Receive_Polling_Threshold(0);
   --     end if;

   --     Port.Configure (USART_Conf);

   --  end Setup_USART;

   --  procedure Setup_UART (Port         : in out USART_Port'Class;
   --                        TX, RX       : GPIO_Point;
   --                        TX_AF, RX_AF : GPIO_Alternate_Function;
   --                        Baud_Rate    : UInt32)
   --  is
   --  begin
   --     Setup_USART (
   --        Port, TX,    RX,    PA0,
   --              TX_AF, RX_AF, GPIO_AF_USART1_7, Baud_Rate, False
   --     );
   --  end Setup_UART;

   --  procedure Setup_Serial (Baud_Rate : UInt32)
   --  is
   --  begin
   --     Setup_UART (
   --        Serial, Serial_TX,        Serial_RX,
   --                GPIO_AF_USART1_7, GPIO_AF_USART1_7, Baud_Rate
   --     );
   --  end Setup_Serial;

   --------------------------------
   -- Configure_User_Button_GPIO --
   --------------------------------

   procedure Configure_User_Button_GPIO is
   begin
      Enable_Clock (User_Button_Point);
      Configure_IO (User_Button_Point, (Mode_In, Resistors => Floating));
   end Configure_User_Button_GPIO;
begin
   Set_High_Speed_External_Clock (25000000);
   Initialize_SDRAM;
end STM32.Board;
