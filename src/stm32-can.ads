with Ada.Real_Time;      use Ada.Real_Time;
with STM32_SVD;          use STM32_SVD;
with STM32_SVD.CAN;      use STM32_SVD.CAN;
with STM32.CAN_Internal; use STM32.CAN_Internal;
with Ada.Interrupts.Names;
with Ada.Containers.Hashed_Maps;

package STM32.CAN is

   Internal_CAN_1 : aliased Internal_CAN_Port with
     Import, Volatile, Address => CAN1_Base;
   Internal_CAN_2 : aliased Internal_CAN_Port with
     Import, Volatile, Address => CAN2_Base;

   CAN_1 : aliased CAN_Port (Internal_CAN_1'Access);
   CAN_2 : aliased CAN_Port (Internal_CAN_2'Access);

   Last_RX_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;

   type CAN_Speed is (CAN_1M, CAN_500K, CAN_250K, CAN_125K);

   type CAN_ID_Type is (Standard, Extended);

   type CAN_Frame_Type is (Data_Frame, Remote_Frame);

   type CAN_Error is
     (No_Error, Stuff_Error, Form_Error, Ack_Error, Bit_Recessive_Error,
      Bit_Dominant_Error, CRC_Error, Set_Bit_Error);

   type CAN_Status is
     (Ok, No_Mailbox, No_Message, Transmission_Error, Arbitration_Loss);

   type CAN_Filter_Type is (Mask, List);
   type CAN_Filter_Bank is range 0 .. 27;

   type CAN_Filter_Config is record
      Bank       : CAN_Filter_Bank;
      FIFO       : Natural range 0 .. 1 := 0;
      Mode       : CAN_Filter_Type;
      ID_Type    : CAN_ID_Type;
      Id1        : UInt32;
      Id2        : UInt32               := 0;
      Mask1      : UInt32               := 0;
      Mask2      : UInt32               := 0;
      Use_32_Bit : Boolean              := True;
   end record;

   --  11-bit and 29-bit ID subtypes
   subtype CAN_Standard_ID is HAL.UInt11;
   subtype CAN_Extended_ID is HAL.UInt29;

   type CAN_Data_8b is array (Natural range <>) of UInt8;

   type CAN_Frame (ID_Type : CAN_ID_Type := Standard) is record
      DLC  : UInt4 range 0 .. 8;
      Data : CAN_Data_8b (1 .. 8);
      RTR  : Boolean;
      case ID_Type is
         when Standard =>
            Standard_ID : CAN_Standard_ID;

         when Extended =>
            Extended_ID : CAN_Extended_ID;
      end case;
   end record;

   type CAN_Frame_Buffer is array (Natural range <>) of CAN_Frame;
   type CAN_Callback is access procedure (Frame : CAN_Frame);

   type Handler_Key (Kind : CAN_ID_Type := Standard) is record
      case Kind is
         when Standard =>
            SID : CAN_Standard_ID;
         when Extended =>
            XID : CAN_Extended_ID;
      end case;
   end record;

   overriding function "=" (L, R : Handler_Key) return Boolean;
   function Hash (K : Handler_Key) return Ada.Containers.Hash_Type;

   package Handler_Map is new Ada.Containers.Hashed_Maps
     (Key_Type => Handler_Key, Element_Type => CAN_Callback, Hash => Hash,
      Equivalent_Keys => "=");

   type Handler_Entry (ID_Type : CAN_ID_Type := Standard) is record
      CB : CAN_Callback;
      case ID_Type is
         when Standard =>
            Standard_ID : CAN_Standard_ID;
         when Extended =>
            Extended_ID : CAN_Extended_ID;
      end case;
   end record;

   procedure Reset (This : aliased in out CAN_Port'Class);

   procedure Initialize
     (This : aliased in out CAN_Port'Class; Speed : CAN_Speed);

   procedure Configure_Filter
     (This : aliased in out CAN_Port'Class; Filter : CAN_Filter_Config);

   function Transmit
     (This : aliased in out CAN_Port'Class; Frame : CAN_Frame)
      return CAN_Status;

   function Receive
     (This : aliased in out CAN_Port'Class; Frame : out CAN_Frame)
      return CAN_Status;

   task CAN_Dispatcher;

   protected Receiver is
      pragma Interrupt_Priority;

      procedure Register_Handler (ID : CAN_Standard_ID; CB : CAN_Callback);
      procedure Register_Handler (ID : CAN_Extended_ID; CB : CAN_Callback);

      procedure Get_Next_Frame (F : out CAN_Frame; Has_Frame : out Boolean);

      -- Used by dispatcher task to wait until at least one frame is queued
      entry Wait_For_Frame;

      -- Resolve a frame to a callback (if any)
      procedure Get_Callback
        (Frame : CAN_Frame; Found : out Boolean; CB : out CAN_Callback);
   private
      procedure Interrupt_Handler;

      pragma Attach_Handler
        (Interrupt_Handler, Ada.Interrupts.Names.CAN1_RX0_Interrupt);

      pragma Attach_Handler
        (Interrupt_Handler, Ada.Interrupts.Names.CAN1_RX1_Interrupt);

      Buffer         : CAN_Frame_Buffer (0 .. 31);
      Head           : Natural := 0;
      Tail           : Natural := 0;
      Count_Received : Natural := 0;
      Handlers       : Handler_Map.Map;
   end Receiver;
private
   procedure Enable_Clock (This : aliased in out CAN_Port'Class);
   procedure Configure_IO (This : aliased in out CAN_Port'Class);

   --     type CAN_Controller
   --       (Periph : not null access CAN_SVD_Periph.Peripheral)
   --     is limited new HAL.Block_Drivers.Block_Driver
   --       and
   --         CAN_SVD_Periph.CAN_Driver
   --     with record
   --        Has_Info   : Boolean := False;
   --     end record;

   --     overriding function Read
   --       (This         : in out CAN_Controller;
   --        Block_Number : UInt64;
   --        Data         : out HAL.Block_Drivers.Block) return Boolean
   --       with Pre => Data'Length <= 16#10000#;

   --     overriding function Write
   --       (This         : in out CAN_Controller;
   --        Block_Number : UInt64;
   --        Data         : HAL.Block_Drivers.Block) return Boolean
   --       with Pre => Data'Length <= 16#10000#;

end STM32.CAN;
