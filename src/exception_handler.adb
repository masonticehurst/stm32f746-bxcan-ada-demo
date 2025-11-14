with System;
with LCD_Std_Out;         use LCD_Std_Out;
with Ada.Exceptions;      use Ada.Exceptions;
with System.Machine_Code; use System.Machine_Code;
with Partitions;          use Partitions;
with File_Block_Drivers;  use File_Block_Drivers;
with File_IO;             use File_IO;

package body Exception_Handler is
   procedure Last_Chance_Handler (E : Exception_Occurrence) is
   begin
      Put_Line (Exception_Information (E), Red);
   end Last_Chance_Handler;

   procedure NMI_Handler is
   begin
      loop
         null;
      end loop;
   end NMI_Handler;

   procedure HardFault_Handler is
   begin
      loop
         null;
      end loop;
   end HardFault_Handler;

   procedure BusFault_Handler is
   begin
      loop
         null;
      end loop;
   end BusFault_Handler;

   procedure UsageFault_Handler is
   begin
      loop
         null;
      end loop;
   end UsageFault_Handler;

   procedure MemManage_Handler is
   begin
      loop
         null;
      end loop;
   end MemManage_Handler;

end Exception_Handler;
