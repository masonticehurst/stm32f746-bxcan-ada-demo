with Ada.Exceptions;

package Exception_Handler is
   procedure Last_Chance_Handler (E : Ada.Exceptions.Exception_Occurrence);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

   procedure NMI_Handler with
     Export, Convention => C, External_Name => "NMI_Handler";

   procedure HardFault_Handler with
     Export, Convention => Ada, External_Name => "HardFault_Handler";

   procedure BusFault_Handler with
     Export, Convention => Ada, External_Name => "BusFault_Handler";

   procedure UsageFault_Handler with
     Export, Convention => Ada, External_Name => "UsageFault_Handler";

   procedure MemManage_Handler with
     Export, Convention => Ada, External_Name => "MemManage_Handler";

end Exception_Handler;
