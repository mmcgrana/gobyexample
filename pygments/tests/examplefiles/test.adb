--  Model IED Simulator
--  COL Gene Ressler, 1 December 2007
with Ada.Text_IO;

with Ada.Characters.Latin_1;
use  Ada.Characters.Latin_1;

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;

with Ada.Strings;
with Ada.Strings.Bounded;

with Binary_Search;

with Ada.Containers.Generic_Array_Sort;

package body Scanner is
   
   Constant_123   : constant Character := Character'Val (16#00#);
   MAX_KEYWORD_LENGTH_C : constant Natural := 24;
   
   New_Constant : constant New_Type
     := 2;
   
   KEYWORDS_C : constant Keyword_Array_T :=
     (To_BS("description"),
      To_BS("with"));
   
   procedure Blah;
   
   procedure blah is
   begin
      
      Declaration:
      declare
         Joe : Type_Type := Random;
      begin
         Do_Something;
      end Declaration;
      Loop_ID:
         loop
            Loop_Do;
            exit when 1=2;
         end loop Loop_ID;
      if True or else False then
         Do_This();
      elsif not False and then True then
         Do_That;
      else
         Panic;
      end if;
   end blah;
   
   function "*" (Left, Right : in Integer) return Integer is
   begin
      <<Goto_Label>>
      goto Goto_Label;
      return Left + Right;
   end "*";
   
   function Function_Specification
     (Param_1        : in Blah; 
      Param2, param3 : in access Blah_Type := 0)
     return It_Type;
   
   package Rename_Check renames Ada.Text_IO;

   type New_Float is delta 0.001 digits 12;
   
   package Package_Inst is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => MAX_KEYWORD_LENGTH_C);

   type Array_Decl12 is array (Positive range <>) of SB.Bounded_String;
   type Array_Decl3 is array (New_Type range Thing_1 .. Thing_2) of SB.Bounded_String;

   type Boring_Type is
     (Start,
      End_Error);

   subtype Sub_Type_check is Character range '0' .. '9';
   
   Initialized_Array : constant Transistion_Array_T :=
     (Start =>
        (Letter_Lower | Letter_Upper => Saw_Alpha,
         ' ' | HT | CR | LF          => Start,
         others => Begin_Error),

      End_Error => (others => Start)

     );

   type Recorder is record
      Advance      : Boolean;
      Return_Token : Token_T;
   end record;
   
   for Recorder use 8;
   
   type Null_Record is null record;
   
   type Discriminated_Record (Size : Natural) is 
      record
         A : String (1 .. Size);
      end record;
   
   pragma Unchecked_Union (Union);
   pragma Convention (C, Union);
   
   type Person is tagged 
      record
         Name   : String (1 .. 10);
         Gender : Gender_Type;
      end record;
   
   type Programmer is new Person with
      record
         Skilled_In : Language_List;
         Favorite_Langauge : Python_Type;
      end record;
   
   type Programmer is new Person 
     and Printable 
     with 
      record
         Skilled_In : Language_List;
         Blah : aliased Integer;
      end record;
   
   ---------------------
   -- Scan_Next_Token --
   ---------------------
   
   task Cyclic_Buffer_Task_Type is
      entry Insert (An_Item : in  Item);
      entry Remove (An_Item : out Item);
   end Cyclic_Buffer_Task_Type;
   
   task body Cyclic_Buffer_Task_Type is
      Q_Size : constant := 100;
      subtype Q_Range is Positive range 1 .. Q_Size;
      Length : Natural range 0 .. Q_Size := 0;
      Head, Tail : Q_Range := 1;
      Data : array (Q_Range) of Item;
   begin
      loop
         select
            when Length < Q_Size =>
               accept Insert (An_Item : in  Item) do
                  Data(Tail) := An_Item;
               end Insert;
               Tail := Tail mod Q_Size + 1;
               Length := Length + 1;
         or
            when Length > 0 =>
               accept Remove (An_Item : out Item) do
                  An_Item := Data(Head);
               end Remove;
               Head := Head mod Q_Size + 1;
               Length := Length - 1;
         end select;
      end loop;
   end Cyclic_Buffer_Task_Type;
     
     
   
   procedure Scan_Next_Token
     (S           : in     String;
      Start_Index :    out Positive;
      End_Index   : in out Natural;     --  Tricky comment
      Line_Number : in out Positive;
      Token       :    out Token_T);
   
   procedure Scan_Next_Token
     (S           : in     String;
      Start_Index :    out Positive;
      End_Index   : in out Natural;     --  Another comment
      Line_Number : in out Positive;
      Token       :    out Token_T)
   is
   begin
      Scanner_Loop:
      loop
         if New_State = End_Error then
            exit Scanner_Loop;
         end if;

         if State = Start and New_State /= Start then
            Start_Index := Peek_Index;
         end if;
      end loop Scanner_Loop;
   end Scan_Next_Token;
   
   procedure Advance is
   begin
      Peek_Index := Peek_Index + 1;
   end Advance;
      

   -- Eliminate the leading space that Ada puts in front of positive
   -- integer images.
   function Image(N : in Integer) return String is
      S : String := Integer'Image(N);
   begin
      if S(1) = ' ' then
         return S(2 .. S'Last);
      end if;
      return S;
   end Image;

end Scanner;
