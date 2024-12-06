with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure day05 with SPARK_Mode is
   type Integer_Pair is record
      First : Integer;
      Second : Integer;
   end record;
   type Pair_Array is array (Positive range <>) of Integer_Pair;
   type Integer_Array is array (Positive range <>) of Integer;
   type Integer_Matrix is array (Positive range <>)
      of Integer_Array (1 .. 32);
   type Natural_Array is array (Positive range <>) of Natural;

   File_Name     : constant String := "input.txt";
   File          : File_Type;
   Line          : String (1 .. 256);
   Line_Last     : Natural;
   Rule_Set      : Pair_Array (1 .. 2048);
   Rule_Len      : Natural := 0;
   Page_Update   : Integer_Matrix (1 .. 256);
   Update_Len    : Natural_Array (1 .. 256);
   Total_Update  : Natural := 0;
   Read_Num      : Integer;
   Num_Last      : Natural;
   Comma_Idx     : Natural;
   Comma_Idx_prv : Natural;
   Pipe_Idx      : Natural;
   Mid_Sum       : Integer := 0;

   function Index_Of (A : Integer_Array; Value : Integer) return Natural is
   begin
      for I in A'Range loop
         if A (I) = Value then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Of;

   function Rule_Passed (Rule : Integer_Pair; Pages : Integer_Array)
         return Boolean is
      First_Idx : Natural;
      Second_Idx : Natural;
   begin
      First_Idx := Index_Of (Pages, Rule.First);
      Second_Idx := Index_Of (Pages, Rule.Second);
      --  if rule applies to pages not in the update, trivially passes
      if First_Idx = 0 or else Second_Idx = 0 then
         return True;
      end if;
      --  rule only passes if first comes before second in the list
      if First_Idx < Second_Idx then
         return True;
      end if;
      return False;
   end Rule_Passed;

   function Satisfies_Ruleset (X : Natural) return Boolean is
   begin
      for I in 1 .. Rule_Len loop
         if not Rule_Passed (Rule_Set (I),
               Page_Update (X) (1 .. Update_Len (X)))
         then
            return False;
         end if;
      end loop;
      return True;
   end Satisfies_Ruleset;

   function Get_Mid_Page (X : Natural) return Integer is
      Mid_Idx : Natural;
   begin
      --  recall: one-based index, so half of length + 1 is mid point
      Mid_Idx := Update_Len (X) / 2 + 1;
      return Page_Update (X) (Mid_Idx);
   end Get_Mid_Page;

   --  to fix the order, just swap the pages for any rule that doesn't pass
   --  similar to bubble sort
   procedure Fix_Order (X : Natural) is
      Rule : Integer_Pair;
      Pages : Integer_Array (1 .. Update_Len (X));
      First_Idx : Natural;
      Second_Idx : Natural;
      A : Integer;
      B : Integer;
   begin
      while not Satisfies_Ruleset (X) loop
         for I in 1 .. Rule_Len loop
            Rule := Rule_Set (I);
            Pages := Page_Update (X) (1 .. Update_Len (X));
            if not Rule_Passed (Rule, Pages) then
               First_Idx := Index_Of (Pages, Rule.First);
               Second_Idx := Index_Of (Pages, Rule.Second);
               --  swap page order
               A := Pages (First_Idx);
               B := Pages (Second_Idx);
               Page_Update (X) (First_Idx) := B;
               Page_Update (X) (Second_Idx) := A;
               exit;
            end if;
         end loop;
      end loop;
   end Fix_Order;

begin
   --  Open the file for reading
   Open (File => File, Mode => In_File, Name => File_Name);

   --  Read the file line by line
   while not End_Of_File (File) loop
      Get_Line (File, Line, Line_Last);
      Comma_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => ",", From => 1);
      Pipe_Idx := Index (Source => Line (1 .. Line_Last),
         Pattern => "|", From => 1);

      --  process the line as dd|dd; adds a pair to 'Rule_Set'
      if Pipe_Idx /= 0 then
         Rule_Len := Rule_Len + 1;
         Get (Line (1 .. Pipe_Idx - 1), Read_Num, Num_Last);
         Rule_Set (Rule_Len).First := Read_Num;
         Get (Line (Pipe_Idx + 1 .. Line_Last), Read_Num, Num_Last);
         Rule_Set (Rule_Len).Second := Read_Num;
      end if;

      --  process the line as dd,dd,...,dd; adds a list to 'Page_Update'
      if Comma_Idx /= 0 then
         Comma_Idx_prv := 0;
         Total_Update := Total_Update + 1;
         Update_Len (Total_Update) := 0;
         while Comma_Idx /= 0 loop
            Update_Len (Total_Update) := Update_Len (Total_Update) + 1;
            Get (Line (Comma_Idx_prv + 1 .. Comma_Idx - 1),
               Read_Num, Num_Last);
            Page_Update (Total_Update) (Update_Len (Total_Update)) := Read_Num;
            Comma_Idx_prv := Comma_Idx;
            Comma_Idx := Index (Source => Line (Comma_Idx + 1 .. Line_Last),
               Pattern => ",", From => Comma_Idx + 1);
         end loop;
         Update_Len (Total_Update) := Update_Len (Total_Update) + 1;
         Get (Line (Comma_Idx_prv + 1 .. Line_Last), Read_Num, Num_Last);
         Page_Update (Total_Update) (Update_Len (Total_Update)) := Read_Num;
      end if;
   end loop;

   --  Close the file
   Close (File);

   --  Part A: for each page update, check against rule set
   Mid_Sum := 0;
   for I in 1 .. Total_Update loop
      if Satisfies_Ruleset (I) then
         Mid_Sum := Mid_Sum + Get_Mid_Page (I);
      end if;
   end loop;
   Put_Line ("Part A: " & Integer'Image (Mid_Sum));

   --  Part B: for non-satisfactory page update, re-arrange and get mid
   Mid_Sum := 0;
   for I in 1 .. Total_Update loop
      if not Satisfies_Ruleset (I) then
         Fix_Order (I);
         Mid_Sum := Mid_Sum + Get_Mid_Page (I);
      end if;
   end loop;
   Put_Line ("Part B: " & Integer'Image (Mid_Sum));

end day05;
