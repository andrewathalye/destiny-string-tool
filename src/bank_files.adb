with Ada.Text_IO; use Ada.Text_IO;

with Config; use Config;
package body Bank_Files is
	-- Read Meta Headers from a Bank File
	-- TODO: Needs cleanup or rewrite for Loop handling
	function Read_Metas (
		Bank_Header : Bank_Header_Type;
		Bank_File : Ada.Streams.Stream_IO.File_Type;
		Bank_Stream : Stream_Access)
	return Meta_Array
	is
		Metas : Meta_Array (1 .. Natural (Bank_Header.Num_Metas));
		Loop_Count : Natural := 0; -- Keep track of gaps within file
	begin
		if Debug then
			Put_Line ("[Debug] Read Metas");
		end if;

		-- Set Index to Meta Offset
		Set_Index (
			Bank_File,
			Stream_IO.Positive_Count (Bank_Header.Offset_Meta + 1));

		-- Manually read in each Meta Header since Offset Entry must be adjusted
		Read_Meta :
		for M in Metas'Range loop
			-- Skip gaps between Meta Headers
			Find_Meta :
			loop
				-- Stop searching for Meta Headers if the whole file has been read
				if Size (Bank_File) - Index (Bank_File)
					< (Meta_Header_Type'Size / 8) - 1
				then
					if Debug then
						Put_Line (Standard_Error,
							"[Warning] End of file reached without finding Meta Header.");
					end if;

					exit Read_Meta;
				end if;

				Meta_Header_Type'Read (Bank_Stream, Metas (M));

				-- If this Meta Header is valid, continue
				if Metas (M).Num_Entries /= 0 then
					exit Find_Meta;
				end if;

				Loop_Count := @ + 1;

				if Debug then
					Put_Line (Standard_Error,
						"[Warning] A gap was encountered when reading Meta Headers.");
				end if;
			end loop Find_Meta;

			Metas (M).Offset_Entry := @
				+ Unsigned_64 (Bank_Header.Offset_Meta)
				+ Unsigned_64 ((M + Loop_Count - 1) * (Meta_Header_Type'Size / 8));
			-- Note: This relies on overflowing an unsigned 64-bit integer!
			-- One is subtracted from M because of Ada indexing

			Metas (M).Loop_Count := Unsigned_32 (Loop_Count);
				-- Store loop count for later use
			if Debug then
				Put_Line (Meta_Header_Type'Image (Metas (M)));
			end if;
		end loop Read_Meta;

		if Debug then
			Put_Line (
				"[Debug] Found Entries: "
				& Unsigned_32'Image (
					[for M of Metas => M.Num_Entries]'Reduce ("+", 0)));
		end if;

		return Metas;
	end Read_Metas;

	-- Read Entry headers (potentially multiple for each Meta Header)
	function Read_Entries (
		Metas : Meta_Array;
		Bank_File : Ada.Streams.Stream_IO.File_Type;
		Bank_Stream : Stream_Access)
	return Entry_Array
	is
		-- Create an array of size sufficient to hold all Entry Headers
		Entries : Entry_Array (1 ..
			[for M of Metas => Natural (M.Num_Entries)]'Reduce ("+", 0));
		Entry_Header : Entry_Header_Type; -- Temporary storage for Entry Header
		Entry_Index : Natural := Entries'First;
		Meta_Index : Natural := Metas'First;
		First_Element_Index : Stream_IO.Positive_Count;
			-- Reference to first Entry in Meta (for offsets)
	begin
		if Debug then
			Put_Line ("[Debug] Reading Entries");
		end if;

		-- Iterate over all Meta Headers
		Read_Meta_References :
		for M of Metas loop
			-- Set Index to First Entry
			Set_Index (
				Bank_File,
				Stream_IO.Positive_Count (M.Offset_Entry + 1));

			-- Iterate over Entries in Meta Header
			Read_Entry_Data :
			for E in 1 .. M.Num_Entries loop
				-- Store index of first Entry element
				First_Element_Index := Index (Bank_File);

				-- Read Entry Header and correct offsets
				Entry_Header_Type'Read (Bank_Stream, Entry_Header);
				Entry_Header.Offset_String := @ + Unsigned_32 (First_Element_Index) + 7;
					-- Calculate absolute string offset
					-- TODO: Determine why the static + 7 offset is needed

				Entry_Header.Hash_Index := Unsigned_32 (Meta_Index) + M.Loop_Count;
					-- Calculate Hash Index for later use

				if Debug then
					Put_Line (Entry_Header_Type'Image (Entry_Header));
				end if;

				-- Add collected Entry to array
				Entries (Entry_Index) := Entry_Header;
				Entry_Index := @ + 1;
			end loop Read_Entry_Data;

			Meta_Index := @ + 1;
		end loop Read_Meta_References;

		return Entries;
	end Read_Entries;


	-- Version-agnostic procedure to read a bank header
	-- Supports definition as the Read attribute
	procedure Read_Bank_Header_Type (
		S : not null access Root_Stream_Type'Class;
		Item : out Bank_Header_Type)
	is
		-- Raw Type for Bank Header : May be read directly from Stream
		type Raw_Bank_Header_Type (Mode : Mode_Type) is record
			File_Length : Unsigned_32; -- 0 .. 3
			Discard_1 : Discard_Array (4 .. 7);
			Num_Metas : Unsigned_32; -- 8 .. B

			case Mode is
				when d1 | d2 =>
					Discard_2_D1D2 : Discard_Array (16#C# .. 16#47#);
					Num_Strings_D1D2 : Unsigned_32; -- 48 .. 4B
					Discard_3_D1D2 : Discard_Array (16#4C# .. 16#4F#);
					Offset_Meta_D1D2 : Unsigned_32; -- 50 .. 53, add 0x60
				when d2s17 =>
					Discard_2_D2S17 : Discard_Array (16#C# .. 16#37#);
					Num_Strings_D2S17 : Unsigned_32; -- 38 .. 3B
					Discard_3_D2S17 : Discard_Array (16#3C# .. 16#3F#);
					Offset_Meta_D2S17 : Unsigned_32; -- 40 .. 43, add 0x50
			end case;
		end record;

		-- Variables
		RBH : Raw_Bank_Header_Type (Item.Mode);
	begin
		Raw_Bank_Header_Type'Read (S, RBH);
		case RBH.Mode is
			when d1 | d2 =>
				Item := (RBH.Mode,
					RBH.File_Length,
					RBH.Num_Metas,
					RBH.Num_Strings_D1D2,
					RBH.Offset_Meta_D1D2 + 16#60#); -- Real start is 0x60 later
			when d2s17 =>
				Item := (RBH.Mode,
					RBH.File_Length,
					RBH.Num_Metas,
					RBH.Num_Strings_D2S17,
					RBH.Offset_Meta_D2S17 + 16#50#); -- Real start is 0x50 later
		end case;
	end Read_Bank_Header_Type;
end Bank_Files;
