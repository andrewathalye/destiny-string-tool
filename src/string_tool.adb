with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Config; use Config;
with Common; use Common;
with Ref_Files; use Ref_Files;
with Bank_Files; use Bank_Files;
with String_Display; use String_Display;

procedure String_Tool is
	-- Local Variables
	SE : Search_Type;
	D : Directory_Entry_Type;
begin
	Put_Line (Standard_Error, "Destiny String Tool v1.7");

	-- Check for sufficient arguments
	case Argument_Count is
		when 3 => null;
		when others =>
			Put_Line ("Usage: "
				& Command_Name
				& " VERSION LANGUAGE STRING_DIR");
			return;
	end case;

	-- Main loop
	declare
		Mode : constant Mode_Type := Mode_Type'Value (Argument (1));
		Language : String renames Argument (2);
		String_Dir : String renames Argument (3);
		Ref_Header : Ref_Header_Type (Mode);
		Ref_File : Stream_IO.File_Type;
		Ref_Stream : Stream_Access;
	begin
		-- Begin iterating
		Start_Search (SE, String_Dir, "*.ref");
		Process_Refs :
		while More_Entries (SE) loop
			<<Read_Ref>>
			Get_Next_Entry (SE, D);
			-- Put_Line (Standard_Error, "[Info] Processing " & Full_Name (D));
			Open (Ref_File, In_File, Full_Name (D));
			Ref_Stream := Stream (Ref_File);

			if Debug then
				Put_Line ("[Debug] Read Reference Header");
			end if;

			-- Read Reference File Header
			begin
				Ref_Header_Type'Read (Ref_Stream, Ref_Header);
			exception
				when Ada.Streams.Stream_IO.End_Error =>
					Put_Line (Standard_Error,
						"[Error] Reference file too small: "
						& Full_Name (D));
					Close (Ref_File);

					-- If an error occurred but there are still more References, loop back
					if More_Entries (SE) then
						goto Read_Ref;
					else
						exit Process_Refs;
					end if;
			end;

			if Debug then
				Put_Line (Ref_Header'Image);
			end if;

			declare
				Hashes : Hash_Array (1 .. Positive (Ref_Header.Num_Hashes));
				Bank_File : Stream_IO.File_Type;
				Bank_Stream : Stream_Access;
				Bank_Header : Bank_Header_Type (Mode);

				-- Bank File Name
				Bank_Name : constant String := String_Dir
					& "/" & Language_Bank (Ref_Header,
						Language_Type'Value (Language))
					& ".str";
			begin
				Hash_Array'Read (Ref_Stream, Hashes);
				Close (Ref_File);

				if Debug then
					Put_Line (Hashes'Image);
					Put_Line ("[Debug] Read Strings from " & Bank_Name);
				end if;

				-- Start handling bank file
				if not Exists (Bank_Name) then
					Put_Line (Standard_Error, "[Error] Unable to open string bank "
						& Bank_Name);
					exit Process_Refs;
				end if;

				Open (Bank_File, In_File, Bank_Name);
				Bank_Stream := Stream (Bank_File);

				Bank_Header_Type'Read (Bank_Stream, Bank_Header);

				if Debug then
					Put_Line (Bank_Header'Image);
				end if;

				-- Read the Meta Headers and Entry Headers and print all Strings
				declare
					Entries : constant Entry_Array := Read_Entries (
						Read_Metas (
							Bank_Header,
							Bank_File,
							Bank_Stream),
						Bank_File,
						Bank_Stream);
				begin
					Print_Strings (Entries, Hashes, Bank_File, Bank_Stream);
				end;

				Close (Bank_File);
			end;
		end loop Process_Refs;
		End_Search (SE);
	end;
end String_Tool;
