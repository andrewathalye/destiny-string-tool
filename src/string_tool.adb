with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings; use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Interfaces; use Interfaces;

with Util; use Util;
with Data_Types; use Data_Types;

procedure String_Tool is
	
	-- Enum Type for Languages
	type Language_Type is (English, Japanese, German, French, Spanish_LA, Spanish_ES, Italian, Portuguese, Polish, Russian, Korean, Chinese_Traditional, Chinese_Simplified);

	-- Local Variables
	SE : Search_Type;
	D : Directory_Entry_Type;
	Chosen_Language : Package_Entry_Hash;

begin
	Put_Line (Standard_Error, "Destiny String Tool v1.1");

	-- Check for sufficient arguments
	case Argument_Count is
		when 2 | 3 => null;
		when others =>
			Put_Line ("Usage: " & Command_Name & " D1 | D2 [LANGUAGE] STRING_DIR");
			return;
	end case;
	
	-- Main loop
	declare
		Mode : constant Mode_Type := Mode_Type'Value (Argument (1));
		Language : String renames Argument (2);
		String_Dir : String renames Argument (Argument_Count);
		RH : Ref_Header (Mode);
		RF : Stream_IO.File_Type;
		RS : Stream_Access;
	begin
		-- Begin iterating
		Start_Search (SE, String_Dir, "*.ref");
		while More_Entries (SE) loop
			<<Read_Entry>>
			Get_Next_Entry (SE, D);	
			-- Put_Line (Standard_Error, "[Info] Processing " & Full_Name (D));
			Open (RF, In_File, Full_Name (D));
			RS := Stream (RF);

			-- Read Reference File Header
			if (Ref_Header'Size / 8) > Size (RF) then
				Put_Line (Standard_Error, "[Error] Reference file too small");
				Close (RF);
				if More_Entries (SE) then
					goto Read_Entry;
				else
					exit;
				end if;
			end if;

			Ref_Header'Read (RS, RH);

			-- Select language hash
			if Argument_Count = 3 then
				Chosen_Language := (case Language_Type'Value (Language) is
					when English => RH.English_Hash,
					when Japanese => RH.Japanese_Hash,
					when German => RH.German_Hash,
					when French => RH.French_Hash,
					when Spanish_LA => RH.Spanish_LA_Hash,
					when Spanish_ES => RH.Spanish_ES_Hash,
					when Italian => RH.Italian_Hash,
					when Portuguese => RH.Portuguese_Hash,
					when Polish => RH.Polish_Hash,
					when Russian => RH.Russian_Hash,
					when Korean => RH.Korean_Hash,
					when Chinese_Traditional => RH.Chinese_Traditional_Hash,
					when Chinese_Simplified => RH.Chinese_Simplified_Hash
				);
			else
				Chosen_Language := RH.English_Hash;
			end if;

			-- Put_Line (RH'Image); --TODO Debug, requires GNAT2022

			declare
				HA : Hash_Array (1 .. Natural (case Mode is when d1 => RH.Num_Hashes_D1, when d2 => RH.Num_Hashes_D2));
				BF : Stream_IO.File_Type;
				BS : Stream_Access;
				BH : Bank_Header;
				-- Bank File Name
				BN : constant String := String_Dir & "/" & Hex_String (Package_ID (Chosen_Language)) & "-" & Hex_String (Entry_ID (Chosen_Language)) & ".str";
			begin
				Hash_Array'Read (RS, HA);
				Close (RF);

				-- Start handling bank file
				if Exists (BN) then
					Open (BF, In_File, BN);
				else
					Put_Line (Standard_Error, "[Error] Unable to open string bank " & BN);
					exit;
				end if;

				BS := Stream (BF);

				Bank_Header'Read (BS, BH);
				BH.Offset_Meta := BH.Offset_Meta + 16#60#; -- Add 0x60 to offset meta to make it match the real meta offset

				-- Set index to Meta Offset
				Set_Index (BF, Stream_IO.Positive_Count (BH.Offset_Meta + 1));

				-- Read meta headers
				declare
					MA : Meta_Array (1 .. Natural (BH.Num_Metas));
				begin
					-- Manually read in each Meta Header since Offset Entry must be adjusted
					for M in MA'Range loop
						if Size (BF) - Index (BF) < (Meta_Header'Size / 8) - 1 then
							-- Put_Line (Standard_Error, "[Error] Prematurely stopped reading metas.");	
							MA (M).Offset_Entry := 0;
							MA (M).Num_Entries := 0;
						else
							Meta_Header'Read (BS, MA (M));
							MA (M).Offset_Entry := Unsigned_64 (BH.Offset_Meta) + MA (M).Offset_Entry + Unsigned_64 ((M - 1) * 16#10#);
							-- Note: This relies on overflowing an unsigned 64-bit integer!
						end if;
					end loop;

					-- Read Entry headers (potentially multiple for each Meta Header)
					declare
						EA : Entries_Array (1 .. Natural (BH.Num_Metas));
						EH : Entry_Header;
						I : Positive := HA'First; -- Used as String Hash Index (Index to HA)
					begin
						for M in EA'Range loop
							Set_Index (BF, Stream_IO.Positive_Count (MA (M).Offset_Entry + 1));
							for E in 1 .. MA (M).Num_Entries loop
								if Size (BF) - Index (BF) < (Entry_Header'Size / 8) - 1 then
									-- Put_Line (Standard_Error, "[Error] Prematurely stopped reading entries.");
									EH.Offset_String := 0;
									EH.Read_Length := 0;
								else
									Entry_Header'Read (BS, EH);
									EH.Offset_String := EH.Offset_String + Unsigned_32 (Index (BF)) - 25;
									Entry_Lists.Append (EA (M), EH);
								end if;
							end loop;
						end loop;

						-- Print strings
						for M in EA'Range loop
							for E of EA (M) loop
								if E.Read_Length > 0 then
									Set_Index (BF, Stream_IO.Positive_Count (E.Offset_String + 1));
									case E.Decode_Mode is
										when Decode_UTF_8 => -- UTF-8 string with cipher applied
											declare
												S : String (1 .. Natural (E.Read_Length));
											begin
												String'Read (BS, S);
												Put_Line (String_Hash'Image (HA (I)) & ": " & Decipher (S, E.Obfuscator));
											exception
												when Constraint_Error => Put_Line ("ERR: " & Decipher (S, E.Obfuscator));
											end;
										when Decode_UTF_16LE => -- Ordinary UTF-16LE string
											declare
												WS : UTF_16_Wide_String (1 .. Natural (E.Read_Length));
											begin
												UTF_16_Wide_String'Read (BS, WS);
												Put_Line (String_Hash'Image (HA (I)) & ": " & Encode (Decode (WS)));
												-- Note: GNAT currently has broken UTF-16 to UTF-8 conversion, so we first decode to UTF-32 then re-encode.
											exception
												when Constraint_Error => Put_Line ("ERR: " & Encode (Decode (WS)));
											end;
										when others =>
											begin
												Put_Line (Standard_Error, String_Hash'Image (HA (I)) & ": [Decode Error]");
											exception
												when Constraint_Error => Put_Line (Standard_Error, "ERR: [Decode Error]");
											end;
									end case;
								end if;
							end loop;
								I := I + 1; -- Increment String Hash Index
						end loop;
						Close (BF);
					end;
				end;
			end;
		end loop;
		End_Search (SE);
	end;
end String_Tool;
