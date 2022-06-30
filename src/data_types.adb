package body Data_Types is
	-- Version-agnostic procedure to read a bank header
	-- Supports definition as the Read attribute
	procedure Read_Bank_Header (S : not null access Root_Stream_Type'Class;
		BH : out Bank_Header)
	is
		-- Raw Type for Bank Header : May be read directly from Stream
		type Raw_Bank_Header (Mode : Mode_Type) is record
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
		RBH : Raw_Bank_Header (BH.Mode);
	begin
		Raw_Bank_Header'Read (S, RBH);
		case BH.Mode is
			when d1 | d2 =>
				BH := (BH.Mode,
					RBH.File_Length,
					RBH.Num_Metas,
					RBH.Num_Strings_D1D2,
					RBH.Offset_Meta_D1D2 + 16#60#); -- Real start is 0x60 later
			when d2s17 =>
				BH := (BH.Mode,
					RBH.File_Length,
					RBH.Num_Metas,
					RBH.Num_Strings_D2S17,
					RBH.Offset_Meta_D2S17 + 16#50#); -- Real start is 0x50 later
		end case;
	end Read_Bank_Header;
end Data_Types;
