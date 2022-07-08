package body Ref_Files is
	-- Private Subprograms
	-- Return Package ID given Hash. See the specification for more information.
	function Package_ID (Hash : Package_Entry_Hash) return Unsigned_16 is
		ID : constant Unsigned_16 :=
			Unsigned_16 (Shift_Right (Hash, 16#D#) and 16#FFF#);
	begin
--		Put_Line ("[Debug] Hash ID was " & Package_Entry_Hash'Image (Hash));
--		Put_Line ("[Debug] Package portion was " & Unsigned_16'Image (ID));

		if (ID and 16#800#) > 0 and (ID and 16#400#) > 0 then
			-- If bit 12 and 11 set, value is encoded with bit 11 unset
			return ID and 16#BFF#;
		elsif (ID and 16#400#) = 0 then
			-- If bit 11 unset, set bit 11 and unset bit 10
			return (ID and 16#3FF#) or 16#400#;
		elsif (ID and 16#200#) = 0 then
			-- If bit 11 set and bit 10 unset, value is encoded with both unset
			return ID and 16#1FF#;
		elsif (ID and 16#400#) > 0 then
			-- If bit 11 set and bit 10 set, value is encoded with bit 11 unset
			return ID and 16#3FF#;
		else
			raise Invalid_Package_Exception with
				"Unknown package encoding configuration.";
		end if;
	end Package_ID;

	-- Return Entry ID given Hash
	function Entry_ID (Hash : Package_Entry_Hash) return Unsigned_16 is
		(Unsigned_16 (Hash and 16#1FFF#));

	-- Print Unsigned_16 as big endian hex string
	function Hex_String (Num : Unsigned_16) return String is
		-- Hex Digit Array
		Hex_Digits : constant array (0 .. 15) of Character :=
			('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
			'a', 'b', 'c', 'd', 'e', 'f');
	begin
		return	[Hex_Digits (Natural (Shift_Right (Num and 16#f000#, 12))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f00#, 8))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f0#, 4))),
			Hex_Digits (Natural (Num and 16#f#))];
	end Hex_String;

	-- Public Subprograms
	-- Return the name of the Bank with strings for a given Language
	function Language_Bank (
		Ref_Header : Ref_Header_Type;
		Language : Language_Type)
	return String
	is
		Chosen_Hash : constant Package_Entry_Hash := (case Language is
				when English => Ref_Header.English_Hash,
				when Japanese => Ref_Header.Japanese_Hash,
				when German => Ref_Header.German_Hash,
				when French => Ref_Header.French_Hash,
				when Spanish_LA => Ref_Header.Spanish_LA_Hash,
				when Spanish_ES => Ref_Header.Spanish_ES_Hash,
				when Italian => Ref_Header.Italian_Hash,
				when Portuguese => Ref_Header.Portuguese_Hash,
				when Polish => Ref_Header.Polish_Hash,
				when Russian => Ref_Header.Russian_Hash,
				when Korean => Ref_Header.Korean_Hash,
				when Chinese_Traditional => Ref_Header.Chinese_Traditional_Hash,
				when Chinese_Simplified => Ref_Header.Chinese_Simplified_Hash
			);
	begin
		return Hex_String (Package_ID (Chosen_Hash))
			& "-" & Hex_String (Entry_ID (Chosen_Hash));
	end Language_Bank;

	-- Version-agnostic 'Read implementation for Ref_Header_Type
	procedure Read_Ref_Header_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Ref_Header_Type)
	is
		-- Types
		type Raw_Ref_Header_Type (Mode : Mode_Type) is record
			File_Length : Unsigned_32; -- 0 .. 3
				-- TODO: Determine if 64 bit value (check big endian files)
			Discard_1 : Discard_Array (4 .. 16#17#);
			English_Hash : Package_Entry_Hash; -- 18 .. 1B
			Japanese_Hash : Package_Entry_Hash; -- 1C .. 1F
			German_Hash : Package_Entry_Hash; -- 20 .. 23
			French_Hash : Package_Entry_Hash; -- 24 .. 27
			Spanish_LA_Hash : Package_Entry_Hash; -- 28 .. 2B
			Spanish_ES_Hash : Package_Entry_Hash; -- 2C .. 2F, region-locked in D1
			Italian_Hash : Package_Entry_Hash; -- 30 .. 33
			Korean_Hash : Package_Entry_Hash; -- 34 .. 37, region-locked in D1
			Chinese_Traditional_Hash : Package_Entry_Hash; -- 38 .. 3B,
				-- region-locked in D1
			Chinese_Simplified_Hash : Package_Entry_Hash; -- 3C .. 3F,
				-- region-locked in D1
			Portuguese_Hash : Package_Entry_Hash; -- 40 .. 43
			Polish_Hash : Package_Entry_Hash; -- 44 .. 47, region-locked in D1
			Russian_Hash : Package_Entry_Hash; -- 48 .. 4B,
				-- does not actually exist in D1

			case Mode is
				when d1 =>
					Discard_2_D1 : Discard_Array (16#4C# .. 16#4F#);
					Num_Hashes_D1 : Unsigned_32; -- 50 .. 53
					Discard_3_D1 : Discard_Array (16#54# .. 16#5F#);
				when d2 | d2s17 =>
					Discard_2_D2 : Discard_Array (16#4C# .. 16#5F#);
					Num_Hashes_D2 : Unsigned_32; -- 60 .. 63
					Discard_3_D2 : Discard_Array (16#64# .. 16#6F#);

			end case;
		end record;

		-- Variables
		RRH : Raw_Ref_Header_Type (Item.Mode);
	begin
		Raw_Ref_Header_Type'Read (Stream, RRH);
		Item := (RRH.Mode,
			RRH.File_Length,
			RRH.English_Hash,
			RRH.Japanese_Hash,
			RRH.German_Hash,
			RRH.French_Hash,
			RRH.Spanish_LA_Hash,
			RRH.Spanish_ES_Hash,
			RRH.Italian_Hash,
			RRH.Korean_Hash,
			RRH.Chinese_Traditional_Hash,
			RRH.Chinese_Simplified_Hash,
			RRH.Portuguese_Hash,
			RRH.Polish_Hash,
			RRH.Russian_Hash,
			Num_Hashes => (case RRH.Mode is
				when d1 => RRH.Num_Hashes_D1,
				when d2 | d2s17 => RRH.Num_Hashes_D2));
	end Read_Ref_Header_Type;

end Ref_Files;
