with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Ref_Files; use Ref_Files;
with Bank_Files; use Bank_Files;

package String_Display is
	-- Subprograms
	procedure Print_Strings (
		Entries : Entry_Array;
		Hashes : Hash_Array;
		Bank_File : File_Type;
		Bank_Stream : Stream_Access);
end String_Display;
