with Interfaces; use Interfaces;

package Common is
	-- Buffer Type
	type Discard_Array is array (Natural range <>) of Unsigned_8;

	-- Mode Type
	type Mode_Type is (d1, d2, d2s17);
end Common;
