Destiny Strings Extractor
=========================

This is a Destiny String Extractor written in Ada.  
It pairs best with the Destiny Unpacker located at  
https://www.github.com/andrewathalye/destiny-unpacker-linux  

Please let me know if you find any bugs. You can use it as follows:  
`./stringtool [LANGUAGE] PATH_TO_STR_SUBDIRECTORY 2>/dev/null > output_dump.txt`
Valid language values are:
	English, Japanese, German, French, Spanish_LA, Spanish_ES, Italian, Portuguese, Polish, Russian, Symbols  
If Language is omitted, English will be chosen.  

Known Issues
------------
French and Japanese sometimes experience decode errors, where the file fails to decode due to invalid UTF-8.  
I do not know what the cause of it is, but Ginsor's Tool does not handle it any better (in fact it outputs invalid UTF-8).  

I am not a native Spanish speaker, so I can't tell whether I have labelled Spanish_LA and Spanish_ES correctly. If you can  
correct me, please do let me know whether I got it wrong or right.  
