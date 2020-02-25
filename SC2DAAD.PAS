PROGRAM SC2DRG;

{$MODE OBJFPC}

USES sysutils;

CONST TWO_KILOBYTES = 2048;
	  VERSION = '0.8';


TYPE cpcHeaderType =  ARRAY[0..127] of Byte;

PROCEDURE Syntax();
VAR  BinaryName : String;
BEGIN
	BinaryName := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
	Writeln();
	Writeln(BinaryName,' ',VERSION);
	Writeln();
	WriteLn('SYNTAX: ',BinaryName,' <target> <inputFilename> <outputFilename> <lines> [transparency]');
	Writeln();
	Writeln('Target may be c64, cp4, cpc, msx, zx or next. CP4 stands for Commodore Plus/4 although it''s obly used for multicolor images, for HiRes it uses the C64 images');
	Writeln();
	Writeln('Transparency could only be used in ZX Spectrum Next, value it''s a 0-255 value representing a color in RRRGGGBB format, or ''auto''. If value is 0 there will be no transparency so pure black cannot be used as transparent color. ');
	Writeln();
	WriteLn('Type "',BinaryName,' <target> HELP" to get specific help per each platform.');
	Writeln();
	Halt(1);
END;


PROCEDURE Help(target: string);
BEGIN
    IF uppercase(target)='CPC'	THEN
    BEGIN
		WriteLn('For CPC you will requiere a CPC .SCR file together with a .PAL file for the palette, both with same base name. You can obtain such files using cpcConvImg application or any other application who can provide such format (http://www.cpcwiki.eu/index.php/Format:Advanced_OCP_Art_Studio_File_Formats)');
		Writeln();
		WriteLn('You should specify the number of lines from top of screen you want to include in the image. Number of lines should be a multiple of 8 and less or equal to 96.');
		Writeln();
		WriteLn('The file created would already have a CPC header. The output file should have .CPC extension, and have three digits for the number, i.e.: 001.CPC, 012.CPC, 118.CPC');
		Writeln();
		WriteLn('Note: although max line number is 96, having a 200 lines picture (full screen) is allowed.');
		Halt(0);
	END;
    IF uppercase(target)='ZX'	THEN
    BEGIN
		WriteLn('For Spectrum you will requiere a Spectrum .SCR file (6912 bytes), a dump of Spectrum screen area.');
		Writeln();
		WriteLn('You should specify the number of lines from top you want to include in the image. Number of lines should be a multiple of 8.');
		Writeln();
		WriteLn('The output file should have .ZXS extension, and have three digits for the number, i.e.: 001.ZXS, 012.ZXS, 118.ZXS');
		Writeln();
		Halt(0);
	END;
    IF uppercase(target)='NEXT'	THEN
    BEGIN
		WriteLn('For Spectrum Next you will requiere a .PCX file with 8 bit color depth (256 colors) and 256x192 resolution, no matter if your image is smaller.');
		Writeln();
		WriteLn('You should specify the number of lines from top you want to include in the image.');
		Writeln();
		WriteLn('The output file should have .NXT extension, and have three digits for the number, i.e.: 001.NXT, 012.NXT, 118.NXT');
		Writeln();
		WriteLn('You can set transparency to any value from 0 to 255, representing a RGB value in RRRGGGBB (bitwise) format, in which case pixels matching that color will be transparent. If value is 0 then there will be no transparency, so pure black cannot be the transparent color. Also, if you set transparency to "auto", the most used color in the image will be the transparent one');
		Writeln();
		WriteLn('Please notice you can create a 256 color file using GIMP, make sure you use an indexed palette, 256 colors. Also take in mind Spectrum Next palette has a palette of 512 colors, so the 256 colors you file has will be addapted to the 256 closer colors from that palette.');
		Halt(0);
	END;
    IF uppercase(target)='MSX'	THEN
    BEGIN
		WriteLn('For MSX you will require a MSX .SC2 file (14343 bytes), a dump of MSX mode 2 screen area.');
		Writeln();
		WriteLn('You should specify the number of lines from top you want to include in the image. Number of lines should be a multiple of 8.');
		Writeln();
		WriteLn('The output file should have .MS2 (MSX mode 2) extension, and have three digits for the number, i.e.: 001.MS2, 012.MS2, 118.MS2');
		Writeln();
		Halt(0);
	END;
    IF uppercase(target)='C64'	THEN
    BEGIN
		WriteLn('For C64 you will require an Art Studio .ART file if you will be using the DAADs standard mode (HiRes) or a .KOA/.KLA file if you will use split screen and Multicolor Images. SC2DAAD will determine the file format by the file extension.');
		Writeln();
		WriteLn('You should specify the number of lines from top you want to include in the image. Number of lines should be a multiple of 8.');
		Writeln();
		WriteLn('The output file should have .64 extension, and have three digits for the number, i.e.: 001.64, 012.64, 118.64');
		Writeln();
		Halt(0);
	END;
    IF uppercase(target)='CP4'	THEN
    BEGIN
		WriteLn('For Commodore Plus/4 you will require a Botticceli HiRes PRG file. There is no support for Multicolor images.');
		Writeln();
		WriteLn('You should specify the number of lines from top you want to include in the image. Number of lines should be a multiple of 8.');
		Writeln();
		WriteLn('The output file should have .4 extension, and have three digits for the number, i.e.: 001.4, 012.4, 118.4');
		Writeln();
		Halt(0);
	END;
	Writeln('Unknown target.');
	Syntax();
END;	


PROCEDURE Error(S:String);
BEGIN
	Writeln(S + '.');
	Writeln();
	Halt(1);
END;



PROCEDURE WriteCPCHeaderWord(var header: cpcHeaderType;index: Byte; val: Word);
BEGIN
  header[index] := val MOD 256;
  header[index+1] := val DIV 256;
END;

PROCEDURE CopyBlock(VAR inFile, outFile:FILE; size:word);
VAR buffer: ARRAY [0..$8000] OF byte;
	Result : Word;
	i :word;
BEGIN
	Blockread(inFile, Buffer, size, Result);
	if Result<Size THEN 
	  FOR i := Result TO Size-1 DO
	    buffer[i]:=0;
	BlockWrite(outFile, Buffer, size)
END;


FUNCTION GetCPCFirmwarePalette(HwValue:Byte):Byte;
BEGIN
	CASE HwValue OF
	 $54: Result:= 0;
	 $44: Result:= 1;
	 $55: Result:= 2;
	 $5C: Result:= 3;
	 $58: Result:= 4;
	 $5D: Result:= 5;
	 $4C: Result:= 6;
	 $45: Result:= 7;
	 $4D: Result:= 8;
	 $56: Result:= 9;
	 $46: Result:= 10;
	 $57: Result:= 11;
	 $5E: Result:= 12;
	 $40: Result:= 13;
	 $5F: Result:= 14;
	 $4E: Result:= 15;
	 $47: Result:= 16;
	 $4F: Result:= 17;
	 $52: Result:= 18;
	 $42: Result:= 19;
	 $53: Result:= 20;
	 $5A: Result:= 21;
	 $59: Result:= 22;
	 $5B: Result:= 23;
	 $4A: Result:= 24;
	 $43: Result:= 25;
	 $4B: Result:= 26;
	 $41: Result:= 13;
	 $48: Result:= 7;
	 $49: Result:= 24;
	 $50: Result:= 1;
	 $51: Result:= 19;
	ELSE
	 Result := 0;
	END; 
END;

PROCEDURE MSX(inputFilename, outputFilename: string; lines:word);
(* SC2 format includes a 7 bytes header, followed by  6144 bytes for pixels and 6144 for attributes. Length must be 14343 bytes *)
VAR	inFile, outFile: FILE;
var linesByte  : byte;
BEGIN
	AssignFile(inFile, inputFilename);
	AssignFile(outFile, outputFilename);
	TRY
	Reset(inFile, 1);
	EXCEPT
		on E: Exception DO Error('File not found:' + E.Message);
	END; 
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Can''t open file for output:' + E.message);
	END; 
	linesByte := lines AND $FF;
	BlockWrite(outFile, linesByte, 1);
	if (FileSize(inFile)<>14343) THEN Error('Invalid input file, size must be 14343 bytes.');
	Seek(inFile, 7); // Go to pixels position
	CopyBlock(inFile, outFile, lines * 32);
	Seek(inFile, $2007); // Go to attributes position
	CopyBlock(inFile, outFile, lines * 32);
	CloseFile(inFile);
	CloseFile(outFile);
END;	

PROCEDURE CPC(inputFilename, outputFilename: string; lines:word);
VAR i, bytesToRead, current2KBuffercount: word;
	palFileName:String;
	inFile, outFile: FILE;
	TempBuffer: ARRAY [0..2047] OF byte;
	Palette : ARRAY[0..15] OF byte;
	cpcHeader : cpcHeaderType;
	AuxStr: String;
	CheckSum: Word;
	ScansPer2kBuffer: Byte;
	FileSize : Word;
	Readed: Longint;
	Offset: Word;
	(* 
	 CPC FILE FORMAT IS (after the AMSDOS header):
	 00    : scans per 2k buffer, 
	 01-16 : palette used, first one is for background, second one for text ink
	 17-18  :number of lines multiplied by 10, that is, number of bytes per scanline
	 19-   : data as in a SCR file, but with less lines - or not.

	 Exception: 200 lines graphic contains 16384 bytes, 16384 for data, and 16 for palette, but the palette is over the last 16 bytes of data, where
	 the spare 48 bytes are. 16384 in CPC header and use CAS_IN_DIRECT to load directly to VRAM, then set the palette (better set all palette to black first
	 to avoid strange colours on screen)
	 *)
	
BEGIN
	palFileName := changeFileExt(inputFilename,'.pal');
	AssignFile(inFile, palFileName);  
	TRY
		Reset(inFile, 1);
		Offset := 128 + 3; // First palette entry it's at byte 3 after the 128 byte AMSDOS header
		for i:=0  TO 15 DO
		BEGIN
		 Seek(inFile, Offset);
		 Blockread(inFile, Palette[i], 1);
		 Offset := Offset  + 12;  // one palette value very 12 bytes
		END;
	EXCEPT
		on E: Exception DO Error('PAL file not found or not valid, the .pal file for the image should be located at same folder than SCR file. If your OS is case sensitive, make sure the PAL file has the extension in lowercase.' + E.Message);
	END; 
	CloseFile(inFile);

	AssignFile(inFile, inputFilename);
	TRY
		Reset(inFile, 1);
		Seek(inFile, 128); // Skip AMSDOS header
	EXCEPT
	 	on E: Exception DO Error('Invalid CPC .SCR file:' + E.message);
	END; 

	AssignFile(outFile, outputFilename);
	TRY
		Rewrite(outFile, 1);
	EXCEPT
		 on E: Exception DO Error('Can''t open file for output:' + E.message);
	END; 
	// Prepare Header
	IF Lines=200 THEN FileSize := 16384
				 ELSE FileSize := 7 + 2048 * (8 DIV (2048 DIV (lines * 10)));

	FillChar(cpcHeader, 128, 0);
	AUXSTR:='UTO DEV HCK';
	Move(AuxStr[1],cpcHeader[1],11);
	cpcHeader[18]:=2; //File type binary
	WriteCPCHeaderWord(cpcHeader,24, FileSize);
	WriteCPCHeaderWord(cpcHeader,64, FileSize);
	Checksum := 0;
	for i:=0 to 66 do Checksum := Checksum + cpcHeader[i];
	WriteCPCHeaderWord(cpcHeader,67, Checksum);


	//Save header
	BlockWrite(outFile, cpcHeader, 128);		

	FOR i:= 0 to 15 do Palette[i]:=GetCPCFirmwarePalette(Palette[i]);

	IF lines=200 THEN //Full screen image, just save all 16.000 bytes plus palette
	BEGIN
		FOR I:=1 to 8 DO CopyBlock(inFile, outFile, 2048);
		Seek(outFile, 16384 - 16 + 128);
		BlockWrite(outFile, Palette, 16);
	END
	ELSE
	BEGIN             // Partial file, more tricky
		BytesToRead := lines * 10;
		ScansPer2kBuffer := TWO_KILOBYTES DIV BytesToRead;
		// Save number of lines and palette
		BlockWrite(outFile, ScansPer2kBuffer, 1);
		BlockWrite(outFile, Palette, 16);
		BlockWrite(outFile, BytesToRead, 2);		
		current2KBuffercount := 19;
	    FOR i:= 1 TO 8 DO
		BEGIN
		 	Blockread(inFile, TempBuffer, TWO_KILOBYTES, Readed);
		 	BlockWrite(outFile, TempBuffer, BytesToRead);
		 	current2KBuffercount := current2KBuffercount + bytesToRead;
		 	IF (current2KBuffercount+bytesToRead > TWO_KILOBYTES) THEN
		 	BEGIN
		 		Fillchar(TempBuffer, TWO_KILOBYTES - current2KBuffercount, 0);
		 		BlockWrite(outFile, TempBuffer, TWO_KILOBYTES - current2KBuffercount);
		 		current2KBuffercount := 0;
		 	END;
		END;
	END;
	CloseFile(inFile);
	CloseFile(outFile);
END;

PROCEDURE NEXT(inputFilename, outputFilename: string; lines:byte; transparency : byte; Auto: boolean);
	(* 
	 NXT FILE FORMAT IS:
	 00     : transparency color, if 0 no transparency is applied
	 01     : number of lines
	 02-257 : palette values
	 257-   : pixel information, 256 bytes per line
	*)
	// auto=true means ignore the transparency color, and use the one that repeats the most, unless it's 0 which will raise an error
	// Hey! I've borrowed the PCX decoding code from PCX2TWK, an utility I made in 1995 as part of my text adventure game engine NMP. So thanks to the 24 years younger myself for that work :-D
VAR	inFile, outFile: FILE;
	r,g,b,rgb,i,a : byte;
	j: word;
	width, height : word;
	PCXHeader: ARRAY[0..127] OF Byte;
	Palette : ARRAY[0..767] OF Byte;
	Palette8B : ARRAY[0..255] OF Byte;
	Buffer : ARRAY[0..255] OF Byte;
	Used : ARRAY[0..255] OF Longint;
	Max: Word;
	maxIndex : Byte;


BEGIN
	AssignFile(inFile, inputFilename);
	TRY
	Reset(inFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid PCX file:' + E.message);
	END; 
	AssignFile(outFile, outputFilename);
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Can''t open file for output:' + E.message);
	END;
	BlockRead(inFile, PCXHeader, 128);
	IF (PCXHeader[0]<>10) THEN Error('Invalid PCX file: bad signature');
	IF (PCXHeader[2]<>1) THEN Error('Invalid PCX file: no RLE');
	IF (PCXHeader[3]<>8) THEN Error('Invalid PCX file: not a 256 colors image');
	width :=PCXHeader[8] + 256*PCXHeader[9] - PCXHeader[4] + 256*PCXHeader[5]+1;
 	height :=PCXHeader[10] + 256*PCXHeader[11] - PCXHeader[6] + 256*PCXHeader[7]+1;
	IF (width<>256) or (height<192) THEN Error('Invalid resolution. Input should be PCX file with 256 as width, and at least 192 as height.');
	BlockWrite(outFile, transparency, 1); // transparency color
	BlockWrite(outFile, lines, 1); // Number of lines
	// Process the palette
	Seek(inFile, filesize(inFile)-768);
	BlockRead(inFile, Palette, 768);
	FOR i := 0 to 255 DO
	BEGIN
		R:= Palette[i*3];
		G:= Palette[i*3+1];
 		B:= Palette[i*3+2];
		R:= round(R * 7 / 255);
		G:= round(G * 7 / 255);
		B:= round(B * 3 / 255);
		RGB := (R SHL 5) + (G SHL 2) + B;
		BlockWrite(outFile, RGB, 1);
		Palette8B[i] := RGB;
	END;
	// Process the pixels
	FOR  i:= 0 TO 255 DO Used[i] := 0;
    Seek(inFile, 128); // Back to pixels data
	FOR  i := 0 to lines -1 DO
	BEGIN
		// Get one line
		j := 0;
		REPEAT
			IF EOF(inFile) THEN Error('Invalid PCX file');
			BlockRead(inFile,a,1);
			IF a>=192 THEN 	BEGIN
								b:=a-192;
								IF EOF(inFile) THEN Error('Invalid PCX file');
								BlockRead(inFile,a,1);
							END 
							ELSE b:=1;
			FillChar(Buffer[j],b,a);
			Used[a] := Used[a] + b;
			j := j + b;
		UNTIL j=width;
		// Save the line
		BlockWrite(outFile, Buffer, 256);
	END;
	IF Auto THEN
	BEGIN
		MaxIndex := 0;
		Max := 0;
		FOR I :=0 to 255 DO 
		BEGIN
		IF Used[i]>Max THEN
		BEGIN
		 Max := Used[i];
		 MaxIndex := i;
		END; 
		END;
		RGB := Palette8B[MaxIndex];
		WriteLn('Most used color is ', MaxIndex,' which assigned RGB is ', RGB);
		if (RGB=0) THEN Writeln('Warning: auto transparency found most used color is RGB (0,0,0) which will not apply transparency');
		Seek(outFile, 0);
		BlockWrite(outFile, RGB, 1); // Replace the transparent color with the one we found
	END;
	CloseFile(inFile);
	CloseFile(outFile);
END;


PROCEDURE ZX(inputFilename, outputFilename: string; lines:byte);
VAR i,rows, bytesToRead, bytesToSkip, attributesSize: word;
	inFile, outFile: FILE;
	TempBuffer: ARRAY [0..255] OF byte;
	(* 
	 ZX FILE FORMAT IS:
	 00    : number of lines
	 01-   : data as in a SCR file, but with less lines - or not - for both pixels and attributes.
	*)
BEGIN
	attributesSize := (lines DIV 8) * 32;
	AssignFile(inFile, inputFilename);
	TRY
	Reset(inFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid ZX Spectrum SCR file:' + E.message);
	END; 
	if (FileSize(inFile)<>6912) THEN Error('Invalid file. Size must be 6912 bytes');
	AssignFile(outFile, outputFilename);
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Can''t open file for output:' + E.message);
	END; 

	BlockWrite(outFile, lines, 1);

	IF lines >= 64 THEN
	BEGIN
	 CopyBlock(inFile, outFile, TWO_KILOBYTES); // First third
	 lines := lines - 64;
	END;
	IF lines >= 64 THEN
	BEGIN
	 CopyBlock(inFile, outFile, TWO_KILOBYTES); // Second third
	 lines := lines - 64;
	END;
	IF lines >= 64 THEN
	BEGIN
	 CopyBlock(inFile, outFile, TWO_KILOBYTES); // Third third
	 lines := lines - 64;
	END;

	IF lines<>0 THEN
	BEGIN
	 rows := lines DIV 8;
	 bytesToRead := rows * 32;
	 bytesToSkip := (8 - rows) * 32;
	 FOR i:= 1 TO 8 DO
	 BEGIN
	 	CopyBlock(inFile, outFile, bytesToRead);
	 	Blockread(inFile, TempBuffer, bytesToSkip);
	 END;
	END;
	
	Seek(inFile, TWO_KILOBYTES*3); //Move to attributes area


	CopyBlock(inFile, outFile, attributesSize);

	CloseFile(inFile);
	CloseFile(outFile);
END;


PROCEDURE WriteRLECompresssion(var outFile: FILE; Buffer: Pointer ;Size: Word);
VAR Counter : ARRAY [0..255] of Word;
	i,j : Word;
	MinValue: Word;
	MinValueIndex : Byte;
	PreviousValue, Value : Byte;
	PreviousValueCount : Byte;
BEGIN
	// Zero counters
	FOR i := 0 to 255 DO Counter[i]:=0;

    // Count how much times each byte value is repeated in the buffer
	FOR i := 0 to Size - 1 DO
	  Counter[Byte((Buffer+i)^)] := Counter[Byte((Buffer+i)^)] + 1;

	 // Determine which is the value repeated the less. We will use that value as mark of compression, so if a value is repeated 6 times, we will
	 // save <mark of compression> 6 <value>. This also means if mark of compression value appears it will allways have to be written as compressed data,
	 // even if it only appears once. 
	 MinValue := 65535;
	 MinValueIndex := 0;
	 FOR i := 0 to 255 DO
	  IF Counter[i]<MinValue THEN
	  BEGIN
	   MinValue := Counter[i];
	   MinValueIndex := i;
	  END;


	BlockWrite(outFile, MinValueIndex, 1); // Set the "repeat" prefix
	// Ok, let's compress now
	PreviousValue := Byte(Buffer^);
	PreviousValueCount := 1;
	i := 1;
	WHILE i<=Size DO
	BEGIN
	// In the normal cycle we will go for the next value, in the last cycle we have already read all data so we enter a fake value different than previous one 
	// to make sure the remaining data is dumped to disk after that.
	if (i<>Size) THEN Value := Byte((Buffer+i)^)  ELSE  Value := (PreviousValueCount+1) MOD 256;

	 IF ((Value<>PreviousValue) OR (PreviousValueCount = 255))  THEN // Si cambia de valor o ya llevamos 255 repeticiones
	 BEGIN
	   IF (PreviousValueCount>3) OR (PreviousValue=MinValueIndex) THEN // if more than 3 times repeated, or previous value is mark of compression, let's compress
	   BEGIN
	    BlockWrite(outFile, MinValueIndex, 1); // Mark of compression
	    BlockWrite(outFile, PreviousValueCount, 1); // Times the value appears
	    BlockWrite(outFile, PreviousValue, 1); // value
	   END
	   ELSE FOR J:= 1 TO PreviousValueCount DO 
		 BEGIN
		 	BlockWrite(outFile, PreviousValue, 1); // if less than 3 times, we dump the value 1, 2 or 3 times, unless the value is mark of compression
		 END;
	   PreviousValue := Value;
	   PreviousValueCount := 1;
	 END 
	 ELSE PreviousValueCount := PreviousValueCount + 1;

	 i:=i+1;
	END;
	// Finally we dump a mark of compression with zero times repeat, what is a mark of end of data
    BlockWrite(outFile, MinValueIndex, 1); // Mark of compression
    PreviousValueCount := 0;
    BlockWrite(outFile, PreviousValueCount, 1); // Mark of compression
END;



PROCEDURE C64ART(inputFilename, outputFilename: string; lines:byte);
VAR inFile, outFile: FILE;
		Aux : byte;
		DataBuffer : pointer;
	(* 
	 C64 HiRes FILE FORMAT IS:
	 00    : number of lines div 8
	 01	   : byte which is used to show repeats in RLE encoding
	 02-   : RLE encoded compressed bytes for the screen pixels
	 nn-   : byte which is used to show repeats in attributes for RLE encoding
	 nn+1-   : RLE encoded attributes
	*)
BEGIN
	
	AssignFile(inFile, inputFilename);
	TRY
	Reset(inFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid C64 ART file:' + E.message);
	END; 
	if (FileSize(inFile)<>9002) AND (FileSize(inFile)<>9009) THEN Error('Invalid file. File size must be 9002 or 9009 bytes (Art Studio HiRes .ART file).');
	AssignFile(outFile, outputFilename);
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Can''t open file for output:' + E.message);
	END; 
	
	Aux := lines div 8;
	BlockWrite(outFile,Aux,1); // Number of lines divided by 8
	
	GetMem(DataBuffer, lines*40);

	Seek(inFile, 2); //Go bitmap area
	BlockRead(inFile, DataBuffer^, lines* 40);
	WriteRLECompresssion(outFile, DataBuffer, lines * 40);

	Seek(inFile, 8002); // Go to attribytes area
  BlockRead(inFile, DataBuffer^, (lines div 8)*40);
  WriteRLECompresssion(outFile, DataBuffer, (lines div 8)*40);


	CloseFile(inFile);
	CloseFile(outFile);
END;

PROCEDURE C64KOA(inputFilename, outputFilename: string; lines:byte);
VAR 	inFile, outFile: FILE;
		Aux : byte;
		DataBuffer : pointer;
		ExtLines: Byte;
	(* 
	 C64 Multicolor FILE FORMAT IS:
	 00    : always 0  - Marks the file is a Multicolor image
	 01	   ; number of lines divided by 8
	 02	   : byte which is used to show repeats in RLE encoding of BITMAP
	 03-   : RLE encoded compressed bytes for the screen pixels
	 nn-   : byte which is used to show repeats in COLOR for RLE encoding
	 nn+1- : RLE encoded COLOR
	 qq-   : bytes which is used to show repeats in SCREEN for 
	 qq+1- : RLE encoded SCREEN
	 xx    : Background color
	*)

	(* Important note: to avoid glitches in the split scanline, this converter will always generate a picture 8 pixels higher than asked. The last 8 scanlines are filled
	   with 01010101, which makes every pixel have color 01. THen in the color area, that zone is filled with zero, which will make colors 01 and 10 be black. That way
	   we will have a 8 pixels high black ribbon. Then the split are is placed in the middle of that area *)
BEGIN
	AssignFile(inFile, inputFilename);
	TRY
	Reset(inFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid C64 KOA/KLA file:' + E.message);
	END; 
	if FileSize(inFile)<>10003 THEN Error('Invalid file. File size must be 10001 bytes long (Standard .KOA/.KLA).');
	AssignFile(outFile, outputFilename);
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Can''t open file for output:' + E.message);
	END; 

	If Lines<>96 THEN Error('Multicolor images for C64 must be 96 pixels high, as the split area is fixed.');
	
	Aux := 0;
	BlockWrite(outFile,Aux,1); // Mark of Multicolor file
	ExtLines := Lines + 8;
	Aux := Extlines div 8;
	BlockWrite(outFile,Aux,1); // Number of lines divided by 8
	
	GetMem(DataBuffer, ExtLines * 40); 

	Seek(inFile, 2); //Go bitmap area (2 byte C64 header)
	FillChar(DataBuffer^, Extlines * 40, $55); // Fill 8 scanlines more with 01010101
	BlockRead(inFile, DataBuffer^, lines* 40);
	WriteRLECompresssion(outFile, DataBuffer, ExtLines * 40);

	Seek(inFile, 8002); // Go to SCREEN area
	FillChar(DataBuffer^,(ExtLines div 8)*40 , 0); // Make sure all colors are color 0
  	BlockRead(inFile, DataBuffer^, (lines div 8) * 40);
  	WriteRLECompresssion(outFile, DataBuffer, (Extlines div 8) * 40);

	Seek(inFile, 9002); // Go to COLOR area
	FillChar(DataBuffer^,(Extlines div 8)*40 , 0); // Make sure all colors are color 0 too
  	BlockRead(inFile, DataBuffer^, (lines div 8)*40);
  	WriteRLECompresssion(outFile, DataBuffer, (Extlines div 8)*40);
    Seek(inFile, Filesize(InFile)-1); // Go to Background value
	BlockRead(inFile, DataBuffer^,1);
	BlockWrite(outFile, DataBuffer^,1);
	CloseFile(inFile);
	CloseFile(outFile);
END;


PROCEDURE CP4(inputFilename, outputFilename: string; lines:byte);
VAR 	inFile, outFile: FILE;
		Aux : byte;
		DataBuffer : pointer;
	(* 
	 CP4  FILE FORMAT IS:
	 00	   ; number of lines divided by 8
	 01	   : byte which is used to show repeats in RLE encoding of BITMAP
	 02-   : RLE encoded compressed bytes for the color pixels
	 nn-   : byte which is used to show repeats in COLOR for RLE encoding
	 nn+1- : RLE encoded COLOR 
	 qq-   : bytes which is used to show repeats in SCREEN for  (luminance)
	 qq+1- : RLE encoded SCREEN
	 xx+1  : 1 byte Background color
	*)
BEGIN
	AssignFile(inFile, inputFilename);
	TRY
	Reset(inFile, 1);
	EXCEPT
	 on E: Exception DO Error('Invalid PRG  file:' + E.message);
	END; 
	if FileSize(inFile)<>10050 THEN Error('Invalid file. File size must be 10050 bytes long (standard .PRG file generated by Botticelli).');
	AssignFile(outFile, outputFilename);
	TRY
	Rewrite(outFile, 1);
	EXCEPT
	 on E: Exception DO Error('Can''t open file for output:' + E.message);
	END; 

	If Lines>200 THEN Error('Plus/4 images height can''tbe over 200 pixels.');
	If Lines MOD 8 <> 0 THEN Error('Plus/4 images height should be a multiple of 8.');
	
	Aux := Lines div 8;
	BlockWrite(outFile,Aux,1); // Number of lines divided by 8
	
	GetMem(DataBuffer, Lines * 40); 

	Seek(inFile, $802); //Go bitmap area (Skip 2 byte CP4 header and two $400 areas for color and luminance)
	BlockRead(inFile, DataBuffer^, Lines* 40);
	WriteRLECompresssion(outFile, DataBuffer, Lines * 40);


	Seek(inFile, $402); // Go to COLOR area
  	BlockRead(inFile, DataBuffer^, (lines div 8)*40);
  	WriteRLECompresssion(outFile, DataBuffer, (lines div 8)*40);


	Seek(inFile, 2); // Go to LUMINANCE area
  	BlockRead(inFile, DataBuffer^, (Lines div 8) * 40);
  	WriteRLECompresssion(outFile, DataBuffer, (Lines div 8) * 40);


    Seek(inFile, Filesize(InFile)-1); // Go to Background color value 
	BlockRead(inFile, DataBuffer^,1);
	BlockWrite(outFile, DataBuffer^,1);
	CloseFile(inFile);
	CloseFile(outFile);END;	


PROCEDURE C64(inputFilename, outputFilename: string; lines:byte);
VAR ext : AnsiString;
BEGIN
	ext := AnsiUpperCase(ExtractFileExt(inputFilename));
    IF (ext = '.KOA') OR (ext='.KLA') THEN C64KOA(inputFilename, outputFilename, lines)
	ELSE C64ART(inputFilename, outputFilename, lines);
END;	


(* MAIN *)
VAR target, inputFilename, outputFilename: string;
	lines : word;
	transparency : byte;
	AuxString: string;
	AuxVal : Longint;
	Auto : Boolean;

BEGIN
	IF ParamCount()<2 THEN Syntax();
	target := uppercase(ParamStr(1));
	inputFilename := ParamStr(2);

	IF (ParamCount()<4) AND (uppercase(inputFilename)<>'HELP') THEN Syntax();

	outputFilename := ParamStr(3);

	// Check param Errors

    IF (target<>'CPC') AND (target<>'NEXT') AND (target<>'ZX') AND (target<>'MSX') AND (target<>'CP4')  AND (target<>'C64') THEN Error('Invalid target.');

    IF (uppercase(inputFilename) = 'HELP' ) THEN Help(target);	

	AuxString := ParamStr(4);
	TRY
		lines := StrToInt(AuxString);
    EXCEPT
        On E : EConvertError DO Error('Invalid number of lines:' + E.message);
    END;

    IF ((target='ZX') OR (target='MSX')) AND (lines >192) THEN Error('Maximum number of lines for MSX/ZX is 192');
    IF (target='CPC') AND (lines >96) AND (lines <>200) THEN Error('Maximum number of lines for CPC is 96, althought you can choose full screen with 200 lines');
    if (target='C64') AND (lines>200) THEN Error('Maximum number of lines for C64 is 200');
	if (target='CP4') AND (lines>200) THEN Error('Maximum number of lines for C64 is 200');

    IF  ((lines MOD 8) <> 0) THEN Error('Number of lines must be a multiple of 8');

    IF (NOT FileExists(inputFilename)) THEN Error('File not found');

	IF target='NEXT' THEN 
	BEGIN
		transparency := 0;
		IF (ParamCount>4) THEN
		BEGIN
			AuxString := ParamStr(5);
			IF (uppercase(AuxString)='AUTO') THEN 
			BEGIN
				Auto := true;
				Transparency := 0;
			END
			ELSE
			BEGIN	
				TRY
					AuxVal := StrToInt(AuxString);
				EXCEPT
					On E : EConvertError DO Error('Invalid transparency color: ' + E.message);
				END;
				IF (AuxVal<0) OR (AuxVal>255) THEN Error('Invalid transparency color: should be a value in the 0-255 range');
				Transparency := AuxVal AND $FF;
			END;	
		END;
	END;

    // Let's do it
    IF target = 'ZX' THEN ZX(inputFilename, outputFilename, lines) 
    ELSE IF target = 'NEXT' THEN NEXT(inputFilename, outputFilename, lines, transparency, auto) 
    ELSE IF target = 'CPC' THEN CPC(inputFilename, outputFilename, lines) 
    ELSE IF target = 'MSX' THEN MSX(inputFilename, outputFilename, lines)
    ELSE IF target = 'CP4' THEN CP4(inputFilename, outputFilename, lines)
    ELSE IF target = 'C64' THEN C64(inputFilename, outputFilename, lines);

END.

