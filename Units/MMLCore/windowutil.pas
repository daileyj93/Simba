unit windowutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ctypes // for cint, etc
  {$IFDEF LINUX},
  x, xlib, // For X* stuff.
  GraphType // For TRawImage
  {$ENDIF};

         {$IFDEF LINUX}
         Procedure XImageToRawImage(XImg: PXImage; Var RawImage: TRawImage);
         function MufasaXErrorHandler(para1:PDisplay; para2:PXErrorEvent):cint;cdecl;
         {$ENDIF}

implementation

{$IFDEF LINUX}

// Too global.
function MufasaXErrorHandler(para1:PDisplay; para2:PXErrorEvent):cint;cdecl;
begin;
  result := 0;
  Writeln('X Error: ');
  writeln('Error code: ' + inttostr(para2^.error_code));
  writeln('Display: ' + inttostr(LongWord(para2^.display)));
  writeln('Minor code: ' + inttostr(para2^.minor_code));
  writeln('Request code: ' + inttostr(para2^.request_code));
  writeln('Resource ID: ' + inttostr(para2^.resourceid));
  writeln('Serial: ' + inttostr(para2^.serial));
  writeln('Type: ' + inttostr(para2^._type));
end;

Procedure XImageToRawImage(XImg: PXImage; Var RawImage: TRawImage);
Begin
  RawImage.Init; { Calls raw.Description.Init as well }

  RawImage.Description.PaletteColorCount:=0;
  RawImage.Description.MaskBitsPerPixel:=0;
  RawImage.Description.Width := XImg^.width;
  RawImage.Description.Height:= XImg^.height;

  RawImage.Description.Format := ricfRGBA;

  if XImg^.byte_order = LSBFirst then
      RawImage.Description.ByteOrder := riboLSBFirst
  else
      RawImage.Description.ByteOrder:= riboMSBFirst;

  RawImage.Description.BitOrder:= riboBitsInOrder; // should be fine

  RawImage.Description.Depth:=XImg^.depth;

  RawImage.Description.BitsPerPixel:=XImg^.bits_per_pixel;

  RawImage.Description.LineOrder:=riloTopToBottom;

  RawImage.Description.LineEnd := rileDWordBoundary;

  RawImage.Description.RedPrec := 8;
  RawImage.Description.GreenPrec:= 8;
  RawImage.Description.BluePrec:= 8;
  RawImage.Description.AlphaPrec:=0;


  // Can be adjusted to the XImage RedMask, etc.
  // For now I just assume the tester uses BGR.
  RawImage.Description.RedShift:=16;
  RawImage.Description.GreenShift:=8;
  RawImage.Description.BlueShift:=0;

  RawImage.DataSize := RawImage.Description.Width * RawImage.Description.Height
                       * (RawImage.Description.bitsperpixel shr 3);
  //RawImage.DataSize := RawImage.Description.Height * RawImage.Description.BitsPerLine;
  RawImage.Data := PByte(XImg^.data);

End;
{$ENDIF}

end.

