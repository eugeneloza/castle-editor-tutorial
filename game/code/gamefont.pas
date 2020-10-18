unit GameFont;

{$mode objfpc}{$H+}

interface

uses SysUtils,
  CastleFonts;

var
  CartoonFont60: TTextureFont;
  CartoonFont30: TTextureFont;

procedure LoadFonts;

implementation

procedure LoadFonts;
begin
  CartoonFont60 := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 60, true);
  CartoonFont30 := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 30, true);
end;

finalization
  FreeAndNil(CartoonFont60);
  FreeAndNil(CartoonFont30);
end.

