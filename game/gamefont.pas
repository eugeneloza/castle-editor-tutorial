unit GameFont;

{$mode objfpc}{$H+}

interface

uses SysUtils,
  CastleFonts;

var
  CartoonFont60: TTextureFont;

procedure LoadFonts;

implementation

procedure LoadFonts;
begin
  CartoonFont60 := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 60, true);
end;

finalization
  FreeAndNil(CartoonFont60);
end.

