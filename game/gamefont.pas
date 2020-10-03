unit GameFont;

{$mode objfpc}{$H+}

interface

uses SysUtils,
  CastleFonts;

var
  ButtonFont: TTextureFont;

procedure LoadFont;

implementation

procedure LoadFont;
begin
  ButtonFont := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 60, true);
end;

finalization
  FreeAndNil(ButtonFont);
end.

