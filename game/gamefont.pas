unit GameFont;

{$mode objfpc}{$H+}

interface

uses
  CastleFonts;

var
  ButtonFont: TTextureFont;

procedure LoadFont;

implementation

procedure LoadFont;
begin
  ButtonFont := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 80, true);
end;

end.

