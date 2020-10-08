unit GameStateOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls;

type
  TStateOptions = class(TUiState)
  private
    procedure ClickBack(Sender: TObject);
    procedure ClickVolume0(Sender: TObject);
    procedure ClickVolume1(Sender: TObject);
    procedure ClickVolume2(Sender: TObject);
    procedure ClickVolume3(Sender: TObject);
    procedure ClickVolume4(Sender: TObject);
    procedure ClickMusic0(Sender: TObject);
    procedure ClickMusic1(Sender: TObject);
    procedure ClickMusic2(Sender: TObject);
    procedure ClickMusic3(Sender: TObject);
    procedure ClickMusic4(Sender: TObject);
    procedure ClickVibrationOff(Sender: TObject);
    procedure ClickVibrationOn(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateOptions: TStateOptions;

implementation
uses
  CastleComponentSerialize,
  CastleSoundEngine,
  GameFont, GameStateMainMenu;

procedure TStateMainMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Options.castle-user-interface', FreeAtStop, UiOwner);
end;

end.

