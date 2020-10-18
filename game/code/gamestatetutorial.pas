unit GameStateTutorial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls, CastleKeysMouse;

type
  TStateTutorial = class(TUiState)
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateTutorial: TStateTutorial;

implementation
uses
  CastleComponentSerialize,
  CastleSoundEngine,
  GameStateGame;

procedure TStateTutorial.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Tutorial.castle-user-interface', FreeAtStop, UiOwner);
end;

function TStateTutorial.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  SoundEngine.Sound(SoundEngine.SoundFromName('start_game'));
  TUiState.Current := StateGame;
end;

end.

